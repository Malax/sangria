package sangria.execution

import language.higherKinds
import sangria.marshalling.InputUnmarshaller

import language.{existentials, implicitConversions}
import sangria.ast
import sangria.schema.{Action, Context, InputType}
import sangria.streaming.SubscriptionStream
import sangria.validation.Violation


trait Middleware[-Ctx] {
  type QueryVal

  def beforeQuery(context: MiddlewareQueryContext[Ctx, _, _]): QueryVal
  def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[Ctx, _, _]): Unit
}

object Middleware {
  def composeFromScalarMiddleware[Ctx](middleware: List[Middleware[Ctx]], userContext: Ctx): Option[(Any, InputType[_]) ⇒ Option[Either[Violation, Any]]] = {
    val relevant =
      middleware.collect {
        case m: MiddlewareFromScalar[Ctx] ⇒ m
      }

    if (relevant.nonEmpty)
      Some((v, tpe) ⇒ {
        var changed = false
        var violation: Violation = null

        val newValue =
          relevant.foldLeft(v) {
            case (acc, _) if violation != null ⇒ acc
            case (acc, m) ⇒ m.fromScalar(acc, tpe, userContext) match {
              case Some(Left(viol)) ⇒
                violation = viol
                acc
              case Some(Right(newAcc)) ⇒
                changed = true
                newAcc
              case None ⇒
                acc
            }
          }

        if (violation != null) Some(Left(violation))
        else if (changed) Some(Right(newValue))
        else None
      })
    else None
  }

  def composeToScalarMiddleware[Ctx](middleware: List[Middleware[Ctx]], userContext: Ctx): Option[(Any, InputType[_]) ⇒ Option[Any]] = {
    val relevant =
      middleware.collect {
        case m: MiddlewareToScalar[Ctx] ⇒ m
      }
    
    if (relevant.nonEmpty)
      Some((v, tpe) ⇒ {
        var changed = false

        val newValue =
          relevant.foldRight(v) {
            case (m, acc) ⇒ m.toScalar(acc, tpe, userContext) match {
              case Some(newAcc) ⇒
                changed = true
                newAcc
              case None ⇒ acc
            }
          }

        if (changed) Some(newValue)
        else None
      })
    else None
  }
}

trait MiddlewareBeforeField[Ctx] extends Middleware[Ctx] {
  type FieldVal

  def beforeField(queryVal: QueryVal, mctx: MiddlewareQueryContext[Ctx, _, _], ctx: Context[Ctx, _]): (FieldVal, Option[Action[Ctx, _]])

  lazy val continue: (Unit, Option[Action[Ctx, _]]) = (Unit, None)
  def continue(fieldVal: FieldVal): (FieldVal, Option[Action[Ctx, _]]) = (fieldVal, None)
}

trait MiddlewareAfterField[Ctx] extends MiddlewareBeforeField[Ctx] {
  def afterField(queryVal: QueryVal, fieldVal: FieldVal, value: Any, mctx: MiddlewareQueryContext[Ctx, _, _], ctx: Context[Ctx, _]): Option[Any]
}

trait MiddlewareToScalar[Ctx] extends Middleware[Ctx] {
  def toScalar(value: Any, inputType: InputType[_], ctx: Ctx): Option[Any]
}

trait MiddlewareFromScalar[Ctx] extends Middleware[Ctx] {
  def fromScalar(value: Any, inputType: InputType[_], ctx: Ctx): Option[Either[Violation, Any]]
}

trait MiddlewareErrorField[Ctx] extends MiddlewareBeforeField[Ctx] {
  def fieldError(queryVal: QueryVal, fieldVal: FieldVal, error: Throwable, mctx: MiddlewareQueryContext[Ctx, _, _], ctx: Context[Ctx, _]): Unit
}

case class MiddlewareQueryContext[+Ctx, RootVal, Input](
  ctx: Ctx,
  executor: Executor[_ <: Ctx, RootVal],
  queryAst: ast.Document,
  operationName: Option[String],
  variables: Input,
  inputUnmarshaller: InputUnmarshaller[Input],
  validationTiming: TimeMeasurement,
  queryReducerTiming: TimeMeasurement)

trait FieldTag

case class StringTag(name: String)

object FieldTag {
  implicit def stringTag(s: String): StringTag = StringTag(s)
  implicit def symbolTag(s: Symbol): StringTag = StringTag(s.name)
}

case class SubscriptionField[S[_]](stream: SubscriptionStream[S]) extends FieldTag


