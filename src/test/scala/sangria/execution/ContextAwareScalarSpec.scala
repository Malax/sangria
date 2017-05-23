package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.util.{FutureResultSupport}
import sangria.schema._
import sangria.macros._
import sangria.marshalling.ScalaInput
import sangria.validation.{ValueCoercionViolation, Violation}

import scala.concurrent.ExecutionContext.Implicits.global

class ContextAwareScalarSpec extends WordSpec with Matchers with FutureResultSupport {

  case class IDEncodingViolation(error: String) extends ValueCoercionViolation(error)

  class Ctx(prefix: String) {
    def encodeId(id: String): String = prefix + id
    def decodeId(id: String): Either[Violation, String] =
      if (id.startsWith(prefix))
        Right(id.substring(prefix.length))
      else
        Left(IDEncodingViolation("invalid id"))
  }

  implicit val EncodedIdType = ContextAwareScalarAlias[Ctx, String, String](
    StringType,
    (ctx, id) ⇒ ctx.encodeId(id),
    (ctx, id) ⇒ ctx.decodeId(id))

  val ComplexInputType = InputObjectType("Complex", List(
    InputField("userId", OptionInputType(EncodedIdType)),
    InputField("name", StringType)))

  val IdArg = Argument("id", EncodedIdType)
  val ComplexArg = Argument("c", ComplexInputType)

  "ContextAwareScalarAlias" should {
    "encode and decode scalar value" in {
      val schema = Schema(ObjectType("Query", fields[Ctx, Unit](
        Field("test", OptionType(EncodedIdType),
          arguments = IdArg :: ComplexArg :: Nil,
          resolve = _.withArgs(IdArg, ComplexArg)(
            (id, complex) ⇒ id + "-" + complex("userId").asInstanceOf[Option[UserId]].get + "-" + complex("name")))
      )))

      val query =
        graphql"""
          query Test($$id: String!, $$c: Complex!) {
            t1: test(id: "test-a", c: {userId: "test-b", name: "foo"})
            t2: test(id: $$id, c: $$c)
            t3: test(id: "invalid", c: {userId: "yay", name: "foo"})
          }
        """

      val ctx = new Ctx("test-")

      val vars = ScalaInput.scalaInput(Map(
        "id" → "test-c",
        "c" → Map(
          "userId" → "test-d",
          "name" → "bar")))

      Executor.execute(schema, query, ctx, variables = vars).await should be (
        Map(
          "data" → Map(
            "t1" → "test-a-b-foo",
            "t2" → "test-c-d-bar",
            "t3" → null),
          "errors" → Vector(
            Map(
              "message" → "Field 'id' has wrong value: invalid id. (line 5, column 26):\n            t3: test(id: \"invalid\", c: {userId: \"yay\", name: \"foo\"})\n                         ^",
              "path" → Vector("t3"),
              "locations" → Vector(Map("line" → 5, "column" → 26))),
            Map(
              "message" → "Field 'c.userId' has wrong value: invalid id. (line 5, column 49):\n            t3: test(id: \"invalid\", c: {userId: \"yay\", name: \"foo\"})\n                                                ^",
              "path" → Vector("t3"),
              "locations" → Vector(Map("line" → 5, "column" → 49)))))
      )
    }
  }
}