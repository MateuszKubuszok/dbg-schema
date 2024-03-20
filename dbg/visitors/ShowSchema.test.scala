package dbg.visitors

class ShowSchemaSpec extends munit.FunSuite {

  enum Foo derives dbg.schema.Schema {
    case Bar
    case Baz(a: Int, b: String)
  }

  test("Pretty print simple enum") {
    val result = ShowSchema.prettyPrint(Foo.Baz(1, "wololo"))
    assert(result == s"""(ShowSchemaSpec.this.Foo.Baz(
                        |  a = 1,
                        |  b = "wololo"
                        |) : ShowSchemaSpec.this.Foo)""".stripMargin)
  }

  case class Foo2(bar: Foo2.Bar) derives dbg.schema.Schema
  object Foo2 {
    case class Bar(a: Int, foo: Option[Foo2])
  }

  test("Pretty print recursive data") {
    val result = ShowSchema.prettyPrint(Foo2(Foo2.Bar(1, Some(Foo2(Foo2.Bar(2, None))))))
    assert(result == s"""ShowSchemaSpec.this.Foo2(
                        |  bar = ShowSchemaSpec.this.Foo2.Bar(
                        |    a = 1,
                        |    foo = (scala.Some(
                        |      value = ShowSchemaSpec.this.Foo2(
                        |        bar = ShowSchemaSpec.this.Foo2.Bar(
                        |          a = 2,
                        |          foo = (scala.None.type : scala.Option)
                        |        )
                        |      )
                        |    ) : scala.Option)
                        |  )
                        |)""".stripMargin)
  }
}
