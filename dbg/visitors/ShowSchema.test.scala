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
}
