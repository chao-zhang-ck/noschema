package org.datacrafts.noschema

import org.datacrafts.noschema.NoSchemaTest._
import org.datacrafts.noschema.rule.DefaultRule
import org.datacrafts.scrooge.shapes.{MapExample, NestedStructExample, StructExample, UnionExample}
import org.scalatest.FlatSpec

// scalastyle:off
class NoSchemaTest extends FlatSpec with NoSchemaDsl {

  "Marshalling and unmarshalling with Map" should "be successful" in {

    // can just print the schema itself
    println(schemaOf[TestClass].format())

    val op = DefaultRule.withSchema[TestClass]
    println(op.format())
    // equivalent to the above, just DSL syntactic sugar
    val op2 = schemaOf[TestClass].operation()
    println(op2.format())

    assert(
      op.operator.marshal(
        Map(
          "v1" -> 10,
          "v5" -> Map("_2" -> 12),
          "v3" -> Iterable(Seq("v21" -> 3)),
          "v6" -> TestClass3(v31 = 5),
          "v7" -> ("org.datacrafts.noschema.NoSchemaTest.Fruit.Apple", Map("name" -> "bigApple")),
          "v8" -> Map("v" -> Seq(Map("v" -> Seq.empty, "v2" -> 2)), "v2" -> 1),
          "thriftMap" -> Map("id" -> "1"),
          "thriftNested" -> Map("str" -> Map("foo" -> "bar")),
          "thriftUnion" -> ("scala.Int", 1),
          "thriftUnion2" -> UnionExample.C(c = "test")
        )) == TestClass(
        v1 = 10,
        v5 = (null, 12),
        v3 = Some(Seq(Some(
          TestClass2(
            v21 = 3,
            v22 = null
          )))),
        v6 = Some(TestClass3(v31 = 5)),
        v2 = None,
        v4 = null,
        v7 = Fruit.Apple("bigApple"),
        v8 = Recursive(Seq(Recursive(v2 = 2)), v2 = 1),
        thriftNested = NestedStructExample(str = StructExample(foo = "bar")),
        thriftMap = MapExample(id = "1"),
        thriftUnion = UnionExample.B(b = 1),
        thriftUnion2 = UnionExample.C(c = "test")
      )
    )

    assert(
      op.operator.unmarshal(
        TestClass(
          v1 = 1,
          v2 = null
        )
      ) == Map(
        "v1" -> 1,
        "v2" -> null,
        // the rest are default values
        "v6" -> null,
        "v7" -> ("org.datacrafts.noschema.NoSchemaTest.Apple", Map("size" -> 1)),
        "v8" -> Map("v2" -> 1, "v" -> Seq.empty),
        "v5" -> Map("_2" -> 2, "_1" -> "a"),
        "v4" -> null,
        "v3" -> Seq(
          Map(
            "v21" -> 3,
            "v22" -> Map("v" -> Map(), "v32" -> Seq(12.0), "v31" -> 0)
          )
        ),
        "thriftMap" -> Map("id" -> "1", "metadata" -> null),
        "thriftNested" -> Map("str" -> Map("foo" -> "bar", "bar" -> null), "qux" -> null),
        "thriftUnion" -> ("scala.Int", 1),
        "thriftUnion2" -> ("org.datacrafts.scrooge.shapes.StructExample", Map("foo" -> "bar", "bar" -> null))
      )
    )

  }
}

object NoSchemaTest {

  case class TestClass(
    v1: Int,
    v2: Option[Seq[Option[Double]]] = None,
    v3: Option[Seq[Option[TestClass2]]] = Some(Seq(Some(TestClass2()))),
    v4: Seq[Int] = null,
    v5: (String, Int) = ("a", 2),
    v6: Option[TestClass3] = None,
    v7: Fruit = Apple(),
    v8: Recursive = Recursive(),
    thriftNested: NestedStructExample = NestedStructExample(str = StructExample(foo = "bar")),
    thriftMap: MapExample = MapExample(id = "1"),
    thriftUnion: UnionExample = UnionExample.B(b = 1),
    thriftUnion2: UnionExample = UnionExample.A(StructExample(foo = "bar"))
  )

  case class TestClass2(v21: Int = 3,
    v22: TestClass3 = TestClass3(0)
  )

  case class TestClass3(v31: Int,
    v32: Iterable[Double] = Seq(12),
    v: Map[String, Int] = Map.empty
  )

  // if there are members with the recursive type and non-empty default value
  // it'll cause runtime instantiation failure due to infinite recursion
  // type wise, it should be supported regardless
  case class Recursive(
    v: Seq[Recursive] = Seq.empty,
    v2: Int = 1
  )

  sealed trait Fruit

  case class Apple(size: Int = 1) extends Fruit
  case class Pear(size: Double = 1.5) extends Fruit

  object Fruit {
    case class Apple(name: String = "red") extends Fruit
  }
}
