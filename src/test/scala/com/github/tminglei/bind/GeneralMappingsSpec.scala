package com.github.tminglei.bind

import org.scalatest._

class GeneralMappingsSpec extends FunSpec with ShouldMatchers {

  case class TestBean(id: Long, name: String, desc: Option[String] = None)

  describe("test pre-defined general usage mappings") {
    val dummyMessages: Messages = (key) => "dummy"

    describe("ignored-simple") {
      val ignored = Mappings.ignored(35)

      it("invalid data") {
        val invalidData = Map("number" -> "t135")
        ignored.validate("number", invalidData, dummyMessages) match {
          case Nil => ignored.convert("number", invalidData) should be (35)
          case err => err should be (Nil)
        }
      }

      it("valid data") {
        val validData = Map("number" -> "135")
        ignored.validate("number", validData, dummyMessages) match {
          case Nil => ignored.convert("number", validData) should be (35)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        ignored.validate("number", nullData, dummyMessages) match {
          case Nil => ignored.convert("number", nullData) should be (35)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("number" -> "")
        ignored.validate("number", emptyData, dummyMessages) match {
          case Nil => ignored.convert("number", emptyData) should be (35)
          case err => err should be (Nil)
        }
      }
    }

    describe("optional-simple") {
      val base = Mappings.number()
      val optional = Mappings.optional(base)

      it("invalid data") {
        val invalidData = Map("number" -> "t122345")
        optional.validate("number", invalidData, dummyMessages) match {
          case Nil => {
            base.validate("number", invalidData, dummyMessages) should be (Seq("number" -> "dummy"))
            optional.convert("number", invalidData) should be (None)
          }
          case err => err should be (Nil)
        }
      }

      it("valid data") {
        val validData = Map("number" -> "122345")
        optional.validate("number", validData, dummyMessages) match {
          case Nil => {
            base.validate("number", validData, dummyMessages) should be (Nil)
            optional.convert("number", validData) should be (Some(122345))
          }
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        optional.validate("number", nullData, dummyMessages) match {
          case Nil => {
            base.validate("number", nullData, dummyMessages) should be (Nil)
            optional.convert("number", nullData) should be (None)
          }
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("number" -> "")
        optional.validate("number", emptyData, dummyMessages) match {
          case Nil => {
            base.validate("number", emptyData, dummyMessages) should be (Nil)
            optional.convert("number", emptyData) should be (None)
          }
          case err => err should be (Nil)
        }
      }
    }

    describe("default-simple") {
      val base = Mappings.number()
      val default = Mappings.default(base, 101)

      it("invalid data") {
        val invalidData = Map("number" -> "t122345")
        default.validate("number", invalidData, dummyMessages) match {
          case Nil => {
            base.validate("number", invalidData, dummyMessages) should be (Seq("number" -> "dummy"))
            default.convert("number", invalidData) should be (101)
          }
          case err => err should be (Nil)
        }
      }

      it("valid data") {
        val validData = Map("number" -> "122345")
        default.validate("number", validData, dummyMessages) match {
          case Nil => {
            base.validate("number", validData, dummyMessages) should be (Nil)
            default.convert("number", validData) should be (122345)
          }
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        default.validate("number", nullData, dummyMessages) match {
          case Nil => {
            base.validate("number", nullData, dummyMessages) should be (Nil)
            default.convert("number", nullData) should be (101)
          }
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("number" -> "")
        default.validate("number", emptyData, dummyMessages) match {
          case Nil => {
            base.validate("number", emptyData, dummyMessages) should be (Nil)
            default.convert("number", emptyData) should be (101)
          }
          case err => err should be (Nil)
        }
      }
    }

    describe("list-simple") {
      val base = Mappings.number()
      val list = Mappings.list(base)

      it("invalid data") {
        val invalidData = Map("number[0]" -> "t122345", "number[1]" -> "t11345")
        list.validate("number", invalidData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => {
            base.validate("number[0]", invalidData, dummyMessages) should be (Seq("number[0]" -> "dummy"))
            base.validate("number[1]", invalidData, dummyMessages) should be (Seq("number[1]" -> "dummy"))
            err should be (Seq("number[0]" -> "dummy", "number[1]" -> "dummy"))
          }
        }
      }

      it("valid data") {
        val validData = Map("number[0]" -> "122345", "number[1]" -> "754")
        list.validate("number", validData, dummyMessages) match {
          case Nil => {
            base.validate("number[0]", validData, dummyMessages) should be (Nil)
            base.validate("number[1]", validData, dummyMessages) should be (Nil)
            list.convert("number", validData) should be (List(122345, 754))
          }
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        list.validate("number", nullData, dummyMessages) match {
          case Nil => {
            base.validate("number[0]", nullData, dummyMessages) should be (Nil)
            list.convert("number", nullData) should be (Nil)
          }
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("number[0]" -> "", "number[1]" -> "133")
        list.validate("number", emptyData, dummyMessages) match {
          case Nil => {
            base.validate("number[0]", emptyData, dummyMessages) should be (Nil)
            base.validate("number[1]", emptyData, dummyMessages) should be (Nil)
            list.convert("number", emptyData) should be (List(0, 133))
          }
          case err => err should be (Nil)
        }
      }
    }

    describe("map-simple") {
      val base = Mappings.number()
      val map = Mappings.map(base)

      it("invalid data") {
        val invalidData = Map("map.aa" -> "t122345", "map.\"b-1\"" -> "t11345")
        map.validate("map", invalidData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => {
            base.validate("map.aa", invalidData, dummyMessages) should be (Seq("map.aa" -> "dummy"))
            base.validate("map.\"b-1\"", invalidData, dummyMessages) should be (Seq("map.\"b-1\"" -> "dummy"))
            err should be (Seq("map.aa" -> "dummy", "map.\"b-1\"" -> "dummy"))
          }
        }
      }

      it("valid data") {
        val validData = Map("map.aa" -> "122345", "map.\"b-1\"" -> "754")
        map.validate("map", validData, dummyMessages) match {
          case Nil => {
            base.validate("map.aa", validData, dummyMessages) should be (Nil)
            base.validate("map.\"b-1\"", validData, dummyMessages) should be (Nil)
            map.convert("map", validData) should be (Map("aa" -> 122345, "b-1" -> 754))
          }
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        map.validate("map", nullData, dummyMessages) match {
          case Nil => {
            base.validate("map.aa", nullData, dummyMessages) should be (Nil)
            map.convert("map", nullData) should be (Map())
          }
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("map.aa" -> "", "map.\"b-1\"" -> "133")
        map.validate("map", emptyData, dummyMessages) match {
          case Nil => {
            base.validate("map.aa", emptyData, dummyMessages) should be (Nil)
            base.validate("map.\"b-1\"", emptyData, dummyMessages) should be (Nil)
            map.convert("map", emptyData) should be (Map("aa" -> 0, "b-1" -> 133))
          }
          case err => err should be (Nil)
        }
      }
    }

    describe("ignored-compound") {
      val testBean = TestBean(101, "test")
      val ignored = Mappings.ignored(testBean)

      it("invalid data") {
        val invalidData = Map(
          "test.id" -> "t135",
          "test.name" -> "test",
          "test.desc" -> ""
        )
        ignored.validate("", invalidData, dummyMessages) match {
          case Nil => ignored.convert("", invalidData) should be (testBean)
          case err => err should be (Nil)
        }
      }

      it("valid data") {
        val validData = Map(
          "test.id" -> "135",
          "test.name" -> "test",
          "test.desc" -> ""
        )
        ignored.validate("", validData, dummyMessages) match {
          case Nil => ignored.convert("", validData) should be (testBean)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        ignored.validate("", nullData, dummyMessages) match {
          case Nil => ignored.convert("", nullData) should be (testBean)
          case err => err should be (Nil)
        }
      }

      it("empty data (wrong way)") {
        val emptyData = Map("test" -> "")
        ignored.validate("", emptyData, dummyMessages) match {
          case Nil => ignored.convert("", emptyData) should be (testBean)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("test" -> null)
        ignored.validate("", emptyData, dummyMessages) match {
          case Nil => ignored.convert("", emptyData) should be (testBean)
          case err => err should be (Nil)
        }
      }
    }

    describe("optional-compound") {
      val dummyMessages1: Messages = (key: String) => {
        if (key == "error.object") "%s missing or not valid"
        else "dummy"
      }

      val base = Mappings.mapping(
        "id" -> Mappings.long(),
        "name" -> Mappings.text(),
        "desc" -> Mappings.optional(Mappings.text())
      )(TestBean.apply)

      val optional = Mappings.optional(base)

      it("invalid data") {
        val invalidData = Map(
          "test.id" -> "t135",
          "test.name" -> "test",
          "test.desc" -> ""
        )
        optional.validate("test", invalidData, dummyMessages1) match {
          case Nil => {
            base.validate("test", invalidData, dummyMessages1) should be (Seq("test.id" -> "dummy"))
            optional.convert("test", invalidData) should be (None)
          }
          case err => err should be (Nil)
        }
      }

      it("valid data") {
        val validData = Map(
          "test.id" -> "135",
          "test.name" -> "test",
          "test.desc" -> ""
        )
        optional.validate("test", validData, dummyMessages1) match {
          case Nil => {
            base.validate("test", validData, dummyMessages1) should be (Nil)
            optional.convert("test", validData) should be (Some(TestBean(135, "test")))
          }
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        optional.validate("test", nullData, dummyMessages1) match {
          case Nil => {
            base.validate("test", nullData, dummyMessages1) should be (Seq("test" -> "test missing or not valid"))
            optional.convert("test", nullData) should be (None)
          }
          case err => err should be (Nil)
        }
      }

      it("empty data (wrong way)") {
        val emptyData = Map("test" -> "")
        optional.validate("test", emptyData, dummyMessages1) match {
          case Nil => {
            base.validate("test", emptyData, dummyMessages1) should be (Seq("test" -> "test missing or not valid"))
            optional.convert("test", emptyData) should be (None)
          }
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("test" -> null)
        optional.validate("test", emptyData, dummyMessages1) match {
          case Nil => {
            base.validate("test", emptyData, dummyMessages1) should be (Seq("test" -> "test missing or not valid"))
            optional.convert("test", emptyData) should be (None)
          }
          case err => err should be (Nil)
        }
      }
    }

    describe("default-compound") {
      val dummyMessages1: Messages = (key: String) => {
        if (key == "error.object") "%s missing or not valid"
        else "dummy"
      }

      val base = Mappings.mapping(
        "id" -> Mappings.long(),
        "name" -> Mappings.text(),
        "desc" -> Mappings.optional(Mappings.text())
      )(TestBean.apply)

      val testBean = TestBean(35, "test1", Some("test bean"))
      val default = Mappings.default(base, testBean)

      it("invalid data") {
        val invalidData = Map(
          "test.id" -> "t135",
          "test.name" -> "test",
          "test.desc" -> ""
        )
        default.validate("test", invalidData, dummyMessages1) match {
          case Nil => {
            base.validate("test", invalidData, dummyMessages1) should be (Seq("test.id" -> "dummy"))
            default.convert("test", invalidData) should be (testBean)
          }
          case err => err should be (Nil)
        }
      }

      it("valid data") {
        val validData = Map(
          "test.id" -> "135",
          "test.name" -> "test",
          "test.desc" -> ""
        )
        default.validate("test", validData, dummyMessages1) match {
          case Nil => {
            base.validate("test", validData, dummyMessages1) should be (Nil)
            default.convert("test", validData) should be (TestBean(135, "test"))
          }
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        default.validate("test", nullData, dummyMessages1) match {
          case Nil => {
            base.validate("test", nullData, dummyMessages1) should be (Seq("test" -> "test missing or not valid"))
            default.convert("test", nullData) should be (testBean)
          }
          case err => err should be (Nil)
        }
      }

      it("empty data (wrong way)") {
        val emptyData = Map("test" -> "")
        default.validate("test", emptyData, dummyMessages1) match {
          case Nil => {
            base.validate("test", emptyData, dummyMessages1) should be (Seq("test" -> "test missing or not valid"))
            default.convert("test", emptyData) should be (testBean)
          }
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("test" -> null)
        default.validate("test", emptyData, dummyMessages1) match {
          case Nil => {
            base.validate("test", emptyData, dummyMessages1) should be (Seq("test" -> "test missing or not valid"))
            default.convert("test", emptyData) should be (testBean)
          }
          case err => err should be (Nil)
        }
      }
    }
  }
}
