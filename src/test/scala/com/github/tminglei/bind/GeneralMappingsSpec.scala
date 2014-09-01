package com.github.tminglei.bind

import org.scalatest._

class GeneralMappingsSpec extends FunSpec with ShouldMatchers {

  describe("test pre-defined general usage mappings") {
    val dummyMessages: Messages = (key) => "dummy"

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
        val invalidData = Map("number[0]" -> "122345", "number[1]" -> "t11345")
        list.validate("number", invalidData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("number[1]" -> "dummy"))
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
  }
}
