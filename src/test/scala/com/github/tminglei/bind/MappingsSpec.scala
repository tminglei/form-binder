package com.github.tminglei.bind

import org.scalatest._

class MappingsSpec extends FunSpec with ShouldMatchers with Constraints with Processors {

  describe("test pre-defined field mappings") {
    val dummyMessages: Messages = (key) => "dummy"

    describe("text") {
      val text = trim pipe_: Mappings.text()

      it("valid data") {
        val data = Map("text" -> "tett ")
        text.validate("text", data, dummyMessages) match {
          case Nil => text.convert("text", data) should be ("tett")
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        text.validate("text", nullData, dummyMessages) match {
          case Nil => text.convert("text", nullData) should be (null)
          case err => err should be (Nil)
        }
      }
    }

    describe("boolean") {
      val boolean = Mappings.boolean()

      it("invalid data") {
        val invalidData = Map("boolean" -> "teed")
        boolean.validate("boolean", invalidData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("boolean" -> "dummy"))
        }
      }

      it("valid data") {
        val validData = Map("boolean" -> "true")
        boolean.validate("boolean", validData, dummyMessages) match {
          case Nil => boolean.convert("boolean", validData) should be (true)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        boolean.validate("boolean", nullData, dummyMessages) match {
          case Nil => boolean.convert("boolean", nullData) should be (false)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("boolean" -> "")
        boolean.validate("boolean", emptyData, dummyMessages) match {
          case Nil => boolean.convert("boolean", emptyData) should be (false)
          case err => err should be (Nil)
        }
      }
    }

    describe("number") {
      val number = (cleanComma pipe_: Mappings.number()).verifying(min(1000), max(10000))

      it("invalid data") {
        val invalidData = Map("number" -> "t12345")
        number.validate("number", invalidData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("number" -> "dummy"))
        }
      }

      it("out-of-scope data") {
        val outScopeData = Map("number" -> "345")
        number.validate("number", outScopeData, dummyMessages) match {
          case Nil => ("out of scope - shouldn't occur!") should be ("")
          case err => err should be (Seq("number" -> "dummy"))
        }
      }

      it("long number data") {
        val number1 = Mappings.number()
        val longNumberData = Map("number" -> "146894532240")
        number1.validate("number", longNumberData, dummyMessages) match {
          case Nil => ("long number - shouldn't occur!") should be ("")
          case err => err should be (Seq("number" -> "dummy"))
        }
      }

      it("valid data w/ comma") {
        val validData = Map("number" -> "3,549")
        number.validate("number", validData, dummyMessages) match {
          case Nil => number.convert("number", validData) should be (3549)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        number.convert("number", nullData) should be (0)
        number.validate("number", nullData, dummyMessages) match {
          case Nil => ("(null->) 0 - shouldn't occur!") should be ("")
          case err => err should be (Seq("number" -> "dummy"))
        }
      }

      it("empty data") {
        val emptyData = Map("number" -> "")
        number.convert("number", emptyData) should be (0)
        number.validate("number", emptyData, dummyMessages) match {
          case Nil => ("(empty->) 0 - shouldn't occur!") should be ("")
          case err => err should be (Seq("number" -> "dummy"))
        }
      }
    }
  }
}
