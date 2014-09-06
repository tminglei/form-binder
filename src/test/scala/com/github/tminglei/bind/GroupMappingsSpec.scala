package com.github.tminglei.bind

import org.scalatest._

class GroupMappingsSpec extends FunSpec with ShouldMatchers with Mappings with Constraints {

  describe("test pre-defined group mappings") {
    val dummyMessages: Messages = (key) => "dummy"

    describe("group-mapping1") {
      val mapping1 = tmapping(
        "count" -> number()
      ).label("xx") verifying { (label, v, messages) =>
        if (v < 3) Seq("count" -> s"$v: cannot less than 3")
        else if (v > 10) Seq("count" -> s"$label: cannot greater than 10")
        else Nil
      }

      it("invalid data") {
        val invalidData = Map("count" -> "t5")
        mapping1.validate("", invalidData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("count" -> "dummy"))
        }
      }

      it("out-of-scope data") {
        val invalidData = Map("count" -> "15")
        mapping1.validate("", invalidData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("count" -> "xx: cannot greater than 10"))
        }
      }

      it("valid data") {
        val validData = Map("count" -> "5")
        mapping1.validate("", validData, dummyMessages) match {
          case Nil => mapping1.convert("", validData) should be (5)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        mapping1.validate("", nullData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("" -> "dummy"))
        }
      }

      it("empty data") {
        val emptyData = Map("" -> null)
        mapping1.validate("", emptyData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("" -> "dummy"))
        }
      }
    }

    describe("group-mapping2") {
      val mapping2 = tmapping(
        "price" -> float(),
        "count" -> number().verifying(min(3), max(10))
      ).label("xx") verifying { case (label, (price, count), messages) =>
        if (price * count > 1000) {
          Seq("" -> s"$label: $price * $count = ${price * count}, too much")
        } else Nil
      }

      it("invalid data") {
        val invalidData = Map("price" -> "23.5f", "count" -> "t5")
        mapping2.validate("", invalidData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("count" -> "dummy"))
        }
      }

      it("out-of-scope data") {
        val invalidData = Map("price" -> "23.5f", "count" -> "15")
        mapping2.validate("", invalidData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("count" -> "dummy"))
        }
      }

      it("out-of-scope data1") {
        val invalidData = Map("price" -> "123.5f", "count" -> "9")
        mapping2.validate("", invalidData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("" -> "xx: 123.5 * 9 = 1111.5, too much"))
        }
      }

      it("valid data") {
        val validData = Map("price" -> "23.5", "count" -> "5")
        mapping2.validate("", validData, dummyMessages) match {
          case Nil => mapping2.convert("", validData) should be ((23.5f, 5))
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        mapping2.validate("", nullData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("" -> "dummy"))
        }
      }

      it("empty data") {
        val emptyData = Map("" -> null)
        mapping2.validate("", emptyData, dummyMessages) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("" -> "dummy"))
        }
      }
    }
  }
}
