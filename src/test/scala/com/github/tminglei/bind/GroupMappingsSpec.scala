package com.github.tminglei.bind

import org.scalatest._
import java.util.ResourceBundle

class GroupMappingsSpec extends FunSpec with ShouldMatchers with Mappings with Constraints {
  val bundle: ResourceBundle = ResourceBundle.getBundle("bind-messages")
  val messages: Messages = (key) => Option(bundle.getString(key))

  describe("test pre-defined group mappings") {

    describe("group-mapping1") {
      val mapping1 = tmapping(
        "count" -> number()
      ).label("xx") verifying { (label, v, messages) =>
        if (v < 3) Seq(s"$v: cannot less than 3")
        else if (v > 10) Seq(s"$label: cannot greater than 10")
        else Nil
      }

      it("invalid data") {
        val invalidData = Map("count" -> "t5")
        mapping1.validate("", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("count" -> "'t5' must be a number."))
        }
      }

      it("out-of-scope data") {
        val invalidData = Map("count" -> "15")
        mapping1.validate("", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("" -> "xx: cannot greater than 10"))
        }
      }

      it("valid data") {
        val validData = Map("count" -> "5")
        mapping1.validate("", validData, messages, Options.apply()) match {
          case Nil => mapping1.convert("", validData) should be (5)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        mapping1.validate("", nullData, messages, Options.apply()) match {
          case Nil => mapping1.convert("", nullData) should be (null.asInstanceOf[(Int)])
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("" -> null)
        mapping1.validate("", emptyData, messages, Options.apply()) match {
          case Nil => mapping1.convert("", emptyData) should be (null.asInstanceOf[(Int)])
          case err => err should be (Nil)
        }
      }
    }

    describe("group-mapping2") {
      val mapping2 = tmapping(
        "price" -> float(),
        "count" -> number().verifying(min(3), max(10))
      ).label("xx") verifying { case (label, (price, count), messages) =>
        if (price * count > 1000) {
          Seq(s"$label: $price * $count = ${price * count}, too much")
        } else Nil
      }

      it("invalid data") {
        val invalidData = Map("price" -> "23.5f", "count" -> "t5")
        mapping2.validate("", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("count" -> "'t5' must be a number."))
        }
      }

      it("out-of-scope data") {
        val invalidData = Map("price" -> "23.5f", "count" -> "15")
        mapping2.validate("", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("count" -> "'15' cannot be greater than 10."))
        }
      }

      it("out-of-scope data1") {
        val invalidData = Map("price" -> "123.5f", "count" -> "9")
        mapping2.validate("", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("" -> "xx: 123.5 * 9 = 1111.5, too much"))
        }
      }

      it("valid data") {
        val validData = Map("price" -> "23.5", "count" -> "5")
        mapping2.validate("", validData, messages, Options.apply()) match {
          case Nil => mapping2.convert("", validData) should be ((23.5f, 5))
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        mapping2.validate("", nullData, messages, Options.apply()) match {
          case Nil => mapping2.convert("", nullData) should be (null.asInstanceOf[(Float, Int)])
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("" -> null)
        mapping2.validate("", emptyData, messages, Options.apply()) match {
          case Nil => mapping2.convert("", emptyData) should be (null.asInstanceOf[(Float, Int)])
          case err => err should be (Nil)
        }
      }
    }

    describe("w/ options") {

      it("w/ eager check") {
        val mappingx = tmapping(
          "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email")),
          "count" -> number().verifying(max(10, "%s > %s"), max(15, "%s > %s"))
        ).options(_.eagerCheck(true))
        val data = Map(
          "email" -> "etttt.att#example-1111111.com",
          "count" -> "20")

        mappingx.validate("", data, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq(
            "email" -> "etttt.att#example-1111111.com: length > 20",
            "email" -> "etttt.att#example-1111111.com: invalid email",
            "count" -> "20 > 10",
            "count" -> "20 > 15"))
        }
      }

      it("w/ ignore empty") {
        val mappingx = tmapping(
          "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
          "count" -> number().verifying(max(10, "%s: > %s"), max(15, "%s: > %s"))
        )
        val nullData = Map[String, String]()
        val emptyData = Map.empty + ("count" -> "")

        ///
        mappingx.validate("", nullData, messages, Options.apply()) match {
          case Nil => mappingx.convert("", nullData) should be (null.asInstanceOf[(String, Int)])
          case err => err should be (Nil)
        }

        mappingx.options(_.ignoreEmpty(true))
            .validate("", nullData, messages, Options.apply()) match {
          case Nil => mappingx.convert("", nullData) should be (null.asInstanceOf[(String, Int)])
          case err => err should be (Nil)
        }

        ///
        mappingx.validate("", emptyData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("email" -> "email is required"))
        }

        mappingx.options(_.ignoreEmpty(true))
            .validate("", emptyData, messages, Options.apply()) match {
          case Nil => mappingx.convert("", emptyData) should be ((null, 0))
          case err => err should be (Nil)
        }
      }

      it("w/ ignore empty and touched") {
        val mappingx = tmapping(
          "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
          "count" -> number().verifying(max(10, "%s: > %s"), max(15, "%s: > %s"))
        )
        val emptyData = Map.empty + ("count" -> "")

        ///
        mappingx.validate("", emptyData, messages, Options().touched(Processors.listTouched(List("email")))) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("email" -> "email is required"))
        }

        mappingx.options(_.ignoreEmpty(true))
            .validate("", emptyData, messages, Options().touched(Processors.listTouched(List("email")))) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("email" -> "email is required"))
        }
      }

      it("w/ eager check thru verifying") {
        val mappingx = tmapping(
          "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email")),
          "count" -> number().verifying(max(10, "%s > %s"), max(15, "%s > %s"))
        ).verifying().options(_.eagerCheck(true))
        val data = Map(
          "email" -> "etttt.att#example-1111111.com",
          "count" -> "20")

        mappingx.validate("", data, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq(
            "email" -> "etttt.att#example-1111111.com: length > 20",
            "email" -> "etttt.att#example-1111111.com: invalid email",
            "count" -> "20 > 10",
            "count" -> "20 > 15"))
        }
      }

      it("w/ ignore empty thru verifying") {
        val mappingx = tmapping(
          "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
          "count" -> number().verifying(max(10, "%s: > %s"), max(15, "%s: > %s"))
        ).verifying()
        val nullData = Map[String, String]()
        val emptyData = Map.empty + ("count" -> "")

        ///
        mappingx.validate("", nullData, messages, Options.apply()) match {
          case Nil => mappingx.convert("", nullData) should be (null.asInstanceOf[(String, Int)])
          case err => err should be (Nil)
        }

        mappingx.options(_.ignoreEmpty(true))
          .validate("", nullData, messages, Options.apply()) match {
          case Nil => mappingx.convert("", nullData) should be (null.asInstanceOf[(String, Int)])
          case err => err should be (Nil)
        }

        ///
        mappingx.validate("", emptyData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("email" -> "email is required"))
        }

        mappingx.options(_.ignoreEmpty(true))
          .validate("", emptyData, messages, Options.apply()) match {
          case Nil => mappingx.convert("", emptyData) should be ((null, 0))
          case err => err should be (Nil)
        }
      }

      it("w/ ignore empty and touched thru verifying") {
        val mappingx = tmapping(
          "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
          "count" -> number().verifying(max(10, "%s: > %s"), max(15, "%s: > %s"))
        ).verifying()
        val emptyData = Map.empty + ("count" -> "")

        ///
        mappingx.validate("", emptyData, messages, Options().touched(Processors.listTouched(List("email")))) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("email" -> "email is required"))
        }

        mappingx.options(_.ignoreEmpty(true))
          .validate("", emptyData, messages, Options().touched(Processors.listTouched(List("email")))) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("email" -> "email is required"))
        }
      }

      it("w/ eager check + transform (mapTo)") {
        val mappingx = tmapping(
          "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email")),
          "count" -> number().verifying(max(10, "%s > %s"), max(15, "%s > %s"))
        ).mapTo(identity).options(_.eagerCheck(true))
        val data = Map(
          "email" -> "etttt.att#example-1111111.com",
          "count" -> "20")

        mappingx.validate("", data, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq(
            "email" -> "etttt.att#example-1111111.com: length > 20",
            "email" -> "etttt.att#example-1111111.com: invalid email",
            "count" -> "20 > 10",
            "count" -> "20 > 15"))
        }
      }

      it("w/ ignore empty + transform (mapTo)") {
        val mappingx = tmapping(
          "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
          "count" -> number().verifying(max(10, "%s: > %s"), max(15, "%s: > %s"))
        ).mapTo(identity)
        val nullData = Map[String, String]()
        val emptyData = Map.empty + ("count" -> "")

        ///
        mappingx.validate("", nullData, messages, Options.apply()) match {
          case Nil => mappingx.convert("", nullData) should be (null.asInstanceOf[(String, Int)])
          case err => err should be (Nil)
        }

        mappingx.options(_.ignoreEmpty(true))
          .validate("", nullData, messages, Options.apply()) match {
          case Nil => mappingx.convert("", nullData) should be (null.asInstanceOf[(String, Int)])
          case err => err should be (Nil)
        }

        ///
        mappingx.validate("", emptyData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("email" -> "email is required"))
        }

        mappingx.options(_.ignoreEmpty(true))
          .validate("", emptyData, messages, Options.apply()) match {
          case Nil => mappingx.convert("", emptyData) should be ((null, 0))
          case err => err should be (Nil)
        }
      }

      it("w/ ignore empty and touched + transform (mapTo)") {
        val mappingx = tmapping(
          "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
          "count" -> number().verifying(max(10, "%s: > %s"), max(15, "%s: > %s"))
        ).mapTo(identity)
        val emptyData = Map.empty + ("count" -> "")

        ///
        mappingx.validate("", emptyData, messages, Options().touched(Processors.listTouched(List("email")))) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("email" -> "email is required"))
        }

        mappingx.options(_.ignoreEmpty(true))
          .validate("", emptyData, messages, Options().touched(Processors.listTouched(List("email")))) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("email" -> "email is required"))
        }
      }
    }
  }
}
