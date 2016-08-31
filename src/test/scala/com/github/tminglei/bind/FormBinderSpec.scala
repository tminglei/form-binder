package com.github.tminglei.bind

import org.scalatest._
import java.util.ResourceBundle

import scala.collection.mutable.ListBuffer

class FormBinderSpec extends FunSpec with Matchers {
  import com.github.tminglei.bind.simple._

  val bundle: ResourceBundle = ResourceBundle.getBundle("bind-messages")
  val messages: Messages = (key) => Option(bundle.getString(key))

  describe("show and check form binder") {

    describe("usage cases") {
      val mappings = expandJson(Some("body")) >-: tmapping(
        "id" -> long(),
        "body" -> tmapping(
          "price" -> (omitLeft("$") >-: float()),
          "count" -> number().verifying(min(3), max(10))
        ).label("xx").verifying { case (label, (price, count), messages) =>
          if (price * count > 1000) {
            Seq(s"$label: $price * $count = ${price * count}, too much")
          } else Nil
        }
      )

      it("w/ valid data") {
        val binder = FormBinder(messages)
        val validData = Map(
          "id" -> "133",
          "body" -> """{"price":"$137.5", "count":5}"""
        )
        binder.bind(mappings, validData).fold(
        errors => "shouldn't happen!!!",
        { case (id, (price, count)) =>
          id should be (133L)
          price should be (137.5f)
          count should be (5)
          (">> bind successful!")
        }
        ) should be (">> bind successful!")
      }

      it("w/ invalid data") {
        val binder = FormBinder(messages)
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"price":337.5, "count":5}"""
        )
        binder.bind(mappings, invalidData).fold(
          errors => errors,
          { case (id, (price, count)) =>
            ("invalid - shouldn't occur!") should be ("")
          }
        ) should be (List("body" -> "xx: 337.5 * 5 = 1687.5, too much"))
      }

      it("w/ invalid data + errors processor") {
        val binder = FormBinder(messages, errsTree())
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"price":337.5, "count":5}"""
        )
        binder.bind(mappings, invalidData).fold(
            errors => errors,
            { case (id, (price, count)) =>
              ("invalid - shouldn't occur!") should be("")
            }
          )  should be(Map("body" -> Map("_errors" -> ListBuffer("xx: 337.5 * 5 = 1687.5, too much"))))
      }
    }

    describe("w/ options") {
      val mappings = expandJson(Some("body")) >-: tmapping(
        "id" -> long(),
        "body" -> tmapping(
          "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
          "price" -> (omitLeft("$") >-: float()),
          "count" -> number().verifying(min(3), max(10))
        ).label("xx").verifying { case (label, (email, price, count), messages) =>
          if (price * count > 1000) {
            Seq(s"$label: $price * $count = ${price * count}, too much")
          } else Nil
        }
      )

      it("w/ eager check") {
        val binder = FormBinder(messages)
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"email":"etttt.att#example-1111111.com", "price":337.5, "count":5}"""
        )

        binder.bind(mappings, invalidData).fold(
          errors => errors,
          { case (id, (email, price, count)) =>
            ("invalid - shouldn't occur!") should be ("")
          }
        ) should be (List("body.email" -> "etttt.att#example-1111111.com: length > 20"))
        ///
        binder.bind(mappings.options(_.eagerCheck(true)), invalidData).fold(
          errors => errors,
          { case (id, (email, price, count)) =>
            ("invalid - shouldn't occur!") should be ("")
          }
        ) should be (List(
          "body.email" -> "etttt.att#example-1111111.com: length > 20",
          "body.email" -> "etttt.att#example-1111111.com: invalid email"
        ))
        ///
        binder.validate(mappings.options(_.eagerCheck(true)), invalidData)
          .asInstanceOf[Seq[(String, String)]] should be (List(
            "body.email" -> "etttt.att#example-1111111.com: length > 20",
            "body.email" -> "etttt.att#example-1111111.com: invalid email"
          ))
      }

      it("w/ ignore empty") {
        val binder = FormBinder(messages)
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"email":null, "price":337.5, "count":5}"""
        )

        binder.bind(mappings, invalidData).fold(
          errors => errors,
          { case (id, (email, price, count)) =>
            ("invalid - shouldn't occur!") should be ("")
          }
        ) should be (List("body.email" -> "email is required"))
        ///
        binder.bind(mappings.options(_.skipUntouched(true)), invalidData).fold(
          errors => errors,
          { case (id, (email, price, count)) =>
            ("invalid - shouldn't occur!") should be ("")
          }
        ) should be (List("body" -> "xx: 337.5 * 5 = 1687.5, too much"))
      }

      it("w/ ignore empty and touched") {
        val binder = FormBinder(messages)
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"email":null, "price":337.5, "count":5}"""
        )

        binder.bind(mappings, invalidData).fold(
          errors => errors,
          { case (id, (email, price, count)) =>
            ("invalid - shouldn't occur!") should be ("")
          }
        ) should be (List("body.email" -> "email is required"))
        ///
        binder.bind(mappings.options(_.skipUntouched(true).touchedChecker(listTouched(List("body.email")))), invalidData).fold(
          errors => errors,
          { case (id, (email, price, count)) =>
            ("invalid - shouldn't occur!") should be ("")
          }
        ) should be (List("body.email" -> "email is required"))
        ///
        binder.validate(mappings.options(_.skipUntouched(true).touchedChecker(listTouched(List("body.email")))), invalidData)
          .asInstanceOf[Seq[(String, String)]] should be (List("body.email" -> "email is required"))
      }

      it("w/ ignore empty and touched (+)") {
        val binder1 = FormBinder(messages)
        //>>> group mapping with bulk pre-processor
        val mappings1 = expandJsonKeys(Some("touched")) >-: tmapping(
          "id" -> long(),
          "data" -> (expandJson() >-: tmapping(
            "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
            "price" -> (omitLeft("$") >-: float()),
            "count" -> number().verifying(min(3), max(10))
          )).label("xx").verifying { case (label, (email, price, count), messages) =>
            if (price * count > 1000) {
              Seq(s"$label: $price * $count = ${price * count}, too much")
            } else Nil
          }
        )
        val invalidData = Map(
          "id" -> "133",
          "data" -> """{"email":null, "price":337.5, "count":5}""",
          "touched" -> """["email", "price"]"""
        )

        binder1.validate(mappings1.options(_.skipUntouched(true).touchedChecker(prefixTouched("data", "touched"))), invalidData)
          .asInstanceOf[Seq[(String, String)]] should be (List("data.email" -> "email is required"))
      }

      it("w/ ignore empty and touched (combined)") {
        val binder = FormBinder(messages)
        val mappingx = expandJson(Some("body")) >-: expandListKeys(Some("body.touched")) >-:
          changePrefix("body.data", "body") >-: changePrefix("body.touched", "touched") >-: mappings
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"data": {"email":null, "price":337.5, "count":5}, "touched": ["email", "price"]}"""
        )

        binder.validate(mappingx.options(_.skipUntouched(true).touchedChecker(prefixTouched("body", "touched"))), invalidData)
          .asInstanceOf[Seq[(String, String)]] should be (List("body.email" -> "email is required"))
      }

      it("w/ i18n and label") {
        val messages1 = (key: String) => if (key == "xx") Some("haha") else Some("dummy")
        val binder = FormBinder(messages1)
        val mappings = tmapping(
          "id" -> long(),
          "body" -> (expandJson() >-: tmapping(
            "email" -> text(maxLength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
            "price" -> (omitLeft("$") >-: float()),
            "count" -> number().verifying(min(3), max(10))
          ).label("@xx").verifying { case (label, (email, price, count), messages) =>
            if (price * count > 1000) {
              Seq(s"$label: $price * $count = ${price * count}, too much")
            } else Nil
          })
        )

        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"email":"example@123.com", "price":337.5, "count":5}"""
        )

        binder.bind(mappings, invalidData).fold(
          errors => errors,
          { case (id, (email, price, count)) =>
            ("invalid - shouldn't occur!") should be ("")
          }
        ) should be (List("body" -> "haha: 337.5 * 5 = 1687.5, too much"))
      }
    }
  }
}