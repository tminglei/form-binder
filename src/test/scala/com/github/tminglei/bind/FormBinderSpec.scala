package com.github.tminglei.bind

import org.json4s.jackson.JsonMethods
import org.scalatest._

class FormBinderSpec extends FunSpec with ShouldMatchers {
  import com.github.tminglei.bind.simple._

  describe("show and check form binder") {
    val messages = (key: String) => Some("dummy")

    describe("usage cases") {
      val binder = expandJsonString(Some("body"), Some("json")) >-: FormBinder(messages)

      val mappings = tmapping(
        "id" -> long(),
        "json" -> tmapping(
          "price" -> (cleanPrefix("$") >-: float()),
          "count" -> number().verifying(min(3), max(10))
        ).label("xx").verifying { case (label, (price, count), messages) =>
          if (price * count > 1000) {
            Seq("" -> s"$label: $price * $count = ${price * count}, too much")
          } else Nil
        }
      )

      it("w/ valid data") {
        val validData = Map(
          "id" -> "133",
          "body" -> """{"price":"$137.5", "count":5}"""
        )
        binder.bind(mappings, validData) { case (id, (price, count)) =>
          id should be (133L)
          price should be (137.5f)
          count should be (5)
          (">> bind successful!")
        } should be (">> bind successful!")
      }

      it("w/ invalid data") {
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"price":337.5, "count":5}"""
        )
        binder.bind(mappings, invalidData) { case (id, (price, count)) =>
          ("invalid - shouldn't occur!") should be ("")
        } should be (Map("json" -> List("xx: 337.5 * 5 = 1687.5, too much")))
      }

      it("w/ invalid data + errors processor") {
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"price":337.5, "count":5}"""
        )
        binder.withErr(errsToJson4s())
          .bind(mappings, invalidData) { case (id, (price, count)) =>
          ("invalid - shouldn't occur!") should be("")
        } should be(JsonMethods.parse(
          """
          {
            "json": {
              "_errors": ["xx: 337.5 * 5 = 1687.5, too much"]
            }
          }
          """))
      }
    }

    describe("w/ options") {
      val binder = expandJsonString(Some("body"), Some("json")) >-: FormBinder(messages)

      val mappings = tmapping(
        "id" -> long(),
        "json" -> tmapping(
          "email" -> text(maxlength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
          "price" -> (cleanPrefix("$") >-: float()),
          "count" -> number().verifying(min(3), max(10))
        ).label("xx").verifying { case (label, (email, price, count), messages) =>
          if (price * count > 1000) {
            Seq("" -> s"$label: $price * $count = ${price * count}, too much")
          } else Nil
        }
      )

      it("w/ eager check") {
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"email":"etttt.att#example-1111111.com", "price":337.5, "count":5}"""
        )

        binder.bind(mappings, invalidData) { case (id, (email, price, count)) =>
          ("invalid - shouldn't occur!") should be ("")
        } should be (Map(
          "json.email" -> List("etttt.att#example-1111111.com: length > 20")
        ))
        ///
        binder.bind(mappings.options(_.eagerCheck(true)), invalidData) { case (id, (email, price, count)) =>
          ("invalid - shouldn't occur!") should be ("")
        } should be (Map(
          "json.email" -> List("etttt.att#example-1111111.com: length > 20", "etttt.att#example-1111111.com: invalid email")
        ))
        ///
        binder.validate(mappings.options(_.eagerCheck(true)), invalidData) should be (Map(
          "json.email" -> List("etttt.att#example-1111111.com: length > 20", "etttt.att#example-1111111.com: invalid email")
        ))
      }

      it("w/ ignore empty") {
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"email":null, "price":337.5, "count":5}"""
        )

        binder.bind(mappings, invalidData) { case (id, (email, price, count)) =>
          ("invalid - shouldn't occur!") should be ("")
        } should be (Map(
          "json.email" -> List("email is required")
        ))
        ///
        binder.bind(mappings.options(_.ignoreEmpty(true)), invalidData) { case (id, (email, price, count)) =>
          ("invalid - shouldn't occur!") should be ("")
        } should be (Map("json" -> List("xx: 337.5 * 5 = 1687.5, too much")))
      }

      it("w/ ignore empty and touched") {
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"email":null, "price":337.5, "count":5}"""
        )

        binder.bind(mappings, invalidData) { case (id, (email, price, count)) =>
          ("invalid - shouldn't occur!") should be ("")
        } should be (Map(
          "json.email" -> List("email is required")
        ))
        ///
        binder.bind(mappings.options(_.ignoreEmpty(true).copy(touched = List("json.email"))),
            invalidData) { case (id, (email, price, count)) =>
          ("invalid - shouldn't occur!") should be ("")
        } should be (Map("json.email" -> List("email is required")))
        ///
        binder.validate(mappings.options(_.ignoreEmpty(true)), invalidData,
          touched = Some(List("json.email"))) should be (Map("json.email" -> List("email is required")))
      }

      it("w/ ignore empty and touched (+)") {
        val binder1 = FormBinder(messages).withTouched(expandJsonTouched("touched", "body"))
        //>>> group mapping with bulk pre-processor
        val mappings1 = tmapping(
          "id" -> long(),
          "body" -> (expandJsonString() >-: tmapping(
            "email" -> text(maxlength(20, "%s: length > %s"), email("%s: invalid email"), required("%s is required")),
            "price" -> (cleanPrefix("$") >-: float()),
            "count" -> number().verifying(min(3), max(10))
          )).label("xx").verifying { case (label, (email, price, count), messages) =>
            if (price * count > 1000) {
              Seq("" -> s"$label: $price * $count = ${price * count}, too much")
            } else Nil
          }
        )
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"email":null, "price":337.5, "count":5}""",
          "touched" -> """{"email":true, "price":false}"""
        )

        binder1.validate(mappings1.options(_.ignoreEmpty(true)),
          invalidData) should be (Map("body.email" -> List("email is required")))
      }

      it("w/ ignore empty and touched (combined)") {
        val expand = expandJsonString(Some("body"), Some("json"))
        val binder1 = expand >-: changePrefix("json.data", "json") >-: FormBinder(messages)
          .withTouched((data) => extractTouched("json.touched", "json").apply(expand("", data)))
        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"data": {"email":null, "price":337.5, "count":5}, "touched": {"email":true, "price":false}}"""
        )

        binder1.validate(mappings.options(_.ignoreEmpty(true)),
          invalidData) should be (Map("json.email" -> List("email is required")))
      }

      it("w/ i18n and label") {
        val messages1 = (key: String) => if (key == "xx") Some("haha") else Some("dummy")
        val binder1 = expandJsonString(Some("body"), Some("json")) >-: FormBinder(messages1)

        val invalidData = Map(
          "id" -> "133",
          "body" -> """{"email":"example@123.com", "price":337.5, "count":5}"""
        )

        binder1.bind(mappings.options(_.i18n(true)), invalidData) { case (id, (email, price, count)) =>
          ("invalid - shouldn't occur!") should be ("")
        } should be (Map("json" -> List("haha: 337.5 * 5 = 1687.5, too much")))
      }
    }
  }
}