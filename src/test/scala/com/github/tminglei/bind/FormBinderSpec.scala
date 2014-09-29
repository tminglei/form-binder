package com.github.tminglei.bind

import org.json4s.JsonAST.{JString, JArray, JObject}
import org.json4s.jackson.JsonMethods
import org.scalatest._

class FormBinderSpec extends FunSpec with ShouldMatchers {
  import com.github.tminglei.bind.simple._

  describe("show and check form binder") {

    it("usage case") {
      val messages = (key: String) => "dummy"
      val binder = expandJsonData("body", Some("json")) pipe_: FormBinder(messages)
      val binder1 = expandJsonData("body", Some("json")) pipe_: FormBinder(messages).withErr(errsToJson4s)

      val mappings = tmapping(
        "id" -> long(),
        "json" -> tmapping(
          "price" -> (cleanPrefix("$") pipe_: float()),
          "count" -> number().verifying(min(3), max(10))
        ).label("xx").verifying { case (label, (price, count), messages) =>
          if (price * count > 1000) {
            Seq("" -> s"$label: $price * $count = ${price * count}, too much")
          } else Nil
        }
      )

      /// valid data
      val data = Map(
        "id" -> "133",
        "body" -> """{"price":"$137.5", "count":5}"""
      )
      binder.bind(mappings, data) { case (id, (price, count)) =>
        id should be (133L)
        price should be (137.5f)
        count should be (5)
        (">> bind successful!")
      } should be (">> bind successful!")

      /// invalid data
      val data1 = Map(
        "id" -> "133",
        "body" -> """{"price":337.5, "count":5}"""
      )
      binder.bind(mappings, data1) { case (id, (price, count)) =>
        ("invalid - shouldn't occur!") should be ("")
      } should be (Map("json" -> List("xx: 337.5 * 5 = 1687.5, too much")))

      binder1.bind(mappings, data1) { case (id, (price, count)) =>
        ("invalid - shouldn't occur!") should be ("")
      } should be (JsonMethods.parse(
        """
          {
            "json": {
              "_errors": ["xx: 337.5 * 5 = 1687.5, too much"]
            }
          }
        """))
    }
  }
}