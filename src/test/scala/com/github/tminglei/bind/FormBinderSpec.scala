package com.github.tminglei.bind

import org.scalatest._

class FormBinderSpec extends FunSpec with ShouldMatchers {
  import com.github.tminglei.bind.simple._

  describe("show and check form binder") {
    it("usage case") {
      val messages = (key: String) => "dummy"
      val binder = expandJsonData("body") pipe_: FormBinder(messages)

      val mappings = tmapping(
        "id" -> long(),
        "body" -> tmapping(
          "price" -> (cleanPrefix("$") pipe_: float()),
          "count" -> number().verifying(min(3), max(10))
        ).verifying { case ((price, count), messages) =>
          if (price * count > 1000) {
            Seq("" -> s"$price * $count = ${price * count}: too much")
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
      } should be (Seq("body" -> "337.5 * 5 = 1687.5: too much"))
    }
  }
}