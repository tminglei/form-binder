package com.github.tminglei.bind

import org.json4s.jackson.JsonMethods
import org.scalatest._

class ProcessorsSpec extends FunSpec with ShouldMatchers {

  describe("test pre-defined pre-processors") {

    it("trim") {
      val trim = Processors.trim
      trim("", Map("" -> null)) should be (Map("" -> null))
      trim("", Map("" -> " yuu")) should be (Map("" -> "yuu"))
      trim("a", Map("a" -> "eyuu")) should be (Map("a" -> "eyuu"))
    }

    it("clean-comma") {
      val cleanComma = Processors.cleanComma
      cleanComma("", Map("" -> null)) should be (Map("" -> null))
      cleanComma("", Map("" -> "123,334")) should be (Map("" -> "123334"))
      cleanComma("a", Map("a" -> "2.345e+5")) should be (Map("a" -> "2.345e+5"))
    }

    it("clean-hyphen") {
      val cleanHyphen = Processors.cleanHyphen
      cleanHyphen("", Map("" -> null)) should be (Map("" -> null))
      cleanHyphen("", Map("" -> "2342-334")) should be (Map("" -> "2342334"))
      cleanHyphen("a", Map("a" -> "2342334")) should be (Map("a" -> "2342334"))
    }

    it("clean-prefix") {
      val cleanPrefix = Processors.cleanPrefix("$")
      cleanPrefix("", Map("" -> null)) should be (Map("" -> null))
      cleanPrefix("", Map("" -> "$3,567")) should be (Map("" -> "3,567"))
      cleanPrefix("a", Map("a" -> "35667")) should be (Map("a" -> "35667"))
    }

    it("clean-postfix") {
      val cleanPostfix = Processors.cleanPostfix("-tat")
      cleanPostfix("", Map("" -> null)) should be (Map("" -> null))
      cleanPostfix("a", Map("a" -> "tewwwtt-tat")) should be (Map("a" -> "tewwwtt"))
    }

    it("clean-redundant-spaces") {
      val cleanRedundantSpaces = Processors.cleanRedundantSpaces
      cleanRedundantSpaces("", Map("" -> null)) should be (Map("" -> null))
      cleanRedundantSpaces("a", Map("a" -> " a  teee  86y")) should be (Map("a" -> " a teee 86y"))
      cleanRedundantSpaces("", Map("" -> "te yu ")) should be (Map("" -> "te yu "))
    }

    it("clean-matched") {
      val cleanMatched = Processors.cleanMatched("-\\d\\d$".r)
      cleanMatched("", Map("" -> null)) should be (Map("" -> null))
      cleanMatched("", Map("" -> "2342-334-12")) should be (Map("" -> "2342-334"))
      cleanMatched("a", Map("a" -> "2342-334")) should be (Map("a" -> "2342-334"))
    }

    it("clean-matched-with-replacement") {
      val cleanMatched = Processors.cleanMatched("-\\d\\d$".r, "-1")
      cleanMatched("", Map("" -> null)) should be (Map("" -> null))
      cleanMatched("", Map("" -> "2342-334-12")) should be (Map("" -> "2342-334-1"))
      cleanMatched("a", Map("a" -> "2342-334")) should be (Map("a" -> "2342-334"))
    }
  }

  describe("test pre-defined bulk pre-processors") {

    describe("changePrefix") {

      it("simple") {
        val changePrefix = Processors.changePrefix("json", "data")
        val data = Map(
          "aa" -> "wett",
          "json.id" -> "123",
          "json.name" -> "tewd",
          "json.dr-1[0]" -> "33",
          "json.dr-1[1]" -> "45"
        )
        val expected = Map(
          "aa" -> "wett",
          "data.id" -> "123",
          "data.name" -> "tewd",
          "data.dr-1[0]" -> "33",
          "data.dr-1[1]" -> "45"
        )

        changePrefix("", data) should be (expected)
      }
    }

    describe("expandJsonData") {

      it("simple") {
        val expandJsonData = Processors.expandJson(Some("json"))
        val data = Map(
          "aa" -> "wett",
          "json" -> """{"id":123, "name":"tewd", "dr-1":[33,45]}"""
        )
        val expected = Map(
          "aa" -> "wett",
          "json.id" -> "123",
          "json.name" -> "tewd",
          "json.dr-1[0]" -> "33",
          "json.dr-1[1]" -> "45"
        )

        expandJsonData("", data) should be (expected)
      }

      it("null or empty") {
        val expandJsonData = Processors.expandJson(Some("json"))

        val nullData = Map("aa" -> "wett")
        expandJsonData("", nullData) should be (nullData)

        val nullData1 = Map("aa" -> "wett", "json" -> null)
        expandJsonData("", nullData1) should be (nullData1)

        val emptyData1 = Map("aa" -> "wett", "json" -> "")
        expandJsonData("", emptyData1) should be (emptyData1)
      }

      it("with dest prefix") {
        val expandJsonData = Processors.expandJson(Some("body"), Some("json"))
        val data = Map(
          "aa" -> "wett",
          "body" -> """{"id":123, "name":"tewd", "dr-1":[33,45]}"""
        )
        val expected = Map(
          "aa" -> "wett",
          "body" -> """{"id":123, "name":"tewd", "dr-1":[33,45]}""",
          "json.id" -> "123",
          "json.name" -> "tewd",
          "json.dr-1[0]" -> "33",
          "json.dr-1[1]" -> "45"
        )

        expandJsonData("", data) should be (expected)
      }
    }
  }

  describe("test pre-defined touched list extractors") {

    describe("expandJsonTouched") {

      it("simple") {
        val expandJsonTouched = Processors.expandJsonTouched("touched", "data")
        val touched = Map("touched" -> """{"email":true, "price":false}""")
        val expected = List("data.email")

        expandJsonTouched(touched).toList should be (expected)
      }
    }

    describe("extractTouched") {
      val extractTouched = Processors.extractTouched("json.touched", "data")
      val data = Map(
        "aa" -> "wett",
        "json.touched.id" -> "true",
        "json.touched.name" -> "true",
        "json.touched.email" -> "false"
      )
      val expected = List("data.id", "data.name")

      extractTouched(data).toList should be (expected)
    }
  }

  describe("test pre-defined post err-processors") {

    describe("errsToJson4s") {

      it("simple") {
        val errs = Seq(
          "" -> "top error1",
          "aa" -> "error aa",
          "aa.ty" -> "error aa.ty",
          "aa" -> "error aa 1",
          "aa.tl[3]" -> "ewty",
          "aa.tl[3]" -> "ewyu7",
          "br-1[t0]" -> "key: eeor",
          "br-1[t0]" -> "tert",
          "br-1[1]" -> "tetty",
          "" -> "top error2"
        )

        val expected = JsonMethods.parse(
          """
            {
              "_errors": ["top error1", "top error2"],
              "br-1": {
                "t0": {
                  "_errors": ["key: eeor", "tert"]
                },
                "1": {
                  "_errors": ["tetty"]
                }
              },
              "aa": {
                "ty": {
                  "_errors": ["error aa.ty"]
                },
                "tl": {
                  "3": {
                    "_errors": ["ewty", "ewyu7"]
                  }
                },
                "_errors": ["error aa", "error aa 1"]
              }
            }
          """)

        Processors.errsToJson4s()(errs) should be (expected)
      }
    }
  }
}
