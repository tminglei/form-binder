package com.github.tminglei.bind

import org.scalatest._

class ProcessorsSpec extends FunSpec with ShouldMatchers {

  describe("test pre-defined pre-processors") {

    it("trim") {
      val trim = Processors.trim
      trim(null) should be (null)
      trim(" yuu") should be ("yuu")
      trim("eyuu") should be ("eyuu")
    }

    it("clean-comma") {
      val cleanComma = Processors.cleanComma
      cleanComma(null) should be (null)
      cleanComma("123,334") should be ("123334")
      cleanComma("2.345e+5") should be ("2.345e+5")
    }

    it("clean-hyphen") {
      val cleanHyphen = Processors.cleanHyphen
      cleanHyphen(null) should be (null)
      cleanHyphen("2342-334") should be ("2342334")
      cleanHyphen("2342334") should be ("2342334")
    }

    it("clean-prefix") {
      val cleanPrefix = Processors.cleanPrefix("$")
      cleanPrefix(null) should be (null)
      cleanPrefix("$3,567") should be ("3,567")
      cleanPrefix("35667") should be ("35667")
    }

    it("clean-postfix") {
      val cleanPostfix = Processors.cleanPostfix("-tat")
      cleanPostfix(null) should be (null)
      cleanPostfix("tewwwtt-tat") should be ("tewwwtt")
    }

    it("clean-redundant-spaces") {
      val cleanRedundantSpaces = Processors.cleanRedundantSpaces
      cleanRedundantSpaces(null) should be (null)
      cleanRedundantSpaces(" a  teee  86y") should be (" a teee 86y")
      cleanRedundantSpaces("te yu ") should be ("te yu ")
    }

    it("clean-matched") {
      val cleanMatched = Processors.cleanMatched("-\\d\\d$".r)
      cleanMatched(null) should be (null)
      cleanMatched("2342-334-12") should be ("2342-334")
      cleanMatched("2342-334") should be ("2342-334")
    }

    it("clean-matched-with-replacement") {
      val cleanMatched = Processors.cleanMatched("-\\d\\d$".r, "-1")
      cleanMatched(null) should be (null)
      cleanMatched("2342-334-12") should be ("2342-334-1")
      cleanMatched("2342-334") should be ("2342-334")
    }
  }

  describe("test pre-defined bulk pre-processors") {

    describe("expandJsonData") {

      it("simple") {
        val expandJsonData = Processors.expandJsonData("json")
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

        expandJsonData(data) should be (expected)
      }

      it("null or empty") {
        val expandJsonData = Processors.expandJsonData("json")

        val nullData = Map("aa" -> "wett")
        expandJsonData(nullData) should be (nullData)

        val nullData1 = Map("aa" -> "wett", "json" -> null)
        expandJsonData(nullData1) should be (nullData1)

        val emptyData1 = Map("aa" -> "wett", "json" -> "")
        expandJsonData(emptyData1) should be (emptyData1)
      }

      it("with dest prefix") {
        val expandJsonData = Processors.expandJsonData("body", Some("json"))
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

        expandJsonData(data) should be (expected)
      }
    }
  }
}
