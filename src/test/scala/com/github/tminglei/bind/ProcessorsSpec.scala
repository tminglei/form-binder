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
}
