package com.github.tminglei.bind

import org.scalatest._
import scala.collection.mutable.ListBuffer

class ProcessorsSpec extends FunSpec with ShouldMatchers {

  describe("test pre-defined pre-processors") {

    it("trim") {
      val trim = Processors.trim
      trim("", Map("" -> null), Options.apply()) should be (Map("" -> null))
      trim("", Map("" -> " yuu"), Options.apply()) should be (Map("" -> "yuu"))
      trim("a", Map("a" -> "eyuu"), Options.apply()) should be (Map("a" -> "eyuu"))
    }

    it("omit") {
      val omit = Processors.omit(",")
      omit("", Map("" -> null), Options.apply()) should be (Map("" -> null))
      omit("", Map("" -> "123,334"), Options.apply()) should be (Map("" -> "123334"))
      omit("a", Map("a" -> "2.345e+5"), Options.apply()) should be (Map("a" -> "2.345e+5"))
    }

    it("omit-left") {
      val omitLeft = Processors.omitLeft("$")
      omitLeft("", Map("" -> null), Options.apply()) should be (Map("" -> null))
      omitLeft("", Map("" -> "$3,567"), Options.apply()) should be (Map("" -> "3,567"))
      omitLeft("a", Map("a" -> "35667"), Options.apply()) should be (Map("a" -> "35667"))
    }

    it("omit-right") {
      val omitRight = Processors.omitRight("-tat")
      omitRight("", Map("" -> null), Options.apply()) should be (Map("" -> null))
      omitRight("a", Map("a" -> "tewwwtt-tat"), Options.apply()) should be (Map("a" -> "tewwwtt"))
    }

    it("omit-redundant") {
      val cleanRedundant = Processors.omitRedundant(" ")
      cleanRedundant("", Map("" -> null), Options.apply()) should be (Map("" -> null))
      cleanRedundant("a", Map("a" -> " a  teee  86y"), Options.apply()) should be (Map("a" -> " a teee 86y"))
      cleanRedundant("", Map("" -> "te yu "), Options.apply()) should be (Map("" -> "te yu "))
    }

    it("omit-matched") {
      val omitMatched = Processors.omitMatched("-\\d\\d$".r)
      omitMatched("", Map("" -> null), Options.apply()) should be (Map("" -> null))
      omitMatched("", Map("" -> "2342-334-12"), Options.apply()) should be (Map("" -> "2342-334"))
      omitMatched("a", Map("a" -> "2342-334"), Options.apply()) should be (Map("a" -> "2342-334"))
    }

    it("omit-matched w/ replacement") {
      val omitMatched = Processors.omitMatched("-\\d\\d$".r, "-1")
      omitMatched("", Map("" -> null), Options.apply()) should be (Map("" -> null))
      omitMatched("", Map("" -> "2342-334-12"), Options.apply()) should be (Map("" -> "2342-334-1"))
      omitMatched("a", Map("a" -> "2342-334"), Options.apply()) should be (Map("a" -> "2342-334"))
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

        changePrefix("", data, Options.apply()) should be (expected)
      }
    }

    describe("expandJson") {

      it("simple") {
        val expandJson = Processors.expandJson(Some("json"))
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

        expandJson("", data, Options.apply()) should be (expected)
      }

      it("null or empty") {
        val expandJsonData = Processors.expandJson(Some("json"))

        val nullData = Map("aa" -> "wett")
        expandJsonData("", nullData, Options.apply()) should be (nullData)

        val nullData1 = Map("aa" -> "wett", "json" -> null)
        expandJsonData("", nullData1, Options.apply()) should be (nullData1)

        val emptyData1 = Map("aa" -> "wett", "json" -> "")
        expandJsonData("", emptyData1, Options.apply()) should be (emptyData1)
      }

      it("with dest prefix") {
        val expandJson = Processors.expandJson(Some("body"))
        val data = Map(
          "aa" -> "wett",
          "body" -> """{"id":123, "name":"tewd", "dr-1":[33,45]}"""
        )
        val expected = Map(
          "aa" -> "wett",
          "body.id" -> "123",
          "body.name" -> "tewd",
          "body.dr-1[0]" -> "33",
          "body.dr-1[1]" -> "45"
        )

        expandJson("", data, Options.apply()) should be (expected)
      }
    }
  }

  describe("test pre-defined post err-processors") {

    describe("foldErrs") {
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

        val expected = Map(
          "" -> List("top error1", "top error2"),
          "aa" -> List("error aa", "error aa 1"),
          "aa.ty" -> List("error aa.ty"),
          "aa.tl[3]" -> List("ewty", "ewyu7"),
          "br-1[t0]" -> List("key: eeor", "tert"),
          "br-1[1]" -> List("tetty")
        )

        Processors.foldErrs()(errs) should be (expected)
      }
    }

    describe("errsTree") {

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

        val expected = Map(
          "_errors" -> ListBuffer("top error1", "top error2"),
          "br-1" -> Map(
            "t0" -> Map(
              "_errors" -> ListBuffer("key: eeor", "tert")
            ),
            "1" -> Map(
              "_errors" -> ListBuffer("tetty")
            )
          ),
          "aa" -> Map(
            "ty" -> Map(
              "_errors" -> ListBuffer("error aa.ty")
            ),
            "tl" -> Map(
              "3" -> Map(
                "_errors" -> ListBuffer("ewty", "ewyu7")
              )
            ),
            "_errors" -> ListBuffer("error aa", "error aa 1")
          ))

        Processors.errsTree()(errs) should be (expected)
      }
    }
  }
}
