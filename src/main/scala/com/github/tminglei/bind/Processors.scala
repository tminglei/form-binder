package com.github.tminglei.bind

import java.util.regex.Pattern
import scala.util.matching.Regex
import org.json4s.JsonAST._
import org.json4s.jackson.JsonMethods

trait Processors {

  //////////////////////////////////////  pre-defined pre-processor implementations  ///////////////////////////

  val trim: PreProcessor = (input: String) => {
    if (input == null) null else input.trim
  }

  val cleanComma: PreProcessor = (input: String) => {
    if (input == null) null else input.replaceAll(",", "")
  }

  val cleanHyphen: PreProcessor = (input: String) => {
    if (input == null) null else input.replaceAll("-", "")
  }

  def cleanPrefix(prefix: String): PreProcessor = (input: String) => {
    if (input == null) null else input.replaceAll("^"+Pattern.quote(prefix), "")
  }

  def cleanPostfix(postfix: String): PreProcessor = (input: String) => {
    if (input == null) null else input.replaceAll(Pattern.quote(postfix)+"$", "")
  }

  val cleanRedundantSpaces: PreProcessor = (input: String) => {
    if (input == null) null else input.replaceAll("[ ]+", " ")
  }

  def cleanMatched(regex: Regex, replacement: String = ""): PreProcessor = (input: String) => {
    if (input == null) null else regex.replaceAllIn(input, replacement)
  }

  ////////////////////////////////////  pre-defined bulk pre-processor implementations  /////////////////////////

  def mergeJsonData(sourceKey: String, destPrefix: Option[String] = None): BulkPreProcessor =
    (data: Map[String, String]) => {
      val prefix = destPrefix.getOrElse(sourceKey)
      val json = JsonMethods.parse(data(sourceKey))
      data ++ mappedJsonData(prefix, json)
    }

  private def mappedJsonData(prefix: String, json: JValue): Map[String, String] = json match {
    case JObject(fields) => {
      fields.map { case (key, value) =>
        mappedJsonData(Option(prefix).filterNot(_.isEmpty).map(_ + ".").getOrElse("") + key, value)
      }.foldLeft(Map.empty[String, String])(_ ++ _)
    }
    case JArray(values) => {
      values.zipWithIndex.map { case (value, i) =>
        mappedJsonData(prefix + "[" + i + "]", value)
      }.foldLeft(Map.empty[String, String])(_ ++ _)
    }
    case JNull => Map.empty
    case JNothing => Map.empty
    case JBool(value) => Map(prefix -> value.toString)
    case JDouble(value) => Map(prefix -> value.toString)
    case JInt(value) => Map(prefix -> value.toString)
    case JString(value) => Map(prefix -> value.toString)
  }
}

object Processors extends Processors