package com.github.tminglei.bind

import java.util.regex.Pattern
import scala.util.matching.Regex
import org.json4s.jackson.JsonMethods
import FrameworkUtils._
import org.json4s._

trait Processors {

  ////////////////////////////////////////  pre-defined pre-processors  ////////////////////////////////

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

  ////////////////////////////////////// pre-defined bulk pre-processors ////////////////////////////

  def changePrefix(srcPrefix: String, destPrefix: String): BulkPreProcessor =
    (data: Map[String, String]) => data.map {
        case (key, value) => (key.replaceFirst("^"+srcPrefix, destPrefix), value)
      }

  def mergeJson4sData(json: JValue, destPrefix: String = "json"): BulkPreProcessor =
    (data: Map[String, String]) => {
      (data - destPrefix) ++ json4sToMapData(destPrefix, json)
    }

  def expandJsonData(sourceKey: String, destPrefix: Option[String] = None): BulkPreProcessor =
    (data: Map[String, String]) => {
      if (data.get(sourceKey).filterNot {v => (v == null || v.isEmpty)}.isDefined) {
        val prefix = destPrefix.getOrElse(sourceKey)
        val json = JsonMethods.parse(data(sourceKey))
        (data - prefix) ++ json4sToMapData(prefix, json)
      } else data
    }

  ////////////////////////////////////// pre-defined touch list extractor //////////////////////////

  def mergeJson4sTouched(json: JValue, destPrefix: String = "json"): TouchedExtractor =
    (data: Map[String, String]) => {
      touchedMapToSeq(json4sToMapData(destPrefix, json))
    }

  def expandJsonTouched(sourceKey: String, destPrefix: String): TouchedExtractor =
    (data: Map[String, String]) => {
      if (data.get(sourceKey).filterNot {v => (v == null || v.isEmpty)}.isDefined) {
        val json = JsonMethods.parse(data(sourceKey))
        touchedMapToSeq(json4sToMapData(destPrefix, json))
      } else Nil
    }

  def extractTouched(srcPrefix: String, destPrefix: String): TouchedExtractor =
    (data: Map[String, String]) => {
      val touched = data.filter {
          case (key, value) => key.startsWith(srcPrefix)
        }.map {
          case (key, value) => (key.replaceFirst("^"+srcPrefix, destPrefix), value)
        }
      touchedMapToSeq(touched)
    }

  private def touchedMapToSeq(touched: Map[String, String]): Seq[String] =
    touched.filter {
      case (key, value) => java.lang.Boolean.valueOf(value)
    }.keys.toSeq

  ////////////////////////////////////// pre-defined post err-processors ////////////////////////////
  import scala.collection.mutable.HashMap

  def errsToJson4s(useBigDecimalForDouble: Boolean = false): PostErrProcessor[JValue] =
    (errs: Seq[(String, String)]) => {
      val root = HashMap[String, Any]()
      val workList = HashMap[String, Any]("" -> root)
      var index = 0
      errs.map { case (name, err) =>
        index += 1
        val name1 = name.replaceAll("\\[", ".").replaceAll("\\]", "") //convert array format to object format
        val (parent, self, isArray) = splitName(name1 + "._errors[" + index + "]")
        val workObj = workObject(workList, parent, isArray)
        workObj += (self -> err)
      }
      mapTreeToJson4s(root, useBigDecimalForDouble)
    }
}

object Processors extends Processors