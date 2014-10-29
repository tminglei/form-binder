package com.github.tminglei.bind

import java.util.regex.Pattern
import scala.util.matching.Regex
import org.json4s.jackson.JsonMethods
import FrameworkUtils._
import org.json4s._

trait Processors {
  import FrameworkUtils.mkPreProcessor
  ////////////////////////////////////  pre-defined pre-processors  ////////////////////////////////

  def trim(): PreProcessor = mkPreProcessor {(input: String) =>
    if (input == null) null else input.trim
  }

  def cleanComma(): PreProcessor = mkPreProcessor {(input: String) =>
    if (input == null) null else input.replaceAll(",", "")
  }

  def cleanHyphen(): PreProcessor = mkPreProcessor {(input: String) =>
    if (input == null) null else input.replaceAll("-", "")
  }

  def cleanPrefix(prefix: String): PreProcessor = mkPreProcessor {(input: String) =>
    if (input == null) null else input.replaceAll("^"+Pattern.quote(prefix), "")
  }

  def cleanPostfix(postfix: String): PreProcessor = mkPreProcessor {(input: String) =>
    if (input == null) null else input.replaceAll(Pattern.quote(postfix)+"$", "")
  }

  def cleanRedundantSpaces(): PreProcessor = mkPreProcessor {(input: String) =>
    if (input == null) null else input.replaceAll("[ ]+", " ")
  }

  def cleanMatched(regex: Regex, replacement: String = ""): PreProcessor = mkPreProcessor {(input: String) =>
    if (input == null) null else regex.replaceAllIn(input, replacement)
  }

  def changePrefix(srcPrefix: String, destPrefix: String): PreProcessor =
    (prefix: String, data: Map[String, String]) => data.map {
        case (key, value) => (key.replaceFirst("^"+srcPrefix, destPrefix), value)
      }

  def mergeJson4sData(json: JValue, destPrefix: String = "json"): PreProcessor =
    (prefix: String, data: Map[String, String]) => {
      (data - destPrefix) ++ json4sToMapData(destPrefix, json)
    }

  def expandJsonString(sourceKey: Option[String] = None, destPrefix: Option[String] = None): PreProcessor =
    (prefix: String, data: Map[String, String]) => {
      val sourceKey1 = sourceKey.getOrElse(prefix)
      if (data.get(sourceKey1).filterNot {v => (v == null || v.isEmpty)}.isDefined) {
        val destPrefix1 = destPrefix.getOrElse(sourceKey1)
        val json = JsonMethods.parse(data(sourceKey1))
        (data - destPrefix1) ++ json4sToMapData(destPrefix1, json)
      } else data
    }

  ////////////////////////////////// pre-defined touch list extractor //////////////////////////////

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

  //////////////////////////////////// pre-defined post err-processors /////////////////////////////
  import scala.collection.mutable.HashMap

  def errsToMapList(): PostErrProcessor[Map[String, List[String]]] =
    (errors: Seq[(String, String)]) => { Map.empty ++
      errors.groupBy(_._1).map {
        case (key, pairs) => (key, pairs.map(_._2).toList)
      }
    }

  def errsToJson4s(): PostErrProcessor[JValue] =
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
      mapTreeToJson4s(root)
    }
}

object Processors extends Processors