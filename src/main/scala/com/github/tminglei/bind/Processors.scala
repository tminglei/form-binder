package com.github.tminglei.bind

import java.util.regex.Pattern
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import org.json4s.jackson.JsonMethods
import org.json4s._

trait Processors {
  import FrameworkUtils._

  private val logger = LoggerFactory.getLogger(Processors.getClass)

  ////////////////////////////////////  pre-defined pre-processors  ////////////////////////////////

  def trim(): PreProcessor =
    (prefix, data, options) => {
      logger.debug(s"trimming '$prefix'")
      data.map { case (k, v) =>
        if (!k.startsWith(prefix)) (k, v)
        else (k, Option(v).map(_.trim).orNull)
      }
    }

  def omit(str: String): PreProcessor = omitMatched(Pattern.quote(str).r, "")

  def omitLeft(str: String): PreProcessor = omitMatched(("^"+Pattern.quote(str)).r, "")

  def omitRight(str: String): PreProcessor = omitMatched((Pattern.quote(str)+"$").r, "")

  def omitRedundant(str: String): PreProcessor = omitMatched(("["+Pattern.quote(str)+"]+").r, str)

  def omitMatched(regex: Regex, replacement: String = ""): PreProcessor =
    (prefix, data, options) => {
      logger.debug(s"replacing '$regex' with '$replacement'")
      data.map { case (k, v) =>
        if (!k.startsWith(prefix)) (k, v)
        else (k, Option(v).map(regex.replaceAllIn(_, replacement)).orNull)
      }
    }

  def changePrefix(from: String, to: String): PreProcessor =
    (prefix, data, options) => {
      logger.debug(s"changing prefix at '$prefix' from '$from' to '$to'")
      data.map { case (k, v) =>
        if (!k.startsWith(prefix)) (k, v)
        else {
          val tail = k.substring(prefix.length).replaceFirst("^[\\.]?" + Pattern.quote(from), to)
            .replaceFirst("^\\.", "")
          val newKey = if (isEmptyStr(tail)) prefix else (prefix + "." + tail)
            .replaceFirst("^\\.", "")
          (newKey, v)
        }
      }
    }

  def expandJson(prefix: Option[String] = None): PreProcessor =
    (prefix1, data, options) => {
      logger.debug(s"expanding json at '${prefix.getOrElse(prefix1)}'")
      val thePrefix = prefix.getOrElse(prefix1)
      if (!isEmptyStr(data.get(thePrefix).orNull)) {
        val json = JsonMethods.parse(data(thePrefix))
        (data - thePrefix) ++ json2map(thePrefix, json)
      } else data
    }
  
  def expandJsonKeys(prefix: Option[String] = None): PreProcessor =
    (prefix1, data, options) => {
      logger.debug(s"expanding json keys at '${prefix.getOrElse(prefix1)}'")
      val data1 = expandJson(prefix).apply(prefix1, data, options)
      val data2 = expandListKeys(prefix).apply(prefix1, data1, options)
      data2
    }

  def expandListKeys(prefix: Option[String] = None): PreProcessor =
    (prefix1, data, options) => {
      logger.debug(s"expanding list keys at '${prefix.getOrElse(prefix1)}'")
      val thePrefix = prefix.getOrElse(prefix1)
      val p = Pattern.compile("^" + Pattern.quote(thePrefix) + "\\[[\\d]+\\].*")
      data.map { case (k, v) =>
        if (p.matcher(k).matches()) {
          val newKey = if (isEmptyStr(thePrefix)) v else thePrefix + "." + v
          (newKey, "true")
        } else (k, v)
      }
    }

  //////////////////////////////////// pre-defined post err-processors /////////////////////////////
  import scala.collection.mutable.HashMap

  def foldErrs(): ErrProcessor[Map[String, List[String]]] =
    (errors: Seq[(String, String)]) => {
      logger.debug("folding errors")
      Map.empty ++ errors.groupBy(_._1).map {
        case (key, pairs) => (key, pairs.map(_._2).toList)
      }
    }

  def errsTree(): ErrProcessor[Map[String, Any]] =
    (errors: Seq[(String, String)]) => {
      logger.debug("converting errors list to errors tree")

      val root = HashMap[String, Any]()
      val workList = HashMap[String, Any]("" -> root)
      errors.map { case (name, error) =>
        val name1 = name.replaceAll("\\[", ".").replaceAll("\\]", "")
        val workObj = workObject(workList, name1 + "._errors", true)
          .asInstanceOf[ListBuffer[String]]
        workObj += (error)
      }
      root.toMap
    }

  //////////////////////////////////// pre-defined touched checkers ////////////////////////////////

  def listTouched(touched: List[String]): TouchedChecker =
    (prefix, data) => {
      logger.debug(s"checking touched in list for '$prefix'")
      touched.find(_.startsWith(prefix)).isDefined
    }

  def prefixTouched(dataPrefix: String, touchedPrefix: String): TouchedChecker =
    (prefix, data) => {
      logger.debug(s"checking touched with data prefix '$dataPrefix' and touched prefix '$touchedPrefix' for '$prefix'")
      val prefixBeChecked = prefix.replaceAll("^" + Pattern.quote(dataPrefix), touchedPrefix)
      data.keys.find(_.startsWith(prefixBeChecked)).isDefined
    }
}

object Processors extends Processors