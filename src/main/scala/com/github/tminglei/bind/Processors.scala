package com.github.tminglei.bind

import java.util.regex.Pattern
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

trait Processors {
  import FrameworkUtils._

  private val logger = LoggerFactory.getLogger(Processors.getClass)

  ////////////////////////////////////  pre-defined pre-processors  ////////////////////////////////

  def trim(): PreProcessor with Metable[ExtensionMeta] =
    mkPreProcessorWithMeta((prefix, data, options) => {
      logger.debug(s"trimming '$prefix'")
      data.map { case (k, v) =>
        if (!k.startsWith(prefix)) (k, v)
        else (k, Option(v).map(_.trim).orNull)
      }
    }, meta = mkExtensionMeta("trim"))

  def omit(str: String) = replacing(Pattern.quote(str).r, "", mkExtensionMeta("omit", str))

  def omitLeft(str: String) = replacing(("^"+Pattern.quote(str)).r, "", mkExtensionMeta("omitLeft", str))

  def omitRight(str: String) = replacing((Pattern.quote(str)+"$").r, "", mkExtensionMeta("omitRight", str))

  def omitRedundant(str: String) = replacing(("["+Pattern.quote(str)+"]+").r, str, mkExtensionMeta("omitRedundant", str))

  def omitMatched(regex: Regex, replacement: String = "") = replacing(regex, replacement, mkExtensionMeta("omitMatched", regex, replacement))

  protected def replacing(regex: Regex, replacement: String, meta: ExtensionMeta): PreProcessor with Metable[ExtensionMeta] =
    mkPreProcessorWithMeta((prefix, data, options) => {
      logger.debug(s"replacing '$regex' with '$replacement'")
      data.map { case (k, v) =>
        if (!k.startsWith(prefix)) (k, v)
        else (k, Option(v).map(regex.replaceAllIn(_, replacement)).orNull)
      }
    }, meta = meta)

  def changePrefix(from: String, to: String): PreProcessor with Metable[ExtensionMeta] =
    mkPreProcessorWithMeta((prefix, data, options) => {
      logger.debug(s"changing prefix from '$from' to '$to' at '$prefix'")
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
    }, meta = ExtensionMeta("changePrefix", s"changePrefix(from '$from' to '$to')", List(from, to)))

  def expandJson(prefix: Option[String] = None): PreProcessor with Metable[ExtensionMeta] =
    mkPreProcessorWithMeta((prefix1, data, options) => {
      logger.debug(s"expanding json at '${prefix.getOrElse(prefix1)}'")
      val thePrefix = prefix.getOrElse(prefix1)
      if (!isEmptyStr(data.get(thePrefix).orNull)) {
        val json = spray.json.JsonParser(data(thePrefix))
        (data - thePrefix) ++ json2map(thePrefix, json)
      } else data
    }, meta = mkExtensionMeta("expandJson", prefix))
  
  def expandJsonKeys(prefix: Option[String] = None): PreProcessor with Metable[ExtensionMeta] =
    mkPreProcessorWithMeta((prefix1, data, options) => {
      logger.debug(s"expanding json keys at '${prefix.getOrElse(prefix1)}'")
      val data1 = expandJson(prefix).apply(prefix1, data, options)
      val data2 = expandListKeys(prefix).apply(prefix1, data1, options)
      data2
    }, meta = mkExtensionMeta("expandJsonKeys", prefix))

  def expandListKeys(prefix: Option[String] = None): PreProcessor with Metable[ExtensionMeta] =
    mkPreProcessorWithMeta((prefix1, data, options) => {
      logger.debug(s"expanding list keys at '${prefix.getOrElse(prefix1)}'")
      val thePrefix = prefix.getOrElse(prefix1)
      val p = Pattern.compile("^" + Pattern.quote(thePrefix) + "\\[[\\d]+\\].*")
      data.map { case (k, v) =>
        if (p.matcher(k).matches()) {
          val newKey = if (isEmptyStr(thePrefix)) v else thePrefix + "." + v
          (newKey, "true")
        } else (k, v)
      }
    }, meta = mkExtensionMeta("expandListKeys", prefix))

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
