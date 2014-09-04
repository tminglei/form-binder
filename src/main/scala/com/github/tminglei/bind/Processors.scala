package com.github.tminglei.bind

import java.util.regex.Pattern
import scala.util.matching.Regex
import org.json4s.JsonAST._
import org.json4s.jackson.JsonMethods

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

  //////////////////////////////////////  pre-defined bulk pre-processors  ////////////////////////////

  def expandJsonData(sourceKey: String, destPrefix: Option[String] = None): BulkPreProcessor =
    (data: Map[String, String]) => {
      if (data.get(sourceKey).filterNot {v => (v == null || v.isEmpty)}.isDefined) {
        val prefix = destPrefix.getOrElse(sourceKey)
        val json = JsonMethods.parse(data(sourceKey))
        if (prefix == sourceKey) (data - sourceKey) ++ mappedJsonData(prefix, json)
        else data ++ mappedJsonData(prefix, json)
      } else data
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
    case JDecimal(value) => Map(prefix -> value.toString)
    case JInt(value) => Map(prefix -> value.toString)
    case JString(value) => Map(prefix -> value.toString)
  }

  ////////////////////////////////////// pre-defined post err-processors ////////////////////////////
  import scala.collection.mutable.HashMap

  val errsToJson4s: PostErrProcessor[JValue] = (errs: Seq[(String, String)]) => {
    def treeMapToJson(tree: HashMap[String, Any]): JValue = JObject(tree.map {
      case (name, obj: HashMap[String, Any]) => JField(name, treeMapToJson(obj))
      case (name, arr: List[String]) => JField(name, JArray(arr.map(JString(_))))
      case (name, any) => sys.error(s"unsupported tuple ($name, $any)")
    }.toSeq: _*)
    ///
    treeMapToJson(errsToTreeMap(errs))
  }

  protected def errsToTreeMap(errs: Seq[(String, String)]): HashMap[String, Any] = {
    def workObject(workList: HashMap[String, Any], name: String): HashMap[String, Any] =
      workList.get(name) match {
        case Some(mapObj) => mapObj.asInstanceOf[HashMap[String, Any]]
        case None => {
          val (parent, self) = splitNames(name)
          val parentObj = workObject(workList, parent)
          val theObj = HashMap[String, Any]()
          parentObj += (self -> theObj)
          workList  += (name -> theObj)
          theObj
        }
      }

    val root = HashMap[String, Any]()
    val workList = HashMap[String, Any]("" -> root)
    errs.map { case (name, err) =>
      val workObj = workObject(workList, name)
      if (workObj.get("_errors").isEmpty) workObj += ("_errors" -> Nil)
      workObj("_errors") = workObj("_errors").asInstanceOf[List[String]] :+ err
    }
    root
  }

  private val OBJECT_ELEMENT = "^(.*)\\.([^\\.]*)$".r
  private val ARRAY_ELEMENT  = "^(.*)\\[([^\\.\\]]*)\\]$".r
  private def splitNames(name: String): (String, String) = name match {
    case ARRAY_ELEMENT (name, index)  => (name, index)
    case OBJECT_ELEMENT(parent, name) => (parent, name)
    case _                            => ("", name)
  }
}

object Processors extends Processors