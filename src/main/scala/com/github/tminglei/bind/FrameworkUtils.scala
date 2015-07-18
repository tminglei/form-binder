package com.github.tminglei.bind

import java.util.regex.Pattern
import scala.collection.mutable.{ListBuffer, HashMap}
import org.json4s._

/**
 * Framework internal used util methods (!!!NOTE: be careful if using it externally)
 */
object FrameworkUtils {
  val ILLEGAL_ARRAY_INDEX = """.*\[(\d*[^\d\[\]]+\d*)+\].*""".r
  /** copied from Play! form/mapping */
  val EMAIL_REGEX = """^(?!\.)("([^"\r\\]|\\["\r\\])*"|([-a-zA-Z0-9!#$%&'*+/=?^_`{|}~]|(?<!\.)\.)*)(?<!\.)@[a-zA-Z0-9][\w\.-]*[a-zA-Z0-9]\.[a-zA-Z][a-zA-Z\.]*[a-zA-Z]$""".r

  //////////////////////////////////////////////////////////////////////////////////
  val PassValidating: Constraint = (name, data, messages, parentOptions) => Nil

  def isEmptyStr(str: String): Boolean =
    str == null || (str.trim == "") || str.equalsIgnoreCase("null")

  def isEmptyInput(name: String, data: Map[String, String], inputMode: InputMode): Boolean = {
    val prefix1 = if (isEmptyStr(name)) "" else name + "."
    val prefix2 = if (isEmptyStr(name)) "" else name + "["

    def subInput(kv: (String, String)) = kv match {
      case (k, v) => (k.startsWith(prefix1) || k.startsWith(prefix2)) && k.length > name.length
    }

    inputMode match {
      case SoloInput => data.get(name).filterNot(isEmptyStr).isEmpty
      case BulkInput => data.find(subInput).isEmpty
      case _ => data.get(name).filterNot(isEmptyStr).isEmpty && data.find(subInput).isEmpty
    }
  }

  // split a dot separated path name to parent part and self part,
  // with indicating whether it's an array path
  private val OBJECT_ELEM_NAME = "^(.*)\\.([^\\.]+)$".r
  private val ARRAY_ELEM_NAME  = "^(.*)\\[([\\d]+)\\]$".r
  def splitName(name: String): (String, String, Boolean) = name match {
    case ARRAY_ELEM_NAME (name, index)  => (name, index, true)
    case OBJECT_ELEM_NAME(parent, name) => (parent, name, false)
    case _  => ("", name, false)
  }

  //-----------------------------------------------------------------------------
  // make an internal converter from `(vString) => value`
  def mkSimpleConverter[T](convert: String => T) =
    (name: String, data: Map[String, String]) => {
      convert(data.get(name).orNull)
    }
  
  // make a constraint from `(label, vString, messages) => [error]` (ps: vString may be NULL/EMPTY)
  def mkSimpleConstraint(validate: (String, String, Messages) => Option[String]): Constraint =
    (name, data, messages, options) => {
      if (options._inputMode != SoloInput) {
        throw new IllegalArgumentException("The constraint should only be used to SINGLE INPUT mapping!")
      } else {
        validate(getLabel(name, messages, options), data.get(name).orNull, messages)
          .map { error => Seq(name -> error) }.getOrElse(Nil)
      }
    }

  @scala.annotation.tailrec
  def processDataRec(prefix: String, data: Map[String,String], options: Options,
            processors: List[PreProcessor]): Map[String,String] =
    processors match {
      case (process :: rest) => {
        val newData = process(prefix, data, options)
        processDataRec(prefix, newData, options, rest)
      }
      case _  => data
    }

  def validateRec(name: String, data: Map[String, String], messages: Messages, options: Options,
            validates: List[Constraint]): Seq[(String, String)] = validates match {
        case (validate :: rest) => {
          val errors = validate(name, data, messages, options)
          if (errors.isEmpty) validateRec(name, data, messages, options, rest)
          else errors ++ (if (options.eagerCheck.getOrElse(false))
            validateRec(name, data, messages, options, rest) else Nil)
        }
        case _ => Nil
      }

  def extraValidateRec[T](name: String, value: => T, messages: Messages, options: Options,
            validates: List[ExtraConstraint[T]]): Seq[(String, String)] =
    validates match {
      case (validate :: rest) => validate(getLabel(name, messages, options), value, messages) match {
        case Nil    => extraValidateRec(name, value, messages, options, rest)
        case errors => errors.map { case (message) => (name, message) } ++ (
          if (options.eagerCheck.getOrElse(false))
            extraValidateRec(name, value, messages, options, rest)
          else Nil)
      }
      case _ => Nil
    }

  //////////////////////////// normal processing related //////////////////////////////

  // i18n on: use i18n label, if exists; else use label; else use last field name from full name
  // i18n off: use label; else use last field name from full name
  def getLabel(fullName: String, messages: Messages, options: Options): String = {
    val (parent, name, isArray) = splitName(fullName)
    val default = if (isArray) (splitName(parent)._2 + "[" + name + "]") else name
    if (options.i18n.getOrElse(false)) {
      options._label.flatMap(messages(_).orElse(options._label)).getOrElse(default)
    } else options._label.getOrElse(default)
  }

  // make a Constraint which will try to check and collect errors
  def checking[T](check: String => T, messageOrKey: Either[String, String], extraMessageArgs: String*): Constraint =
    mkSimpleConstraint((label, value, messages) => value match {
      case null|"" => None
      case x => {
        try { check(x); None }
        catch {
          case e: Exception => {
            val msgTemplate = messageOrKey.fold(s => s, messages(_).get);
            Some(msgTemplate.format((value +: extraMessageArgs): _*))
          }
        }
      }
    })

  // make a compound Constraint, which checks whether any inputting constraints passed
  def anyPassed(constraints: Constraint*): Constraint =
    (name, data, messages, options) => {
      var errErrors: List[(String, String)] = Nil
      val found = constraints.find(c => {
        val errors = c.apply(name, data, messages, options)
        errErrors ++= errors
        errors.isEmpty
      })
      if (found.isDefined) Nil
      else {
        val label = getLabel(name, messages, options)
        val errStr = errErrors.map(_._2).mkString("[", ", ", "]")
        Seq(name -> messages("error.anypassed").get.format(label, errStr))
      }
    }

  // Computes the available indexes for the given key in this set of data.
  def indexes(key: String, data: Map[String, String]): Seq[Int] = {
    val KeyPattern = ("^" + Pattern.quote(key) + """\[(\d+)\].*$""").r
    data.toSeq.collect { case (KeyPattern(index), _) => index.toInt }.sorted.distinct
  }

  // Computes the available keys for the given prefix in this set of data.
  def keys(prefix: String, data: Map[String, String]): Seq[String] = {
    val KeyPattern = ("^" + Pattern.quote(prefix) + """\.("?[^."]+"?).*$""").r
    data.toSeq.collect { case (KeyPattern(key), _) => key }.distinct
  }
  
  /////////////////////////// json processing related /////////////////////////////////

  def json2map(prefix: String, json: JValue): Map[String, String] =
    json match {
      case JArray(values) => values.zipWithIndex.map {
          case (value, i) => json2map(prefix + "[" + i + "]", value)
        }.foldLeft(Map.empty[String, String])(_ ++ _)
      case JObject(fields) => fields.map { case (key, value) =>
          json2map((if (prefix.isEmpty) "" else prefix + ".") + key, value)
        }.foldLeft(Map.empty[String, String])(_ ++ _)
      case JNull => Map.empty
      case JNothing => Map.empty
      case JBool(value) => Map(prefix -> value.toString)
      case JDouble(value) => Map(prefix -> value.toString)
      case JDecimal(value) => Map(prefix -> value.toString)
      case JInt(value) => Map(prefix -> value.toString)
      case JString(value) => Map(prefix -> value.toString)
    }
  
  // Find a workObject from map tree workList; create one if not exist
  def workObject(workList: HashMap[String, Any], name: String, isArray: Boolean): Any = {
    workList.get(name) match {
      case Some(theObj) => theObj
      case None => {
        val (parent, self, isArray1) = splitName(name)
        val parentObj = workObject(workList, parent, isArray1).asInstanceOf[HashMap[String, Any]]
        val theObj = if (isArray) ListBuffer[String]() else HashMap[String, Any]()
        parentObj += (self -> theObj)
        workList  += (name -> theObj)
        theObj
      }
    }
  }
}
