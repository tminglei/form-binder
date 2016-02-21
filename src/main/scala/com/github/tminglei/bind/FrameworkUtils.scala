package com.github.tminglei.bind

import java.util.regex.Pattern
import org.slf4j.LoggerFactory

import scala.collection.mutable.{ListBuffer, HashMap}
import org.json4s._

/**
 * Framework internal used util methods (!!!NOTE: be careful if using it externally)
 */
object FrameworkUtils {
  private val logger = LoggerFactory.getLogger(FrameworkUtils.getClass)

  val ILLEGAL_ARRAY_INDEX = """.*\[(\d*[^\d\[\]]+\d*)+\].*""".r
  /** copied from Play! form/mapping */
  val EMAIL_REGEX = """^(?!\.)("([^"\r\\]|\\["\r\\])*"|([-a-zA-Z0-9!#$%&'*+/=?^_`{|}~]|(?<!\.)\.)*)(?<!\.)@[a-zA-Z0-9][\w\.-]*[a-zA-Z0-9]\.[a-zA-Z][a-zA-Z\.]*[a-zA-Z]$""".r

  //////////////////////////////////////////////////////////////////////////////////
  val PassValidating: Constraint = (name, data, messages, parentOptions) => Nil

  def isEmptyStr(str: String): Boolean =
    str == null || (str.trim == "") || str.equalsIgnoreCase("null")

  def isEmptyInput(name: String, data: Map[String, String], inputMode: InputMode): Boolean = {
    logger.trace(s"checking empty input for $name")
    
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
  def splitName(name: String): (String, String, Boolean) = {
    logger.trace(s"splitting name: '$name'")

    name match {
      case ARRAY_ELEM_NAME (name, index)  => (name, index, true)
      case OBJECT_ELEM_NAME(parent, name) => (parent, name, false)
      case _  => ("", name, false)
    }
  }

  // Find a workObject from map tree workList; create one if not exist
  def workObject(workList: HashMap[String, Any], name: String, isArray: Boolean): Any = {
    logger.trace(s"get working object for $name")
    
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

  //-----------------------------------------------------------------------------
  def mkExtensionMeta(name: String, params: Any*): ExtensionMeta = {
    val paramsStr = params.map(Option(_).map(_.toString).getOrElse("")).mkString(", ")
    ExtensionMeta(name, s"$name($paramsStr)", params.toList)
  }

  // make an internal converter from `(vString) => value`
  def mkSimpleConverter[T](convert: String => T) =
    (name: String, data: Map[String, String]) => {
      convert(data.get(name).orNull)
    }
  
  // make a constraint from `(label, vString, messages) => [error]` (ps: vString may be NULL/EMPTY)
  def mkSimpleConstraint(validate: (String, String, Messages) => Option[String], meta: ExtensionMeta): Constraint with Metable[ExtensionMeta] =
    mkConstraintWithMeta(
      (name, data, messages, options) => {
        if (options._inputMode != SoloInput) {
          throw new IllegalArgumentException("The constraint should only be used to SINGLE INPUT mapping!")
        } else {
          validate(getLabel(name, messages, options), data.get(name).orNull, messages)
            .map { error => Seq(name -> error) }.getOrElse(Nil)
        }
      }, meta)

  def mkConstraintWithMeta(validate: (String, Map[String, String], Messages, Options) => Seq[(String, String)], meta: ExtensionMeta) =
    new Constraint with Metable[ExtensionMeta] {
      def apply(name: String, data: Map[String, String], messages: Messages, options: Options) =
        validate.apply(name, data, messages, options)
      override def _meta: ExtensionMeta = meta
    }

  def mkExtraConstraintWithMeta[T](validate: (String, T, Messages) => Seq[String], meta: ExtensionMeta) =
    new ExtraConstraint[T] with Metable[ExtensionMeta] {
      def apply(label: String, vObj: T, messages: Messages) = validate.apply(label, vObj, messages)
      override def _meta: ExtensionMeta = meta
    }

  def mkPreProcessorWithMeta(process: (String, Map[String, String], Options) => Map[String, String], meta: ExtensionMeta) =
    new PreProcessor with Metable[ExtensionMeta] {
      def apply(prefix: String, data: Map[String, String], options: Options) = process.apply(prefix, data, options)
      override def _meta: ExtensionMeta = meta
    }
  
  def isUntouchedEmpty(name: String, data: Map[String, String], options: Options) = 
    isEmptyInput(name, data, options._inputMode) && 
       options.ignoreEmpty.getOrElse(false) && 
      (options.touched.isEmpty || ! options.touched.get.apply(name, data))

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
            constraints: List[Constraint]): Seq[(String, String)] = {
    if (options.eagerCheck.getOrElse(false))
      constraints.flatMap(_.apply(name, data, messages, options))
    else {
      constraints match {
        case (constraint :: rest) => 
          constraint.apply(name, data, messages, options) match {
            case Nil    => validateRec(name, data, messages, options, rest)
            case errors => errors
          }
        case _ => Nil
      }
    }
  }    

  def extraValidateRec[T](name: String, value: => T, messages: Messages, options: Options,
            constraints: List[ExtraConstraint[T]]): Seq[(String, String)] = {
    val label = getLabel(name, messages, options)
    if (options.eagerCheck.getOrElse(false))
      constraints.flatMap(_.apply(label, value, messages).map((name, _)))
    else {
      constraints match {
        case (constraint :: rest) =>
          constraint.apply(label, value, messages) match {
            case Nil    => extraValidateRec(name, value, messages, options, rest)
            case errors => errors.map { case (message) => (name, message) }
          }
        case _ => Nil
      }
    }
  }    

  ///---

  // i18n on: use i18n label, if exists; else use label; else use last field name from full name
  // i18n off: use label; else use last field name from full name
  def getLabel(fullName: String, messages: Messages, options: Options): String = {
    logger.trace(s"getting label for '$fullName' with options (i18n: ${options.i18n}}, _label: ${options._label})")

    val (parent, name, isArray) = splitName(fullName)
    val default = if (isArray) (splitName(parent)._2 + "[" + name + "]") else name
    if (options.i18n.getOrElse(false)) {
      options._label.flatMap(messages(_).orElse(options._label)).getOrElse(default)
    } else options._label.getOrElse(default)
  }

  // make a Constraint which will try to check and collect errors
  def checking[T](check: String => T, messageOrKey: Either[String, String], extraMessageArgs: String*): Constraint =
    mkSimpleConstraint(
      (label, vString, messages) => {
        logger.debug(s"checking for '$vString'")

        vString match {
          case null|"" => None
          case x => {
            try { check(x); None }
            catch {
              case e: Exception => {
                val msgTemplate = messageOrKey.fold(s => s, messages(_).get)
                Some(msgTemplate.format((vString +: extraMessageArgs): _*))
              }
            }
          }
        }
      }, mkExtensionMeta("checking"))

  // make a compound Constraint, which checks whether any inputting constraints passed
  def anyPassed(constraints: Constraint*): Constraint with Metable[ExtensionMeta] = mkConstraintWithMeta(
    (name, data, messages, options) => {
      logger.debug(s"checking any passed for $name")
      
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
    }, meta = mkExtensionMeta("anyPassed"))

  // Computes the available indexes for the given key in this set of data.
  def indexes(name: String, data: Map[String, String]): Seq[Int] = {
    logger.debug(s"get indexes for $name")
    // matches: 'prefix[index]...'
    val KeyPattern = ("^" + Pattern.quote(name) + """\[(\d+)\].*$""").r
    data.toSeq.collect { case (KeyPattern(index), _) => index.toInt }.sorted.distinct
  }

  // Computes the available keys for the given prefix in this set of data.
  def keys(prefix: String, data: Map[String, String]): Seq[String] = {
    logger.debug(s"get keys for $prefix")
    // matches: 'prefix.xxx...' | 'prefix."xxx.t"...'
    val KeyPattern = ("^" + Pattern.quote(prefix) + """\.("[^"]+"|[^.]+).*$""").r
    data.toSeq.collect { case (KeyPattern(key), _) => key }.distinct
  }

  // Construct data map from inputting jackson json object
  def json2map(prefix: String, json: JValue): Map[String, String] = {
    logger.trace(s"json to map - prefix: $prefix")

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
      case JLong(value) => Map(prefix -> value.toString)
      case JString(value) => Map(prefix -> value.toString)
    }
  }
}
