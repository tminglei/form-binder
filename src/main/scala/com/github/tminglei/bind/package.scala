package com.github.tminglei

package object bind {

  // (messageKey) => [message] (ps: all input parameters WON'T BE NULL/EMPTY)
  type Messages = (String) => Option[String]

  // (name, data, messages, options) => errors (ps: all input parameters WON'T BE NULL/EMPTY)
  type Constraint = (String, Map[String, String], Messages, Options) => Seq[(String, String)]

  // (label, vObject, messages) => errors (ps: all input parameters WON'T BE NULL/EMPTY)
  type ExtraConstraint[T] = (String, T, Messages) => Seq[String]

  // (prefix, data, options) => data (ps: all input parameters WON'T BE NULL/EMPTY)
  type PreProcessor = (String, Map[String, String], Options) => Map[String, String]

  // (errors) => R (ps: all inputs parameter WON'T BE NULL/EMPTY)
  type ErrProcessor[R] = (Seq[(String, String)]) => R

  // (prefix, data) => true/false (ps: all input parameters WON'T BE NULL/EMPTY)
  type TouchedChecker = (String, Map[String, String]) => Boolean

  /**
   * A helper object, used to simplify `form-binder` usage
   *
   * Note: add {{{import com.github.tminglei.bind.simple._}}} to your class, then
   *   you can use form binder's built-in mappings/constraints/processors directly
   */
  object simple extends Mappings with Constraints with Processors {
    import collection.convert.wrapAsScala._

    type FormBinder[R] = com.github.tminglei.bind.FormBinder[R]
    val  FormBinder = com.github.tminglei.bind.FormBinder

    ///--
    def data(params: java.util.Map[String, Array[String]], others: (String, String)*): Map[String, String] =
      data(params.map { case (k, v) => (k, v.toSeq) }.toMap, others: _*)

    def data(params: Map[String, Seq[String]], others: (String, String)*): Map[String, String] = {
      params.map { case (key, values) =>
        if (values == null || values.length == 0) Nil
        else if (values.length == 1 && ! key.endsWith("[]")) Seq((key, values(0)))
        else {
          for(i <- 0 until values.length) yield {
            val cleanKey = key.replaceAll("\\[\\]$", "")
            (s"$cleanKey[$i]", values(i))
          }
        }
      }.flatten.toMap ++ others
    }
  }
}

package bind {

  import org.slf4j.LoggerFactory

  /**
    * The Facade class
    */
  case class FormBinder[R](messages: Messages,
    errProcessor: ErrProcessor[R] = identity[Seq[(String, String)]] _) {

    private val logger = LoggerFactory.getLogger(FormBinder.getClass)

    /**
      * bind mappings to data, and return an either, holding validation errors (left) or converted value (right)
      */
    def bind[T](mapping: Mapping[T], data: Map[String, String], root: String = ""): Either[R, T] = {
      logger.debug(s"start binding ... from '$root'")
      mapping.validate(root, data, messages, Options.apply()) match {
        case Nil  => Right(mapping.convert(root, data))
        case errs => Left(errProcessor.apply(errs))
      }
    }

    /**
      * bind and validate data, return (processed) errors
      */
    def validate[T](mapping: Mapping[T], data: Map[String, String], root: String = ""): R = {
      logger.debug(s"start validating ... from '$root'")
      errProcessor.apply(
        mapping.validate(root, data, messages, Options.apply())
      )
    }
  }

  ///////////////////////////////////////// helper interfaces and classes //////////////////////////////////
  /**
    * Some mark traits, used to help ensure the matching of fixtures in a data processing flow/pipe
    */
  sealed trait InputMode
  object SoloInput extends InputMode
  object BulkInput extends InputMode
  object PolyInput extends InputMode

  /**
    * A mark trait, to help distinguish ext object from other normals
    */
  trait Extensible extends Cloneable

  /**
    * Trait/classes for meta support
    */
  trait Metable[M] {
    def _meta: M
  }

  case class MappingMeta(targetType: reflect.ClassTag[_], baseMappings: List[Mapping[_]] = Nil)
  case class ExtensionMeta(name: String, desc: String, params: List[_] = Nil)

  final class Ignored[T]

  /**
    * Used to transfer config info in the data processing flow
    */
  case class Options(
    /** whether to check errors as more as possible */
    eagerCheck: Option[Boolean] = None,
    /** whether to skip checking untouched empty field/values */
    skipUntouched: Option[Boolean] = None,
    /** used to check whether a field was touched by user; if yes, required fields can't be empty */
    touchedChecker: Option[TouchedChecker] = None,
    // internal state, only applied to current mapping
    private[bind] val _label: Option[String] = None,
    private[bind] val _constraints: List[Constraint] = Nil,
    private[bind] val _extraConstraints: List[ExtraConstraint[_]] = Nil,
    private[bind] val _processors: List[PreProcessor] = Nil,
    private[bind] val _ignoreConstraints: Boolean = false,
    private[bind] val _inputMode: InputMode = SoloInput,
    private[bind] val _extData: Option[Extensible] = None
  ) {
    def eagerCheck(check: Boolean): Options = copy(eagerCheck = Some(check))
    def skipUntouched(skip: Boolean): Options = copy(skipUntouched = Some(skip))
    def touchedChecker(touched: TouchedChecker): Options = copy(touchedChecker = Some(touched))

    def $extraConstraints[T] = _extraConstraints.map(_.asInstanceOf[ExtraConstraint[T]])

    def merge(parent: Options): Options = copy(
      eagerCheck  = eagerCheck.orElse(parent.eagerCheck),
      skipUntouched = skipUntouched.orElse(parent.skipUntouched),
      touchedChecker = touchedChecker.orElse(parent.touchedChecker))
  }
}