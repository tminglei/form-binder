package com.github.tminglei.bind

import FrameworkUtils._
import org.slf4j.LoggerFactory

import scala.reflect._

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

///////////////////////////////////////// core interfaces and classes //////////////////////////////////
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

case class MappingMeta(targetType: ClassTag[_], baseMappings: List[Mapping[_]] = Nil)
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

/**
 * A mapping, w/ constraints/processors/options, was used to validate/convert input data
 */
trait Mapping[T] extends Metable[MappingMeta] {
  def options: Options = Options.apply()
  def options(setting: Options => Options) = this
  def label(label: String) = options(_.copy(_label = Option(label)))
  def $ext(setting: Extensible => Extensible) = options(_.copy(_extData = Option(setting(options._extData.orNull))))
  def >-:(newProcessors: PreProcessor*) = options(_.copy(_processors = newProcessors ++: options._processors))
  def >+:(newConstraints: Constraint*) = options(_.copy(_constraints = newConstraints ++: options._constraints))
  def verifying(validates: ExtraConstraint[T]*) = options(_.copy(_extraConstraints = options._extraConstraints ++ validates))

  def convert(name: String, data: Map[String, String]): T
  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)]
  def mapTo[R](transform: T => R): Mapping[R] = new TransformMapping[T, R](this, transform)
}

///////////////////////////////////////// core mapping implementations /////////////////////////////////
/**
 * A wrapper mapping, used to transform converted value to another
 */
private
case class TransformMapping[T, R](base: Mapping[T], transform: T => R,
                extraConstraints: List[ExtraConstraint[R]] = Nil) extends Mapping[R] {
  private val logger = LoggerFactory.getLogger(TransformMapping.getClass)

  override def _meta = base._meta
  override def options = base.options
  override def options(setting: Options => Options) = copy(base = base.options(setting))
  override def verifying(validates: ExtraConstraint[R]*) = copy(extraConstraints = extraConstraints ++ validates)

  def convert(name: String, data: Map[String, String]): R = {
    logger.debug(s"transforming $name")
    transform(base.convert(name, data))
  }

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val errors = base.validate(name, data, messages, parentOptions)
    if (errors.isEmpty)
      Option(convert(name, data)).map { v =>
        extraValidateRec(name, v, messages, base.options.merge(parentOptions), extraConstraints)
      }.getOrElse(Nil)
    else errors
  }
}

/**
 * A field mapping is an atomic mapping, which doesn't contain other mappings
 */
case class FieldMapping[T](inputMode: InputMode = SoloInput, doConvert: (String, Map[String, String]) => T,
                moreValidate: Constraint = PassValidating, meta: MappingMeta,
                override val options: Options = Options.apply()) extends Mapping[T] {
  private val logger = LoggerFactory.getLogger(FieldMapping.getClass)

  override val _meta = meta
  override def options(setting: Options => Options) = copy(options = setting(options))

  def convert(name: String, data: Map[String, String]): T = {
    logger.debug(s"converting $name")
    val newData = processDataRec(name, data, options.copy(_inputMode = inputMode), options._processors)
    doConvert(name, newData)
  }

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    logger.debug(s"validating $name")

    val theOptions = options.merge(parentOptions).copy(_inputMode = inputMode)
    val newData = processDataRec(name, data, theOptions, theOptions._processors)

    if (isUntouchedEmpty(name, newData, theOptions)) Nil
    else {
      val validates = (if (theOptions._ignoreConstraints) Nil else theOptions._constraints) :+
        moreValidate
      val errors = validateRec(name, newData, messages, theOptions, validates)
      if (errors.isEmpty) {
        Option(doConvert(name, newData)).map { v =>
          extraValidateRec(name, v, messages, theOptions, theOptions.$extraConstraints)
        }.getOrElse(Nil)
      } else errors
    }
  }
}

/**
 * A group mapping is a compound mapping, and is used to construct a complex/nested mapping
 */
case class GroupMapping[T](fields: Seq[(String, Mapping[_])], doConvert: (String, Map[String, String]) => T,
                override val options: Options = Options.apply(_inputMode = BulkInput)) extends Mapping[T] {
  private val logger = LoggerFactory.getLogger(GroupMapping.getClass)

  override val _meta = MappingMeta(classTag[Product], Nil)
  override def options(setting: Options => Options) = copy(options = setting(options))

  def convert(name: String, data: Map[String, String]): T = {
    logger.debug(s"converting $name")

    val newData = processDataRec(name, data, options, options._processors)
    if (isEmptyInput(name, newData, options._inputMode)) null.asInstanceOf[T]
    else doConvert(name, newData)
  }

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    logger.debug(s"validating $name")

    val theOptions = options.merge(parentOptions)
    val newData  = processDataRec(name, data, theOptions, theOptions._processors)

    if (isUntouchedEmpty(name, newData, theOptions)) Nil
    else {
      val validates = theOptions._constraints :+
        { (name: String, data: Map[String, String], messages: Messages, options: Options) =>
          if (isEmptyInput(name, data, options._inputMode)) Nil
          else {
            fields.map { case (fieldName, binding) =>
              val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
              binding.validate(fullName, data, messages, options)
            }.flatten
          }
        }

      val errors = validateRec(name, newData, messages, theOptions, validates)
      if (errors.isEmpty) {
        if (isEmptyInput(name, newData, options._inputMode)) Nil
        else {
          extraValidateRec(name, doConvert(name, newData), messages, theOptions, theOptions.$extraConstraints)
        }
      } else errors
    }
  }
}
