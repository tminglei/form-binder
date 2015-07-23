package com.github.tminglei.bind

import FrameworkUtils._
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

///////////////////////////////////////// core interfaces and classes //////////////////////////////////
/**
 * Some mark traits, used to help ensure the matching of fixtures in a data processing flow/pipe
 */
sealed trait InputMode
object SoloInput extends InputMode
object BulkInput extends InputMode
object PolyInput extends InputMode

/**
 * Used to transfer config info in the data processing flow
 */
case class Options(
  i18n: Option[Boolean] = None,
  eagerCheck: Option[Boolean] = None,
  ignoreEmpty: Option[Boolean] = None,
  touched: Option[TouchedChecker] = None,
  // internal options, only applied to current mapping
  _label: Option[String] = None,
  _constraints: List[Constraint] = Nil,
  _processors: List[PreProcessor] = Nil,
  _ignoreConstraints: Boolean = false,
  _inputMode: InputMode = SoloInput
 ) {
  def i18n(i18n: Boolean): Options = copy(i18n = Some(i18n))
  def eagerCheck(check: Boolean): Options = copy(eagerCheck = Some(check))
  def ignoreEmpty(ignore: Boolean): Options = copy(ignoreEmpty = Some(ignore))
  def touched(touched: TouchedChecker): Options = copy(touched = Some(touched))

  def merge(parent: Options): Options = copy(
    i18n = i18n.orElse(parent.i18n),
    eagerCheck  = eagerCheck.orElse(parent.eagerCheck),
    ignoreEmpty = ignoreEmpty.orElse(parent.ignoreEmpty),
    touched = touched.orElse(parent.touched))
}

/**
 * A mapping, w/ constraints/processors/options, was used to validate/convert input data
 */
trait Mapping[T] {
  def options: Options = Options.apply()
  def options(setting: Options => Options) = this
  def label(label: String) = options(_.copy(_label = Option(label)))
  def >-:(newProcessors: PreProcessor*) = options(_.copy(_processors = newProcessors ++: options._processors))
  def >+:(newConstraints: Constraint*) = options(_.copy(_constraints = newConstraints ++: options._constraints))
  def verifying(validates: ExtraConstraint[T]*) = this

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
                moreValidate: Constraint = PassValidating, extraConstraints: List[ExtraConstraint[T]] = Nil,
                override val options: Options = Options.apply()) extends Mapping[T] {
  private val logger = LoggerFactory.getLogger(FieldMapping.getClass)

  override def options(setting: Options => Options) = copy(options = setting(options))
  override def verifying(validates: ExtraConstraint[T]*) = copy(extraConstraints = extraConstraints ++ validates)

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
          extraValidateRec(name, v, messages, theOptions, extraConstraints)
        }.getOrElse(Nil)
      } else errors
    }
  }
}

/**
 * A group mapping is a compound mapping, and is used to construct a complex/nested mapping
 */
case class GroupMapping[T](fields: Seq[(String, Mapping[_])], doConvert: (String, Map[String, String]) => T,
                extraConstraints: List[ExtraConstraint[T]] = Nil,
                override val options: Options = Options.apply(_inputMode = BulkInput)) extends Mapping[T] {
  private val logger = LoggerFactory.getLogger(GroupMapping.getClass)

  override def options(setting: Options => Options) = copy(options = setting(options))
  override def verifying(validates: ExtraConstraint[T]*) = copy(extraConstraints = extraConstraints ++ validates)

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
          extraValidateRec(name, doConvert(name, newData), messages, theOptions, extraConstraints)
        }
      } else errors
    }
  }
}
