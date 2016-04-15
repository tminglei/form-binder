package com.github.tminglei.bind

import FrameworkUtils._
import org.slf4j.LoggerFactory

/**
 * A mapping, w/ constraints/processors/options, was used to validate/convert input data
 */
trait Mapping[T] extends Metable[MappingMeta] {

  /**
    * get the mapping's options
    */
  def options: Options = Options.apply()

  /**
    * set the mapping's options
    */
  def options(setting: Options => Options) = this

  /**
    * associate a label the mapping, which will be used when generating error message
    */
  def label(label: String) = options(_.copy(_label = Option(label)))

  /**
    * prepend some [[PreProcessor]]s to the mapping, which will be used to pre-process inputting data strings
    */
  def >-:(newProcessors: PreProcessor*) = options(_.copy(_processors = newProcessors ++: options._processors))

  /**
    * prepend some [[Constraint]]s to the mapping, which will be used to validate data string before it was converted
    */
  def >+:(newConstraints: Constraint*) = options(_.copy(_constraints = newConstraints ++: options._constraints))

  /**
    * append some [[ExtraConstraint]]s to the mapping, which will be used to validate data after it was converted
    */
  def verifying(validates: ExtraConstraint[T]*) = options(_.copy(_extraConstraints = options._extraConstraints ++ validates))

  ///---
  /**
    * do the converting and return the result object if successful
    * (p.s. to prevent failure, [[validate]] should be called before this)
    *
    * @param name the full name of the data node
    * @param data the data map
    * @return result object
    */
  def convert(name: String, data: Map[String, String]): T

  /**
    * do the validating and return the error message list
    * (p.s. if successful, the returned list is empty)
    *
    * @param name the full name of the data node
    * @param data the data map
    * @param messages the messages object
    * @param parentOptions the parent mapping's options
    * @return error message list
    */
  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)]

  /**
    * used to map/transform result object to another
    *
    * @param transform the transform function
    * @tparam R target result type
    * @return a new wrapper mapping
    */
  def map[R](transform: T => R): Mapping[R] = new TransformMapping[T, R](this, transform)
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

  override val _meta = MappingMeta(reflect.classTag[Product], Nil)
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
            fields.flatMap { case (fieldName, binding) =>
              val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
              binding.validate(fullName, data, messages, options)
            }
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
