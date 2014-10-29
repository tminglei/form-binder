package com.github.tminglei.bind

/**
 * add {{{import com.github.tminglei.bind.simple._}}} to use
 * form binder's built-in mappings/constraints/processors directly
 */
object simple extends Mappings with Constraints with Processors {
  type FormBinder[R] = com.github.tminglei.bind.FormBinder[R]
  val  FormBinder = com.github.tminglei.bind.FormBinder
}

import FrameworkUtils._
// the Facade class
case class FormBinder[R](messages: Messages,
               constraints: List[Constraint] = Nil,
               processors: List[PreProcessor] = Nil,
               touchExtractor: Option[TouchedExtractor] = None,
               errProcessor: Option[PostErrProcessor[R]] = None) {

  def >-:(newProcessors: PreProcessor*) = copy(processors = newProcessors ++: processors)
  def >+:(newConstraints: Constraint*) = copy(constraints = newConstraints ++: constraints)
  def withTouched(touchExtractor: TouchedExtractor) = copy(touchExtractor = Some(touchExtractor))
  def withErr[R1](errProcessor: PostErrProcessor[R1]) = copy(errProcessor = Some(errProcessor))

  /**
   * bind mappings to data, if validation passed, consume it
   * @return `consume` produced result, if validation passed; (transformed) errors, if validation failed
   */
  def bind[T, R2](mapping: Mapping[T], data: Map[String, String])(consume: T => R2) = {
    val data1  = processDataRec("", data, processors)
    val errors = validateRec("", data1, messages, Options.apply(), constraints)
    if (errors.isEmpty) {
      mapping.validate("", data1, messages, mapping.options) match {
        case Nil  => consume(mapping.convert("", data1))
        case errs => errProcessor.getOrElse(Processors.errsToMapList).apply(errs)
      }
    } else errProcessor.getOrElse(Processors.errsToMapList).apply(errors)
  }

  /**
   * bind and validate only data, not consume it
   * @return (transformed) errors
   */
  def validate[T](mapping: Mapping[T], data: Map[String, String], touched: Option[Seq[String]] = None) = {
    val data1 = processDataRec("", data, processors)
    val touched1 = touched.orElse(touchExtractor.map(_.apply(data))).getOrElse(Nil)
    val errors = validateRec("", data1, messages, Options.apply(), constraints)
    val errs = if (errors.isEmpty) mapping.validate("", data1, messages, mapping.options.copy(touched = touched1)) else errors
    errProcessor.getOrElse(Processors.errsToMapList).apply(errs)
  }
}

/////////////////////////// core interfaces and classes //////////////////////////////////
case class Options(
  i18n: Option[Boolean] = None,
  eagerCheck: Option[Boolean] = None,
  ignoreEmpty: Option[Boolean] = None,
  touched: Seq[String] = Nil,
  // internal options, only applied to current mapping
  _label: Option[String] = None,
  _constraints: List[Constraint] = Nil,
  _processors: List[PreProcessor] = Nil,
  _ignoreConstraints: Boolean = false,
  _multiInput: Boolean = false
 ) {
  def i18n(i18n: Boolean): Options = copy(i18n = Some(i18n))
  def eagerCheck(check: Boolean): Options = copy(eagerCheck = Some(check))
  def ignoreEmpty(ignore: Boolean): Options = copy(ignoreEmpty = Some(ignore))

  def merge(parent: Options): Options = copy(
    i18n = i18n.orElse(parent.i18n),
    eagerCheck  = eagerCheck.orElse(parent.eagerCheck),
    ignoreEmpty = ignoreEmpty.orElse(parent.ignoreEmpty),
    touched = parent.touched)
}

trait Mapping[T] {
  def options: Options = Options.apply()
  def options(setting: Options => Options) = this
  def label(label: String) = options(_.copy(_label = Option(label)))
  def >-:(newProcessors: PreProcessor*) = options(_.copy(_processors = newProcessors ++: options._processors))
  def >+:(newConstraints: Constraint*) = options(_.copy(_constraints = newConstraints ++: options._constraints))
  def verifying(validates: ExtraConstraint[T]*) = this

  def convert(name: String, data: Map[String, String]): T
  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)]
  def mapTo[R](transform: T => R): Mapping[R] = new TransformMapping(this, transform)
}

/////////////////////////// core mapping implementations /////////////////////////////////
private // A wrapper mapping, used to transform converted value to another
case class TransformMapping[T, R](base: Mapping[T], transform: T => R, extraConstraints: List[ExtraConstraint[R]] = Nil) extends Mapping[R] {
  override def options = base.options
  override def options(setting: Options => Options) = copy(base = base.options(setting))
  override def verifying(validates: ExtraConstraint[R]*) = copy(extraConstraints = extraConstraints ++ validates)

  def convert(name: String, data: Map[String, String]): R = transform(base.convert(name, data))

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val errors = base.validate(name, data, messages, parentOptions)
    if (errors.isEmpty)
      Option(convert(name, data)).map { v =>
        extraValidateRec(name, v, messages, base.options.merge(parentOptions), extraConstraints)
      }.getOrElse(Nil)
    else errors
  }
}

// A field mapping is an atomic mapping, which doesn't contain other mappings
case class FieldMapping[T](convert0: (String, Map[String, String]) => T,
                myValidate: (String, Map[String, String], Messages, Options) => Seq[(String, String)] = PassValidating,
                extraConstraints: List[ExtraConstraint[T]] = Nil,
                override val options: Options = Options.apply()) extends Mapping[T] {

  override def options(setting: Options => Options) = copy(options = setting(options))
  override def verifying(validates: ExtraConstraint[T]*) = copy(extraConstraints = extraConstraints ++ validates)

  def convert(name: String, data: Map[String, String]): T = convert0(name, processDataRec(name, data, options._processors))

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = options.merge(parentOptions)
    val data1  = processDataRec(name, data, theOptions._processors)
    if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty
      && isEmptyInput(name, data1, theOptions._multiInput)) Nil
    else {
      val errors = validateRec(name, data1, messages, theOptions, (if (theOptions._ignoreConstraints) Nil else theOptions._constraints) :+
        { (label: String, name: String, data: Map[String, String], messages: Messages) => myValidate(name, data, messages, theOptions) })
      if (errors.isEmpty)
        Option(convert(name, data1)).map { v =>
          extraValidateRec(name, v, messages, theOptions, extraConstraints)
        }.getOrElse(Nil)
      else errors
    }
  }
}

// A group mapping is a compound mapping, and is used to construct a complex/nested mapping
case class GroupMapping[T](fields: Seq[(String, Mapping[_])], convert0: (String, Map[String, String]) => T,
                extraConstraints: List[ExtraConstraint[T]] = Nil,
                override val options: Options = Options.apply(_multiInput = true)) extends Mapping[T] {

  override def options(setting: Options => Options) = copy(options = setting(options))
  override def verifying(validates: ExtraConstraint[T]*) = copy(extraConstraints = extraConstraints ++ validates)

  def convert(name: String, data: Map[String, String]): T = {
    val data1 = processDataRec(name, data, options._processors)
    if (isEmptyInput(name, data1, options._multiInput)) null.asInstanceOf[T]
    else convert0(name, data1)
  }

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = options.merge(parentOptions)
    val data1  = processDataRec(name, data, theOptions._processors)
    if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty
      && isEmptyInput(name, data1, theOptions._multiInput)) Nil
    else {
      val errors = validateRec(name, data1, messages, theOptions, theOptions._constraints :+
        { (label: String, name: String, data: Map[String, String], messages: Messages) =>
          if (isEmptyInput(name, data, theOptions._multiInput)) Nil
          else {
            fields.map { case (fieldName, binding) =>
              val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
              binding.validate(fullName, data, messages, theOptions)
            }.flatten
          }
        })
      if (errors.isEmpty) {
        if (isEmptyInput(name, data1, options._multiInput)) Nil
        else {
          extraValidateRec(name, convert0(name, data1), messages, theOptions, extraConstraints)
        }
      } else errors
    }
  }
}
