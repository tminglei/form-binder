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
                   preProcessors: Seq[BulkPreProcessor] = Nil,
                   touchExtractor: Option[TouchedExtractor] = None,
                   errProcessor: Option[PostErrProcessor[R]] = None) {

  def pipe_:(newProcessors: BulkPreProcessor*) = copy(preProcessors = newProcessors ++ preProcessors)
  def withTouched(touchExtractor: TouchedExtractor) = copy(touchExtractor = Some(touchExtractor))
  def withErr[R1](errProcessor: PostErrProcessor[R1]) = copy(errProcessor = Some(errProcessor))

  /**
   * bind mappings to data, if validation passed, consume it
   * @return `consume` produced result, if validation passed; (transformed) errors, if validation failed
   */
  def bind[T, R2](mapping: Mapping[T], data: Map[String, String])(consume: T => R2) = {
    val data1 = bulkProcessRec(data, preProcessors.toList)
    mapping.validate("", data1, messages, mapping.options) match {
      case Nil  => consume(mapping.convert("", data1))
      case errs => errProcessor.getOrElse(defaultErrProcessor).apply(errs)
    }
  }

  /**
   * bind and validate only data, not consume it
   * @return (transformed) errors
   */
  def validate[T](mapping: Mapping[T], data: Map[String, String], touched: Option[Seq[String]] = None) = {
    val data1 = bulkProcessRec(data, preProcessors.toList)
    val touched1 = touched.orElse(touchExtractor.map(_.apply(data))).getOrElse(Nil)
    val errs  = mapping.validate("", data1, messages, mapping.options.copy(touched = touched1))
    errProcessor.getOrElse(defaultErrProcessor).apply(errs)
  }

  private val defaultErrProcessor: PostErrProcessor[Map[String, List[String]]] =
    (errors) => { Map.empty ++
      errors.groupBy(_._1).map {
        case (key, pairs) => (key, pairs.map(_._2).toList)
      }
    }
}

/////////////////////////// core interfaces and classes //////////////////////////////////
case class Options(
  label: Option[String] = None,
  i18n: Option[Boolean] = None,
  eagerCheck: Option[Boolean] = None,
  ignoreEmpty: Option[Boolean] = None,
  touched: Seq[String] = Nil
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
  def label(label: String) = this

  def convert(name: String, data: Map[String, String]): T
  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)]
  def verifying(validates: ExtraConstraint[T]*): Mapping[T] = new MoreCheckMapping(this, validates)
  def mapTo[R](transform: T => R): Mapping[R] = new TransformMapping(this, transform)
}

/////////////////////////// core mapping implementations /////////////////////////////////
private // A wrapper mapping, used to transform converted value to another
case class TransformMapping[T, R](base: Mapping[T], transform: T => R) extends Mapping[R] {
  override def options = base.options
  override def options(setting: Options => Options) = copy(base = base.options(setting))
  override def label(label: String) = copy(base = base.label(label))

  def convert(name: String, data: Map[String, String]): R = transform(base.convert(name, data))

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] =
    base.validate(name, data, messages, parentOptions)
}

private // A wrapper mapping, used to hold and process extra constraints
case class MoreCheckMapping[T](base: Mapping[T], validates: Seq[ExtraConstraint[T]]) extends Mapping[T] {
  override def options = base.options
  override def options(setting: Options => Options) = copy(base = base.options(setting))
  override def label(label: String) = copy(base = base.label(label))

  def convert(name: String, data: Map[String, String]): T = base.convert(name, data)

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = base.options.merge(parentOptions)
    val result = base.validate(name, data, messages, theOptions)
    if (result.isEmpty) {
      Option(convert(name, data)).map {
        case value => extraValidateRec(name, value, validates.toList, messages, theOptions)
      }.getOrElse(Nil)
    } else {
      result
    }
  }
}

// A simple wrapped mapping, with label and options support
case class ThinMapping[T](convert0: (String, Map[String, String]) => T,
                validate0: (String, Map[String, String], Messages, Options) => Seq[(String, String)],
                override val options: Options = Options.apply()) extends Mapping[T] {

  override def options(setting: Options => Options) = copy(options = setting(options))
  override def label(label: String) = copy(options = options.copy(label = Option(label)))

  def convert(name: String, data: Map[String, String]): T = convert0(name, data)
  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] =
    validate0(name, data, messages, options.merge(parentOptions))
}

// A field mapping is an atomic mapping, which doesn't contain other mappings
case class FieldMapping[T](constraints: Seq[Constraint], convert: String => T, processors: Seq[PreProcessor] = Nil,
                  override val options: Options = Options.apply()) extends Mapping[T] {

  override def options(setting: Options => Options) = copy(options = setting(options))
  override def label(label: String) = copy(options = options.copy(label = Option(label)))
  def pipe_:(newProcessors: PreProcessor*) = copy(processors = newProcessors ++ processors)

  def convert(name: String, data: Map[String, String]): T =
    convert(processRec(data.get(name).orNull, processors.toList))

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = options.merge(parentOptions)
    if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty
      && data.get(name).filterNot {v => (v == null || v.isEmpty)}.isEmpty) Nil
    else {
      val value = processRec(data.get(name).orNull, processors.toList)
      validateRec(name, value, constraints.toList, messages, theOptions)
    }
  }

  def validate(name: String, value: String, messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = options.merge(parentOptions)
    if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty
      && (value == null || value.isEmpty)) Nil
    else validateRec(name, value, constraints.toList, messages, theOptions)
  }
}

// A group mapping is a compound mapping, and is used to construct a complex/nested mapping
case class GroupMapping[T](fields: Seq[(String, Mapping[_])], convert0: (String, Map[String, String]) => T,
                  override val options: Options = Options.apply()) extends Mapping[T] {

  override def options(setting: Options => Options) = copy(options = setting(options))
  override def label(label: String) = copy(options = options.copy(label = Option(label)))

  def convert(name: String, data: Map[String, String]): T = convert0(name, data)

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = options.merge(parentOptions)
    if (data.keys.find(_.startsWith(name)).isEmpty || data.contains(name)) {
      if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty) Nil
      else Seq(name -> messages("error.object").get.format(getLabel(messages, name, theOptions)))
    } else {
      fields.map { case (fieldName, binding) =>
        val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
        binding.validate(fullName, data, messages, theOptions)
      }.flatten
    }
  }
}
