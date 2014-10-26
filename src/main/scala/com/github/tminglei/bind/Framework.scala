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
               processors: List[BulkPreProcessor] = Nil,
               touchExtractor: Option[TouchedExtractor] = None,
               errProcessor: Option[PostErrProcessor[R]] = None) {

  def >>:(newProcessors: BulkPreProcessor*) = copy(processors = newProcessors ++: processors)
  def withTouched(touchExtractor: TouchedExtractor) = copy(touchExtractor = Some(touchExtractor))
  def withErr[R1](errProcessor: PostErrProcessor[R1]) = copy(errProcessor = Some(errProcessor))

  /**
   * bind mappings to data, if validation passed, consume it
   * @return `consume` produced result, if validation passed; (transformed) errors, if validation failed
   */
  def bind[T, R2](mapping: Mapping[T], data: Map[String, String])(consume: T => R2) = {
    val data1 = bulkProcessRec("", data, processors)
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
    val data1 = bulkProcessRec("", data, processors)
    val touched1 = touched.orElse(touchExtractor.map(_.apply(data))).getOrElse(Nil)
    val errs  = mapping.validate("", data1, messages, mapping.options.copy(touched = touched1))
    errProcessor.getOrElse(defaultErrProcessor).apply(errs)
  }

  private val defaultErrProcessor: PostErrProcessor[Map[String, List[String]]] =
    (errors: Seq[(String, String)]) => { Map.empty ++
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
  override def label(label: String) = copy(base = base.label(label))
  override def verifying(validates: ExtraConstraint[R]*) = copy(extraConstraints = extraConstraints ++ validates)

  def convert(name: String, data: Map[String, String]): R = transform(base.convert(name, data))

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val errors = base.validate(name, data, messages, parentOptions)
    if (errors.isEmpty) extraValidateRec(name, convert(name, data), extraConstraints, messages, options.merge(parentOptions))
    else errors
  }
}

// A simple wrapped mapping, with label and options support
case class ThinMapping[T](convert0: (String, Map[String, String]) => T,
                validate0: (String, Map[String, String], Messages, Options) => Seq[(String, String)],
                extraConstraints: List[ExtraConstraint[T]] = Nil,
                override val options: Options = Options.apply()) extends Mapping[T] {

  override def options(setting: Options => Options) = copy(options = setting(options))
  override def label(label: String) = copy(options = options.copy(label = Option(label)))
  override def verifying(validates: ExtraConstraint[T]*) = copy(extraConstraints = extraConstraints ++ validates)

  def convert(name: String, data: Map[String, String]): T = convert0(name, data)
  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val errors = validate0(name, data, messages, options.merge(parentOptions))
    if (errors.isEmpty) extraValidateRec(name, convert0(name, data), extraConstraints, messages, options.merge(parentOptions))
    else errors
  }
}

// A field mapping is an atomic mapping, which doesn't contain other mappings
case class FieldMapping[T](constraints: Seq[Constraint], convert0: String => T,
                extraConstraints: List[ExtraConstraint[T]] = Nil, processors: List[PreProcessor] = Nil,
                override val options: Options = Options.apply()) extends Mapping[T] {

  override def options(setting: Options => Options) = copy(options = setting(options))
  override def label(label: String) = copy(options = options.copy(label = Option(label)))
  override def verifying(validates: ExtraConstraint[T]*) = copy(extraConstraints = extraConstraints ++ validates)
  def >>:(newProcessors: PreProcessor*) = copy(processors = newProcessors ++: processors)

  def convert(name: String, data: Map[String, String]): T = convert0(processRec(data.get(name).orNull, processors))

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] =
    validate(name, data.get(name).orNull, messages, parentOptions)

  def validate(name: String, value: String, messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = options.merge(parentOptions)
    val value1 = processRec(value, processors)
    val errors = if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty
        && (value1 == null || value1.isEmpty)) Nil
      else validateRec(name, value1, constraints.toList, messages, theOptions)
    if (errors.isEmpty) extraValidateRec(name, convert0(value1), extraConstraints, messages, theOptions)
    else errors
  }
}

// A group mapping is a compound mapping, and is used to construct a complex/nested mapping
case class GroupMapping[T](fields: Seq[(String, Mapping[_])], convert0: (String, Map[String, String]) => T,
                extraConstraints: List[ExtraConstraint[T]] = Nil, processors: List[BulkPreProcessor] = Nil,
                override val options: Options = Options.apply()) extends Mapping[T] {

  override def options(setting: Options => Options) = copy(options = setting(options))
  override def label(label: String) = copy(options = options.copy(label = Option(label)))
  override def verifying(validates: ExtraConstraint[T]*) = copy(extraConstraints = extraConstraints ++ validates)
  def >>:(newProcessors: BulkPreProcessor*) = copy(processors = newProcessors ++: processors)

  def convert(name: String, data: Map[String, String]): T = convert0(name, bulkProcessRec(name, data, processors))

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = options.merge(parentOptions)
    val data1 = bulkProcessRec(name, data, processors)
    val errors = if (data1.keys.find(_.startsWith(name)).isEmpty || data1.contains(name)) {
        if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty) Nil
        else Seq(name -> messages("error.object").get.format(getLabel(messages, name, theOptions)))
      } else {
        fields.map { case (fieldName, binding) =>
          val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
          binding.validate(fullName, data1, messages, theOptions)
        }.flatten
      }
    if (errors.isEmpty) extraValidateRec(name, convert(name, data), extraConstraints, messages, theOptions)
    else errors
  }
}
