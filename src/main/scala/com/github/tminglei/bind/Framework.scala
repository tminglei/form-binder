package com.github.tminglei.bind

/**
 * add {{{import com.github.tminglei.bind.simple._}}} to use
 * form binder's built-in mappings/constraints/processors directly
 */
object simple extends Mappings with Constraints with Processors {
  type FormBinder[R] = com.github.tminglei.bind.FormBinder[R]
  val  FormBinder = com.github.tminglei.bind.FormBinder
}

///////////////////////////////////////////////////////////////////////////////////

case class FormBinder[R](messages: Messages,
                   preProcessors: Seq[BulkPreProcessor] = Nil,
                   errProcessor: Option[PostErrProcessor[R]] = None) {

  def pipe_:(newProcessors: BulkPreProcessor*) = this.copy(preProcessors = newProcessors ++ preProcessors)
  def withErr[R1](errProcessor: PostErrProcessor[R1]) = this.copy(errProcessor = Some(errProcessor))

  def bind[T, R2](mapping: Mapping[T], data: Map[String, String])(consume: T => R2) = {
    val data1 = processrec(data, preProcessors.toList)
    mapping.validate("", data1, messages, Options.apply()) match {
      case Nil  => consume(mapping.convert("", data1))
      case errs => errProcessor.getOrElse(defaultErrProcessor).apply(errs)
    }
  }

  def validate[T](mapping: Mapping[T], data: Map[String, String]) = {
    val data1 = processrec(data, preProcessors.toList)
    val errs  = mapping.validate("", data1, messages, Options.apply())
    errProcessor.getOrElse(defaultErrProcessor).apply(errs)
  }

  @scala.annotation.tailrec
  private def processrec(data: Map[String,String], processors: List[BulkPreProcessor]): Map[String,String] =
    processors match {
      case (process :: rest) => processrec(process(data), rest)
      case _ => data
    }

  private val defaultErrProcessor: PostErrProcessor[Map[String, List[String]]] =
    (errors) => { Map.empty ++
      errors.groupBy(_._1).map {
        case (key, pairs) => (key, pairs.map(_._2).toList)
      }
    }
}

///
case class Options(
  label: Option[String] = None,
  eagerCheck: Option[Boolean] = None,
  ignoreEmpty: Option[Boolean] = None,
  touched: Seq[String] = Nil
 ) {
  def eagerCheck(check: Boolean): Options = copy(eagerCheck = Some(check))
  def ignoreEmpty(ignore: Boolean): Options = copy(ignoreEmpty = Some(ignore))
  def touched(touched: Seq[String]): Options = copy(touched = touched)

  def merge(parent: Options): Options = copy(
    eagerCheck  = eagerCheck.orElse(parent.eagerCheck),
    ignoreEmpty = ignoreEmpty.orElse(parent.ignoreEmpty),
    touched = parent.touched)
}

trait Mapping[T] {
  def options: Options = Options.apply()
  def options(setting: Options => Options): Mapping[T] = ???

  def convert(name: String, data: Map[String, String]): T
  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)]
  def verifying(validates: ExtraConstraint[T]*): Mapping[T] = new MoreCheckMapping(this, validates)
  def mapTo[R](transform: T => R): Mapping[R] = new TransformMapping(this, transform)
}

/////////////////////////////////////////////////////////////////////////////////

private
class TransformMapping[T, R](base: Mapping[T], transform: T => R) extends Mapping[R] {
  override def options = base.options
  override def options(setting: Options => Options) = { base.options(setting); this }

  def convert(name: String, data: Map[String, String]): R = transform(base.convert(name, data))

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] =
    base.validate(name, data, messages, parentOptions)
}

private
class MoreCheckMapping[T](base: Mapping[T], validates: Seq[ExtraConstraint[T]]) extends Mapping[T] {
  override def options = base.options
  override def options(setting: Options => Options) = { base.options(setting); this }

  def convert(name: String, data: Map[String, String]): T = base.convert(name, data)

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = base.options.merge(parentOptions)
    if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty
      && (data.keys.find(_.startsWith(name)).isEmpty ||
        (data.contains(name) && data.get(name).map {v => (v == null || v.isEmpty)} == Some(true)))) Nil
    else {
      val result = base.validate(name, data, messages, theOptions)
      if (result.isEmpty) {
        validaterec(name, convert(name, data), validates.toList, messages, theOptions)
      } else {
        result
      }
    }
  }

  private def validaterec(name: String, value: T, validates: List[ExtraConstraint[T]],
                          messages: Messages, options: Options): Seq[(String, String)] =
    validates match {
      case (validate :: rest) => validate(options.label.getOrElse(name), value, messages) match {
        case Nil    => validaterec(name, value, rest, messages, options)
        case errors => errors.map { case (fieldName, message) => {
          val fullName = if (name.isEmpty) fieldName else if (fieldName.isEmpty) name else name + "." + fieldName
          (fullName, message)
        }} ++ (if (options.eagerCheck.getOrElse(false))
          validaterec(name, value, rest, messages, options) else Nil)
      }
      case _ => Nil
    }
}

case class FieldMapping[T](constraints: Seq[Constraint], convert: String => T, processors: Seq[PreProcessor] = Nil,
                  override val options: Options = Options.apply()) extends Mapping[T] {

  override def options(setting: Options => Options): FieldMapping[T] = copy(options = setting(options))
  def label(label: String): FieldMapping[T] = copy(options = options.copy(label = Option(label)))

  def pipe_:(newProcessors: PreProcessor*): FieldMapping[T] = copy(processors = newProcessors ++ processors)

  def convert(name: String, data: Map[String, String]): T =
    convert(processrec(data.get(name).orNull, processors.toList))

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = options.merge(parentOptions)
    if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty
      && data.get(name).map {v => (v == null || v.isEmpty)} == Some(true)) Nil
    else {
      val value = processrec(data.get(name).orNull, processors.toList)
      validaterec(name, value, constraints.toList, messages, theOptions)
    }
  }

  def validate(name: String, value: String, messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = options.merge(parentOptions)
    if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty
      && (value == null || value.isEmpty)) Nil
    else validaterec(name, value, constraints.toList, messages, theOptions)
  }

  private def validaterec(name: String, value: String, validates: List[Constraint],
                          messages: Messages, options: Options): Seq[(String, String)] =
    validates match {
      case (validate :: rest) => validate(options.label.getOrElse(name), value, messages) match {
        case Some(message) => Seq(name -> message) ++ (if (options.eagerCheck.getOrElse(false))
          validaterec(name, value, rest, messages, options) else Nil)
        case None          => validaterec(name, value, rest, messages, options)
      }
      case _ => Nil
    }

  @scala.annotation.tailrec
  private def processrec(value: String, processors: List[PreProcessor]): String =
    processors match {
      case (process :: rest) => processrec(process(value), rest)
      case _                 => value
    }
}

case class GroupMapping[T](fields: Seq[(String, Mapping[_])], convert0: (String, Map[String, String]) => T,
                  override val options: Options = Options.apply()) extends Mapping[T] {

  override def options(setting: Options => Options): GroupMapping[T] = copy(options = setting(options))
  def label(label: String): GroupMapping[T] = copy(options = options.copy(label = Option(label)))

  def convert(name: String, data: Map[String, String]): T = convert0(name, data)

  def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = {
    val theOptions = options.merge(parentOptions)
    if (data.keys.find(_.startsWith(name)).isEmpty || data.contains(name)) {
      if (theOptions.ignoreEmpty.getOrElse(false) && theOptions.touched.find(_.startsWith(name)).isEmpty) Nil
      else Seq(name -> messages("error.object").format(theOptions.label.getOrElse(name)))
    } else {
      fields.map { case (fieldName, binding) =>
        val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
        binding.validate(fullName, data, messages, theOptions)
      }.flatten
    }
  }
}
