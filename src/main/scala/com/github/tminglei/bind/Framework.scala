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
    mapping.validate("", data1, messages) match {
      case Nil  => consume(mapping.convert("", data1))
      case errs => errProcessor.map(_.apply(errs)).getOrElse(errs)
    }
  }

  @scala.annotation.tailrec
  private def processrec(data: Map[String,String], processors: List[BulkPreProcessor]): Map[String,String] = {
    processors match {
      case (process :: rest) => processrec(process(data), rest)
      case _ => data
    }
  }
}

///
case class Options(
  label: Option[String] = None,
  deepCheck: Option[Boolean] = None,
  ignoreEmpty: Option[Boolean] = None
 ) {
  def label(label: String): Options = copy(label = Option(label))
  def deepCheck(check: Boolean): Options = copy(deepCheck = Some(check))
  def ignoreEmpty(ignore: Boolean): Options = copy(ignoreEmpty = Some(ignore))
}

trait Mapping[T] {
  def options: Options
  def options(setting: Options => Options): Mapping[T]

  def convert(name: String, data: Map[String, String]): T
  def validate(name: String, data: Map[String, String], messages: Messages): Seq[(String, String)]
  def verifying(validates: ExtraConstraint[T]*): Mapping[T] = new MoreCheckMapping(this, validates)
  def mapTo[R](transform: T => R): Mapping[R] = new TransformMapping(this, transform)
}

/////////////////////////////////////////////////////////////////////////////////

private
class TransformMapping[T, R](base: Mapping[T], transform: T => R) extends Mapping[R] {
  def options = base.options
  def options(setting: Options => Options) = { base.options(setting); this }

  def convert(name: String, data: Map[String, String]): R = transform(base.convert(name, data))

  def validate(name: String, data: Map[String, String], messages: Messages): Seq[(String, String)] =
    base.validate(name, data, messages)
}

private
class MoreCheckMapping[T](base: Mapping[T], validates: Seq[ExtraConstraint[T]]) extends Mapping[T] {
  def options = base.options
  def options(setting: Options => Options) = { base.options(setting); this }

  def convert(name: String, data: Map[String, String]): T = base.convert(name, data)

  def validate(name: String, data: Map[String, String], messages: Messages): Seq[(String, String)] = {
    val result = base.validate(name, data, messages)
    if (result.isEmpty) {
      validaterec(name, convert(name, data), validates.toList, messages, base.options.label)
    } else {
      result
    }
  }

  @scala.annotation.tailrec
  private def validaterec(name: String, value: T, validates: List[ExtraConstraint[T]],
                          messages: Messages, label: Option[String]): Seq[(String, String)] = {
    validates match {
      case (validate :: rest) => validate(label.getOrElse(name), value, messages) match {
        case Nil    => validaterec(name, value, rest, messages, label)
        case errors => errors.map { case (fieldName, message) => {
          val fullName = if (name.isEmpty) fieldName else if (fieldName.isEmpty) name else name + "." + fieldName
          (fullName, message)
        }}
      }
      case _ => Nil
    }
  }
}

case class FieldMapping[T](constraints: Seq[Constraint], convert: String => T, processors: Seq[PreProcessor] = Nil,
                           options: Options = Options.apply()) extends Mapping[T] {

  def options(setting: Options => Options): FieldMapping[T] = copy(options = setting(options))
  def label(label: String): FieldMapping[T] = options(_.label(label))

  def pipe_:(newProcessors: PreProcessor*): FieldMapping[T] = copy(processors = newProcessors ++ processors)

  def convert(name: String, data: Map[String, String]): T =
    convert(processrec(data.get(name).orNull, processors.toList))

  def validate(name: String, data: Map[String, String], messages: Messages): Seq[(String, String)] =
    validaterec(name, processrec(data.get(name).orNull, processors.toList), constraints.toList, messages, options.label)

  def validate(name: String, value: String, messages: Messages): Seq[(String, String)] =
    validaterec(name, value, constraints.toList, messages, options.label)

  @scala.annotation.tailrec
  private def validaterec(name: String, value: String, validates: List[Constraint],
                          messages: Messages, label: Option[String]): Seq[(String, String)] = {
    validates match {
      case (validate :: rest) => validate(label.getOrElse(name), value, messages) match {
        case Some(message) => Seq(name -> message)
        case None          => validaterec(name, value, rest, messages, label)
      }
      case _ => Nil
    }
  }

  @scala.annotation.tailrec
  private def processrec(value: String, processors: List[PreProcessor]): String = {
    processors match {
      case (process :: rest) => processrec(process(value), rest)
      case _                 => value
    }
  }
}

case class GroupMapping[T](fields: Seq[(String, Mapping[_])], convert0: (String, Map[String, String]) => T,
                           options: Options = Options.apply()) extends Mapping[T] {

  def options(setting: Options => Options): GroupMapping[T] = copy(options = setting(options))
  def label(label: String): GroupMapping[T] = options(_.label(label))

  def convert(name: String, data: Map[String, String]): T = convert0(name, data)

  def validate(name: String, data: Map[String, String], messages: Messages): Seq[(String, String)] =
    if (data.keys.find(_.startsWith(name)).isEmpty || data.contains(name)) {
      Seq(name -> messages("error.object").format(options.label.getOrElse(name)))
    } else {
      fields.map { case (fieldName, binding) =>
        val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
        binding.validate(fullName, data, messages)
      }.flatten
    }
}
