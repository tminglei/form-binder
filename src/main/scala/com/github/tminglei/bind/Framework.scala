package com.github.tminglei.bind

case class Binder[T, R](mapping: Mapping[T], messages: Messages,
              preprocessors: Seq[BulkPreProcessor] = Nil,
              errprocessor: Option[PostErrProcessor[R]] = None) {

  def pipe_:(newprocessors: BulkPreProcessor*) = this.copy(preprocessors = newprocessors ++ preprocessors)
  def withErr[R1](errprocessor: PostErrProcessor[R1]) = this.copy(errprocessor = Some(errprocessor))

  def bind[R2](data: Map[String, String])(consume: T => R2) = {
    val data1 = processrec(data, preprocessors.toList)
    mapping.validate("", data1, messages) match {
      case Nil  => consume(mapping.convert("", data1))
      case errs => errprocessor.map(_.apply(errs)).getOrElse(errs)
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
trait Mapping[T] {

  def convert(name: String, data: Map[String, String]): T

  def validate(name: String, data: Map[String, String], messages: Messages): Seq[(String, String)]

  def verifying(validates: ExtraConstraint[T]*): Mapping[T] = new MoreCheckMapping(this, validates)

  def mapTo[R](transform: T => R): Mapping[R] = new TransformMapping(this, transform)

}

trait Constraint {

  def validate(label: String, value: String, messages: Messages): Option[String]

}

/////////////////////////////////////////////////////////////////////////////////

private
class TransformMapping[T, R](base: Mapping[T], transform: T => R) extends Mapping[R] {

  def convert(name: String, data: Map[String, String]): R = transform(base.convert(name, data))

  def validate(name: String, data: Map[String, String], messages: Messages): Seq[(String, String)] =
    base.validate(name, data, messages)
}

private
class MoreCheckMapping[T](base: Mapping[T], validates: Seq[ExtraConstraint[T]]) extends Mapping[T] {

  def convert(name: String, data: Map[String, String]): T = base.convert(name, data)

  def validate(name: String, data: Map[String, String], messages: Messages): Seq[(String, String)] = {
    val result = base.validate(name, data, messages)
    if (result.isEmpty) {
      validaterec(name, convert(name, data), validates.toList, messages)
    } else {
      result
    }
  }

  @scala.annotation.tailrec
  private def validaterec(name: String, value: T, validates: List[ExtraConstraint[T]],
                          messages: Messages): Seq[(String, String)] = {
    validates match {
      case (validate :: rest) => validate(value, messages) match {
        case Nil    => validaterec(name, value, rest, messages)
        case errors => errors.map { case (fieldName, message) => {
          val fullName = if (name.isEmpty) fieldName else if (fieldName.isEmpty) name else name + "." + fieldName
          (fullName, message)
        }
        }
      }
      case _ => Nil
    }
  }
}

abstract class FieldMapping[T](constraints: Seq[Constraint],
           var _processors: Seq[PreProcessor] = Nil, var _label: Option[String] = None) extends Mapping[T] {

  def pipe_:(processors: PreProcessor*): FieldMapping[T] = { _processors = (processors ++ _processors).toList; this }

  def label(label: String): FieldMapping[T] = { _label = Option(label); this }

  def convert(name: String, data: Map[String, String]): T =
    convert(processrec(data.get(name).orNull, _processors.toList))

  def convert(value: String): T

  def validate(name: String, data: Map[String, String], messages: Messages): Seq[(String, String)] =
    validaterec(name, processrec(data.get(name).orNull, _processors.toList), constraints.toList, messages, _label)

  def validate(name: String, value: String, messages: Messages): Seq[(String, String)] =
    validaterec(name, value, constraints.toList, messages, _label)

  @scala.annotation.tailrec
  private def validaterec(name: String, value: String, constraints: List[Constraint],
                          messages: Messages, label: Option[String]): Seq[(String, String)] = {
    constraints match {
      case (x :: rest) => x.validate(label.getOrElse(name), value, messages) match {
        case Some(message) => Seq(name -> message)
        case None          => validaterec(name, value, rest, messages, label)
      }
      case _ => Nil
    }
  }

  @scala.annotation.tailrec
  private def processrec(value: String, processors: List[PreProcessor]): String = {
    processors match {
      case (process :: rest) => {
        processrec(process(value), rest)
      }
      case _ => value
    }
  }
}

abstract class GroupMapping[T](var _label: Option[String] = None) extends Mapping[T] {

  def label(label: String): GroupMapping[T] = { _label = Option(label); this }

  def fields: Seq[(String, Mapping[_])]

  def validate(name: String, data: Map[String, String], messages: Messages): Seq[(String, String)] =
    if (data.contains(name) && data.get(name).isEmpty) {
      Seq(name -> messages("error.required").format(_label.getOrElse(name)))
    } else {
      fields.map { case (fieldName, binding) =>
        val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
        binding.validate(fullName, data, messages)
      }.flatten
    }
}