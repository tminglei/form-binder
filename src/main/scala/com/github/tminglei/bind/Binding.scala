package com.github.tminglei.bind

trait Binding[T] {

  def convert(name: String, params: Map[String, String]): T

  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)]

  def verifying(validates: ExtraConstraint[T]*): Binding[T] = new MoreCheckBinding(this, validates)

  def mapTo[R](transform: T => R): Binding[R] = new TransformBinding(this, transform)

}

trait Constraint {

  def validate(label: String, value: String, messages: Messages): Option[String]

}

/////////////////////////////////////////////////////////////////////////////////

private
class TransformBinding[T, R](base: Binding[T], transform: T => R) extends Binding[R] {
  
  def convert(name: String, params: Map[String, String]): R = transform(base.convert(name, params))
  
  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)] =
    base.validate(name, params, messages)
}

private
class MoreCheckBinding[T](base: Binding[T], validates: Seq[ExtraConstraint[T]]) extends Binding[T] {

  def convert(name: String, params: Map[String, String]): T = base.convert(name, params)

  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)] = {
    val result = base.validate(name, params, messages)
    if (result.isEmpty) {
      validaterec(name, convert(name, params), validates, messages)
    } else {
      result
    }
  }

  @scala.annotation.tailrec
  private def validaterec(name: String, value: T, validates: Seq[ExtraConstraint[T]],
                          messages: Messages): Seq[(String, String)] = {
    validates match {
      case (validate :: rest) => validate(value, messages) match {
        case Nil    => validaterec(name, value, rest, messages)
        case errors => errors.map { case (fieldName, message) => {
            val fullName = if (name.nonEmpty) fieldName else if (fieldName.isEmpty) name else name + "." + fieldName
            (fullName, message)
          }
        }
      }
      case _ => Nil
    }
  }
}

abstract class FieldBinding[T](constraints: Seq[Constraint],
                   var _processors: Seq[PreProcessor] = Nil, var _label: Option[String] = None) extends Binding[T] {

  def >>:(processors: PreProcessor*): FieldBinding[T] = { _processors = processors ++ _processors; this }

  def label(label: String) = { _label = Option(label) }

  def convert(name: String, params: Map[String, String]): T =
    convert(processrec(params.get(name).orNull, _processors))

  def convert(value: String): T

  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)] =
    validaterec(name, processrec(params.get(name).orNull, _processors), constraints, messages, _label)

  def validate(name: String, value: String, messages: Messages): Seq[(String, String)] =
    validaterec(name, value, constraints, messages, _label)

  @scala.annotation.tailrec
  private def validaterec(name: String, value: String, constraints: Seq[Constraint],
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
  private def processrec(value: String, processors: Seq[PreProcessor]): String = {
    processors match {
      case (process :: rest) => processrec(process(value), rest)
      case _ => value
    }
  }
}

abstract class GroupBinding[T](var _label: Option[String] = None) extends Binding[T] {
  
  def label(label: String) = { _label = Option(label) }

  def fields: Seq[(String, Binding[_])]

  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)] =
    if (params.contains(name) && params.get(name).isEmpty) {
      Seq(name -> messages("error.required").format(_label.getOrElse(name)))
    } else {
      fields.map { case (fieldName, binding) =>
        val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
        binding.validate(fullName, params, messages)
      }.flatten
    }
}