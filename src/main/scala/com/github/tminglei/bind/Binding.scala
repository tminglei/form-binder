package com.github.tminglei.bind

trait Binding[T] {

  def convert(name: String, params: Map[String, String]): T

  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)]

  def verifying(validates: Constraint1[T]*): Binding[T] =
    new MoreCheckBinding(this, validates)

}

trait Constraint {

  def validate(name: String, value: String, messages: Messages): Option[String]

}

/////////////////////////////////////////////////////////////////////////////////

private
class MoreCheckBinding[T](baseBinding: Binding[T],
                          validates: Seq[Constraint1[T]]) extends Binding[T] {

  def convert(name: String, params: Map[String, String]): T = baseBinding.convert(name, params)

  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)] = {
    val result = baseBinding.validate(name, params, messages)
    if (result.isEmpty) {
      validaterec(name, convert(name, params), validates, messages)
    } else {
      result
    }
  }

  @scala.annotation.tailrec
  private def validaterec(name: String, value: T, validates: Seq[Constraint1[T]],
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

abstract class FieldBinding[T](constraints: Constraint*) extends Binding[T] {

  def convert(name: String, params: Map[String, String]): T =
    convert(params.get(name).orNull)

  def convert(value: String): T

  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)] =
    validaterec(name, params.get(name).orNull, Seq(constraints: _*), messages)

  @scala.annotation.tailrec
  private def validaterec(name: String, value: String, constraints: Seq[Constraint],
                          messages: Messages): Seq[(String, String)] = {
    constraints match {
      case (x :: rest) => x.validate(name, value, messages) match {
        case Some(message) => Seq(name -> message)
        case None          => validaterec(name, value, rest, messages)
      }
      case _ => Nil
    }
  }
}

abstract class CompoundBinding[T] extends Binding[T] {

  def fields: Seq[(String, Binding[_])]

  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)] = {
    fields.map { case (fieldName, binding) =>
      val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
      binding.validate(fullName, params, messages)
    }.flatten
  }
}