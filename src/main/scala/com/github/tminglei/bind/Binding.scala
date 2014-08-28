package com.github.tminglei.bind

trait Binding[T] {

  def convert(name: String, params: Map[String, String]): T

  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)]

  def verifying(validate: (T, Messages) => Seq[(String, String)]): Binding[T] =
    new MoreCheckBinding(this, validate)
}

trait Constraint {

  def validate(name: String, value: String, messages: Messages): Option[String]

}

/////////////////////////////////////////////////////////////////////////////////

private
class MoreCheckBinding[T](baseBinding: Binding[T],
                          validate: (T, Messages) => Seq[(String, String)]) extends Binding[T] {

  def convert(name: String, params: Map[String, String]): T = baseBinding.convert(name, params)

  def validate(name: String, params: Map[String, String], messages: Messages): Seq[(String, String)] = {
    val result = baseBinding.validate(name, params, messages)
    if (result.isEmpty) {
      validate(convert(name, params), messages)
    } else {
      result
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
      val fullname = if (name.isEmpty) fieldName else name + "." + fieldName
      binding.validate(fullname, params, messages)
    }.flatten
  }
}