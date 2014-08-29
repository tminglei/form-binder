package com.github.tminglei.bind

trait Constraints {

  def required(message: String = ""): Constraint = new Constraint() {
    override def validate(name: String, value: String, messages: Messages): Option[String] =
      if (value == null || value.isEmpty) {
        Some( (if (message.isEmpty) messages("error.required") else message).format(name))
      } else None
  }

  def maxlength(length: Int): Constraint = new Constraint() {
    override def validate(name: String, value: String, messages: Messages): Option[String] =
      if (value != null && value.length > length) {
        Some(messages("error.maxlength").format(name, length))
      } else None
  }

  def minlength(length: Int): Constraint = new Constraint() {
    override def validate(name: String, value: String, messages: Messages): Option[String] =
      if (value != null && value.length < length) {
        Some(messages("error.minlength").format(name, length))
      } else None
  }

  def length(length: Int): Constraint = new Constraint() {
    override def validate(name: String, value: String, messages: Messages): Option[String] =
      if (value != null && value.length != length) {
        Some(messages("error.length").format(name, length))
      } else None
  }

  def oneOf(values: Seq[String], message: String = ""): Constraint = new Constraint() {
    override def validate(name: String, value: String, messages: Messages): Option[String] =
      if (value != null && !values.contains(value)) {
        Some( (if (message.isEmpty) messages("error.oneOf") else message).format(name, values.map("'" + _ + "'").mkString(", ")))
      } else None
  }

  def pattern(pattern: String, message: String = ""): Constraint = new Constraint {
    override def validate(name: String, value: String, messages: Messages): Option[String] =
      if (value != null && !value.matches("^" + pattern + "$")) {
        Some( (if (message.isEmpty) messages("error.pattern") else message).format(name, pattern))
      } else None
  }

  //////////////////////////////////  pre-defined constraint1 implementations  /////////////////////////

  def min[T: Ordering](minVal: T): Constraint1[T] = (value, messages) => {
    val ord = Ordering[T]; import ord._
    if (value < minVal) {
      Seq("" -> messages("error.min").format(value, minVal))
    } else Nil
  }
  
  def max[T: Ordering](maxVal: T): Constraint1[T] = (value, messages) => {
    val ord = Ordering[T]; import ord._
    if (value > maxVal) {
      Seq("" -> messages("error.max").format(value, maxVal))
    } else Nil
  }
}

object Constraints extends Constraints