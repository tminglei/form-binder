package com.github.tminglei.bind

import scala.util.matching.Regex

trait Constraints {

  ////////////////////////////////////////////  pre-defined constraints  ////////////////////////////////////

  def required(message: String = ""): Constraint = new Constraint() {
    override def validate(label: String, value: String, messages: Messages): Option[String] =
      if (value == null || value.isEmpty) {
        Some( (if (message.isEmpty) messages("error.required") else message).format(label))
      } else None
  }

  def maxlength(length: Int, message: String = ""): Constraint = new Constraint() {
    override def validate(label: String, value: String, messages: Messages): Option[String] =
      if (value != null && value.length > length) {
        Some( (if (message.isEmpty) messages("error.maxlength") else message).format(value, length))
      } else None
  }

  def minlength(length: Int, message: String = ""): Constraint = new Constraint() {
    override def validate(label: String, value: String, messages: Messages): Option[String] =
      if (value != null && value.length < length) {
        Some( (if (message.isEmpty) messages("error.minlength") else message).format(value, length))
      } else None
  }

  def length(length: Int, message: String = ""): Constraint = new Constraint() {
    override def validate(label: String, value: String, messages: Messages): Option[String] =
      if (value != null && value.length != length) {
        Some( (if (message.isEmpty) messages("error.length") else message).format(value, length))
      } else None
  }

  def oneOf(values: Seq[String], message: String = ""): Constraint = new Constraint() {
    override def validate(label: String, value: String, messages: Messages): Option[String] =
      if (value != null && !values.contains(value)) {
        Some( (if (message.isEmpty) messages("error.oneOf") else message).format(value, values.map("'" + _ + "'").mkString(", ")))
      } else None
  }

  def pattern(pattern: Regex, message: String = ""): Constraint = new Constraint {
    override def validate(label: String, value: String, messages: Messages): Option[String] =
      if (value != null && pattern.findFirstIn(value).isEmpty) {
        Some( (if (message.isEmpty) messages("error.pattern") else message).format(value, pattern.regex))
      } else None
  }

  def email(message: String = ""): Constraint = pattern(Constraints.EMAIL_REGEX, message)

  //////////////////////////////////////////  pre-defined extra constraints  ////////////////////////////////

  def min[T: Ordering](minVal: T, message: String = ""): ExtraConstraint[T] = (value, messages) => {
    val ord = Ordering[T]; import ord._
    if (value < minVal) {
      Seq("" -> (if (message.isEmpty) messages("error.min") else message).format(value, minVal))
    } else Nil
  }
  
  def max[T: Ordering](maxVal: T, message: String = ""): ExtraConstraint[T] = (value, messages) => {
    val ord = Ordering[T]; import ord._
    if (value > maxVal) {
      Seq("" -> (if (message.isEmpty) messages("error.max") else message).format(value, maxVal))
    } else Nil
  }
}

object Constraints extends Constraints {
  /** copied from Play! form/mapping */
  val EMAIL_REGEX = """^(?!\.)("([^"\r\\]|\\["\r\\])*"|([-a-zA-Z0-9!#$%&'*+/=?^_`{|}~]|(?<!\.)\.)*)(?<!\.)@[a-zA-Z0-9][\w\.-]*[a-zA-Z0-9]\.[a-zA-Z][a-zA-Z\.]*[a-zA-Z]$""".r
}