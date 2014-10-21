package com.github.tminglei.bind

import scala.util.matching.Regex

trait Constraints {

  ////////////////////////////////////////////  pre-defined constraints  ////////////////////////////////////

  def required(message: String = ""): Constraint = (label, value, messages) =>
    if (value == null || value.isEmpty) {
      Some( (if (message.isEmpty) messages("error.required") else Some(message)).get.format(label))
    } else None

  def maxlength(length: Int, message: String = ""): Constraint = (label, value, messages) =>
    if (value != null && value.length > length) {
      Some( (if (message.isEmpty) messages("error.maxlength") else Some(message)).get.format(value, length))
    } else None

  def minlength(length: Int, message: String = ""): Constraint = (label, value, messages) =>
    if (value != null && value.length < length) {
      Some( (if (message.isEmpty) messages("error.minlength") else Some(message)).get.format(value, length))
    } else None

  def length(length: Int, message: String = ""): Constraint = (label, value, messages) =>
    if (value != null && value.length != length) {
      Some( (if (message.isEmpty) messages("error.length") else Some(message)).get.format(value, length))
    } else None

  def oneOf(values: Seq[String], message: String = ""): Constraint = (label, value, messages) =>
    if (!values.contains(value)) {
      Some( (if (message.isEmpty) messages("error.oneOf") else Some(message))
        .get.format(value, values.map("'" + _ + "'").mkString(", ")) )
    } else None

  def pattern(regex: Regex, message: String = ""): Constraint = (label, value, messages) =>
    if (value != null && regex.findFirstIn(value).isEmpty) {
      Some( (if (message.isEmpty) messages("error.pattern") else Some(message)).get.format(value, regex.toString))
    } else None

  def patternNot(regex: Regex, message: String = ""): Constraint = (label, value, messages) =>
    if (value != null && regex.findFirstIn(value).isDefined) {
      Some( (if (message.isEmpty) messages("error.patternnot") else Some(message)).get.format(value, regex.toString))
    } else None

  def email(message: String = ""): Constraint = pattern(Constraints.EMAIL_REGEX, message)

  //////////////////////////////////////////  pre-defined extra constraints  ////////////////////////////////

  def min[T: Ordering](minVal: T, message: String = ""): ExtraConstraint[T] = (label, value, messages) => {
    val ord = Ordering[T]; import ord._
    if (value < minVal) {
      Seq("" -> (if (message.isEmpty) messages("error.min") else Some(message)).get.format(label, minVal))
    } else Nil
  }
  
  def max[T: Ordering](maxVal: T, message: String = ""): ExtraConstraint[T] = (label, value, messages) => {
    val ord = Ordering[T]; import ord._
    if (value > maxVal) {
      Seq("" -> (if (message.isEmpty) messages("error.max") else Some(message)).get.format(label, maxVal))
    } else Nil
  }
}

object Constraints extends Constraints {
  /** copied from Play! form/mapping */
  val EMAIL_REGEX = """^(?!\.)("([^"\r\\]|\\["\r\\])*"|([-a-zA-Z0-9!#$%&'*+/=?^_`{|}~]|(?<!\.)\.)*)(?<!\.)@[a-zA-Z0-9][\w\.-]*[a-zA-Z0-9]\.[a-zA-Z][a-zA-Z\.]*[a-zA-Z]$""".r
}