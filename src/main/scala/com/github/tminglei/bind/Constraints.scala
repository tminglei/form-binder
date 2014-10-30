package com.github.tminglei.bind

import scala.util.matching.Regex

trait Constraints {
  import FrameworkUtils._
  ////////////////////////////////////////////  pre-defined constraints  ////////////////////////////////////

  def required(message: String = ""): Constraint with OneInput with MultiInput =
    new Constraint with OneInput with MultiInput {
      def apply(name: String, data: Map[String, String], messages: Messages, options: Options) =
        if (isEmptyInput(name, data, options._multiInput)) {
          Seq( name -> (if (message.isEmpty) messages("error.required") else Some(message))
            .get.format(getLabel(name, messages, options)))
        } else Nil
    }

  def maxlength(length: Int, message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (value != null && value.length > length) {
      Some( (if (message.isEmpty) messages("error.maxlength") else Some(message)).get.format(value, length))
    } else None)

  def minlength(length: Int, message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (value != null && value.length < length) {
      Some( (if (message.isEmpty) messages("error.minlength") else Some(message)).get.format(value, length))
    } else None)

  def length(length: Int, message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (value != null && value.length != length) {
      Some( (if (message.isEmpty) messages("error.length") else Some(message)).get.format(value, length))
    } else None)

  def oneOf(values: Seq[String], message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (!values.contains(value)) {
      Some( (if (message.isEmpty) messages("error.oneOf") else Some(message))
        .get.format(value, values.map("'" + _ + "'").mkString(", ")) )
    } else None)

  def pattern(regex: Regex, message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (value != null && regex.findFirstIn(value).isEmpty) {
      Some( (if (message.isEmpty) messages("error.pattern") else Some(message)).get.format(value, regex.toString))
    } else None)

  def patternNot(regex: Regex, message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (value != null && regex.findFirstIn(value).isDefined) {
      Some( (if (message.isEmpty) messages("error.patternnot") else Some(message)).get.format(value, regex.toString))
    } else None)

  def email(message: String = ""): Constraint with OneInput = pattern(EMAIL_REGEX, message)

  def numArrayIndex(message: String = ""): Constraint with MultiInput =
    new Constraint with MultiInput {
      def apply(name: String, data: Map[String, String], messages: Messages, options: Options) =
        data.filter(_._1.startsWith(name)).map { case (key, value) =>
          ILLEGAL_ARRAY_INDEX.findFirstIn(key).map { m =>
            (key -> (NAME_ERR_PREFIX + (if (message.isEmpty) messages("error.arrayindex") else Some(message)).get).format(key))
          }
        }.filterNot(_.isEmpty).map(_.get).toSeq
    }

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

object Constraints extends Constraints