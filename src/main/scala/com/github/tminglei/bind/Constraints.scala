package com.github.tminglei.bind

import scala.util.matching.Regex

trait Constraints {
  import FrameworkUtils._

  /////////////////////////////////////////  pre-defined constraints  ///////////////////////////////

  def required(message: String = ""): Constraint =
    (name, data, messages, options) => {
      if (isEmptyInput(name, data, options._inputMode)) {
        val errMsg =
          if (!isEmptyInput(name, data, PolyInput)) {
            val msgTemplate = messages("error.wronginput").get
            val simple = getLabel("simple", messages, options)
            val compound = getLabel("compound", messages, options)

            if (options._inputMode == SoloInput)
              msgTemplate.format(simple, compound)
            else
              msgTemplate.format(compound, simple)
          } else {
            (if (message.isEmpty) messages("error.required") else Some(message))
              .get.format(getLabel(name, messages, options))
          }

        Seq((name, errMsg))
      } else Nil
    }

  def maxLength(length: Int, message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (value != null && value.length > length) {
      Some( (if (message.isEmpty) messages("error.maxlength") else Some(message))
        .get.format(value, length))
    } else None)

  def minLength(length: Int, message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (value != null && value.length < length) {
      Some( (if (message.isEmpty) messages("error.minlength") else Some(message))
        .get.format(value, length))
    } else None)

  def length(length: Int, message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (value != null && value.length != length) {
      Some( (if (message.isEmpty) messages("error.length") else Some(message))
        .get.format(value, length))
    } else None)

  def oneOf(values: Seq[String], message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (!values.contains(value)) {
      Some( (if (message.isEmpty) messages("error.oneof") else Some(message))
        .get.format(value, values.map("'" + _ + "'").mkString(", ")) )
    } else None)

  def email(message: String = "") = pattern(EMAIL_REGEX, message)

  def pattern(regex: Regex, message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (value != null && regex.findFirstIn(value).isEmpty) {
      Some( (if (message.isEmpty) messages("error.pattern") else Some(message))
        .get.format(value, regex.toString))
    } else None)

  def patternNot(regex: Regex, message: String = "") = mkSimpleConstraint((label, value, messages) =>
    if (value != null && regex.findFirstIn(value).isDefined) {
      Some( (if (message.isEmpty) messages("error.patternnot") else Some(message))
        .get.format(value, regex.toString))
    } else None)

  def indexInKeys(message: String = ""): Constraint =
    (name, data, messages, options) => {
      data.filter(_._1.startsWith(name)).map { case (key, value) =>
        ILLEGAL_ARRAY_INDEX.findFirstIn(key).map { m =>
          (key -> (if (message.isEmpty) messages("error.arrayindex") else Some(message)).get.format(key))
        }
      }.filterNot(_.isEmpty).map(_.get).toSeq
    }

  ///////////////////////////////////////  pre-defined extra constraints  ////////////////////////////

  def min[T: Ordering](minVal: T, message: String = ""): ExtraConstraint[T] =
    (label, value, messages) => {
      val ord = Ordering[T]; import ord._
      if (value < minVal) {
        Seq((if (message.isEmpty) messages("error.min") else Some(message))
          .get.format(value, minVal))
      } else Nil
    }
  
  def max[T: Ordering](maxVal: T, message: String = ""): ExtraConstraint[T] =
    (label, value, messages) => {
      val ord = Ordering[T]; import ord._
      if (value > maxVal) {
        Seq((if (message.isEmpty) messages("error.max") else Some(message))
          .get.format(value, maxVal))
      } else Nil
    }
}

object Constraints extends Constraints