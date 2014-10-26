package com.github.tminglei

package object bind {

  // (messageKey) => [message]
  type Messages = (String) => Option[String]

  // (label, name, data, message) => errors
  type Constraint = (String, String, Map[String, String], Messages) => Seq[(String, String)]

  // (label, vObject, messages) => errors
  type ExtraConstraint[T] = (String, T, Messages) => Seq[(String, String)]

  // (prefix, data) => data
  type PreDataProcessor = (String, Map[String, String]) => Map[String, String]

  // (data) => touched list
  type TouchedExtractor = (Map[String, String]) => Seq[String]

  // (errors) => R
  type PostErrProcessor[R] = (Seq[(String, String)]) => R

}
