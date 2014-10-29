package com.github.tminglei

package object bind {

  // (messageKey) => [message] (ps: all input parameters WON'T BE NULL/EMPTY)
  type Messages = (String) => Option[String]

  // (name, data, messages, options) => errors (ps: all input parameters WON'T BE NULL/EMPTY)
  type Constraint = (String, Map[String, String], Messages, Options) => Seq[(String, String)]

  // (label, vObject, messages) => errors (ps: all input parameters WON'T BE NULL/EMPTY)
  type ExtraConstraint[T] = (String, T, Messages) => Seq[(String, String)]

  // (prefix, data, options) => data (ps: all input parameters WON'T BE NULL/EMPTY)
  type PreProcessor = (String, Map[String, String], Options) => Map[String, String]

  // (data) => touched list (ps: all input parameters WON'T BE NULL/EMPTY)
  type TouchedExtractor = (Map[String, String]) => Seq[String]

  // (errors) => R (ps: all inputs parameter WON'T BE NULL/EMPTY)
  type PostErrProcessor[R] = (Seq[(String, String)]) => R

}
