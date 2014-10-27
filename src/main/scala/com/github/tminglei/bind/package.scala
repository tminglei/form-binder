package com.github.tminglei

package object bind {

  // (messageKey) => [message] (ps: all input parameter were ensured NOT NULL/EMPTY)
  type Messages = (String) => Option[String]

  // (label, name, data, message) => errors (ps: all input parameter were ensured NOT NULL/EMPTY)
  type Constraint = (String, String, Map[String, String], Messages) => Seq[(String, String)]

  // (label, vObject, messages) => errors (ps: all input parameter were ensured NOT NULL/EMPTY)
  type ExtraConstraint[T] = (String, T, Messages) => Seq[(String, String)]

  // (prefix, data) => data (ps: all input parameter were ensured NOT NULL/EMPTY)
  type PreDataProcessor = (String, Map[String, String]) => Map[String, String]

  // (data) => touched list (ps: all input parameter were ensured NOT NULL/EMPTY)
  type TouchedExtractor = (Map[String, String]) => Seq[String]

  // (errors) => R (ps: all input parameter were ensured NOT NULL/EMPTY)
  type PostErrProcessor[R] = (Seq[(String, String)]) => R

}
