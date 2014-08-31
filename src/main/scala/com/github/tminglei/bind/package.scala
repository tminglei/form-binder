package com.github.tminglei

package object bind {

  // (messageKey) => message
  type Messages = (String) => String

  // (vObject, messages) => errors
  type ExtraConstraint[T] = (T, Messages) => Seq[(String, String)]

  // (input) => output
  type PreProcessor = (String) => String

  // (data) => data
  type BulkPreProcessor = (Map[String, String]) => Map[String, String]

  // (errors) => R
  type PostErrProcessor[R] = (Seq[(String, String)]) => R

}
