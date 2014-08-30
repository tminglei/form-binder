package com.github.tminglei

package object bind {

  // key => message
  type Messages = String => String

  // (vObject, messages) => errors
  type ExtraConstraint[T] = (T, Messages) => Seq[(String, String)]

  // input => output
  type PreProcessor = String => String

  // (prefix, params) => params
  type BulkPreProcessor = (String, Map[String, String]) => Map[String, String]

}
