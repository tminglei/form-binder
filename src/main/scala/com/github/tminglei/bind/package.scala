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

  /**
   * A helper object, used to simplify `form-binder` usage
   *
   * Note: add {{{import com.github.tminglei.bind.simple._}}} to your class, then
   *   you can use form binder's built-in mappings/constraints/processors directly
   */
  object simple extends Mappings with Constraints with Processors {
    type FormBinder[R] = com.github.tminglei.bind.FormBinder[R]
    val  FormBinder = com.github.tminglei.bind.FormBinder
  }
}
