package com.github.tminglei

package object bind {

  type Messages = String => String

  type Constraint1[T] = (T, Messages) => Seq[(String, String)]

}
