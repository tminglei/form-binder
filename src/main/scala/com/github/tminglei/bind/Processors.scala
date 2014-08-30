package com.github.tminglei.bind

trait Processors {

  def trim(input: String): String = if (input == null) null else input.trim

  def rmRedundantSpaces(input: String): String = if (input == null) null else input.replaceAll("[ ]+", " ")

}
