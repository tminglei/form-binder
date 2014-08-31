package com.github.tminglei.bind

trait Processors {

  //////////////////////////////////////  pre-defined pre-processor implementations  ///////////////////////////

  val trim: PreProcessor = (input: String) => {
    if (input == null) null else input.trim
  }

  val cleanComma: PreProcessor = (input: String) => {
    if (input == null) null else input.replaceAll(",", "")
  }

  val cleanHyphen: PreProcessor = (input: String) => {
    if (input == null) null else input.replaceAll("-", "")
  }

  def cleanPrefix(prefix: String): PreProcessor = (input: String) => {
    if (input == null) null else input.replaceAll("^"+patternStr(prefix), "")
  }

  def cleanPostfix(postfix: String): PreProcessor = (input: String) => {
    if (input == null) null else input.replaceAll(patternStr(postfix)+"$", "")
  }

  val cleanRedundantSpaces: PreProcessor = (input: String) => {
    if (input == null) null else input.replaceAll("[ ]+", " ")
  }

  protected def patternStr(str: String): String = str.map {
    ch => ch match {
      case '^'|'$'|'.'|'['|']'|'('|')' => "\\"+ch
      case _ => ""+ch
    }
  }.mkString("")
}

object Processors extends Processors