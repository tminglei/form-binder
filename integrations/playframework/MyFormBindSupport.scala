package com.example

import play.api.mvc._
import com.github.tminglei.bind.Messages
import com.github.tminglei.bind.simple._

trait MyFormBindSupport { self =>
  /**
   * Extracted and copied from [[play.api.data.Form]]
   */
  def params(implicit request: Request[_]): Map[String, String] = {
    // convert to `Map[String, Seq[String]]`
    ((request.body match {
      case body: play.api.mvc.AnyContent if body.asFormUrlEncoded.isDefined => body.asFormUrlEncoded.get
      case body: play.api.mvc.AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.asFormUrlEncoded
      case body: play.api.mvc.AnyContent if body.asJson.isDefined => FormUtils.fromJson(js = body.asJson.get).mapValues(Seq(_))
      case body: Map[_, _] => body.asInstanceOf[Map[String, Seq[String]]]
      case body: play.api.mvc.MultipartFormData[_] => body.asFormUrlEncoded
      case body: play.api.libs.json.JsValue => data.FormUtils.fromJson(js = body).mapValues(Seq(_))
      case _ => Map.empty[String, Seq[String]]
    }) ++ request.queryString)
      // continue to convert to `Map[String, String]`
      .foldLeft(Map.empty[String, String]) {
        case (s, (key, values)) if key.endsWith("[]") => s ++ values.zipWithIndex.map { case (v, i) => (key.dropRight(2) + "[" + i + "]") -> v }
        case (s, (key, values)) => s + (key -> values.headOption.getOrElse(""))
      }
  }

  def binder(implicit request: Request) =
    expandJsonString(Some("json")) >-: FormBinder(getMessages()).withErr(errsToJson4s)

  ///TODO prepare your i18n Messages
  private def getMessages(implicit request: Request): Messages = ???
}
