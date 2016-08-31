package com.github.tminglei.bind

import org.scalatest._
import simple._

class AttachmentSpec extends FunSpec with Matchers {

  describe("test mapping extension support") {
    it("simple test") {
      tmapping(
        "id" -> long().$.in("path").$$.$.desc("pet id").$$,
        "name" -> text().$.in("query").desc("pet name").$$
      ).fields.foreach {
        case ("id", id) => id.options._attachment.orNull should be (Attachment(Some("path"), Some("pet id")))
        case ("name", name) => name.options._attachment.orNull should be (Attachment(Some("query"), Some("pet name")))
      }
    }
  }

  ///---

  case class Attachment(
    _in: Option[String] = None,
    _desc: Option[String] = None
  )

  case class AttachmentBuilder[T](mapping: Mapping[T], attachment: Attachment) {
    def in(in: String) = copy(mapping, attachment.copy(_in = Some(in)))
    def desc(desc: String) = copy(mapping, attachment.copy(_desc = Some(desc)))
    def $$ = mapping.options(_.copy(_attachment = Some(attachment)))
  }

  implicit class AttachmentImplicit[T](mapping: Mapping[T]) {
    def $ = AttachmentBuilder(mapping, mapping.options._attachment.getOrElse(Attachment()).asInstanceOf[Attachment])
  }

}
