package com.github.tminglei.bind

import org.scalatest._
import simple._

class MappingExtSpec extends FunSpec with ShouldMatchers {
  case class Ext(
    _in: Option[String] = None,
    _desc: Option[String] = None
    ) extends Extensible {
    def in(in: String) = copy(_in = Some(in))
    def desc(desc: String) = copy(_desc = Some(desc))
  }

  implicit class ExtHelper(_ext: Extensible) {
    def $ = if (_ext == null) Ext() else _ext.asInstanceOf[Ext]
  }

  describe("test mapping extension support") {
    it("simple test") {
      tmapping(
        "id" -> long().$ext(_.$.in("path").desc("pet id")),
        "name" -> text().$ext(_.$.in("query").desc("pet name"))
      ).fields.map {
        case ("id", id) => id.options._extData.orNull should be (Ext(Some("path"), Some("pet id")))
        case ("name", name) => name.options._extData.orNull should be (Ext(Some("query"), Some("pet name")))
      }
    }
  }
}
