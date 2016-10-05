package com.github.tminglei.bind

import java.time._
import java.time.format.DateTimeFormatter
import java.util.{ResourceBundle, UUID}
import org.scalatest._

class FieldMappingsSpec extends FunSpec with Matchers with Constraints with Processors {
  val bundle: ResourceBundle = ResourceBundle.getBundle("bind-messages")
  val messages: Messages = (key) => Option(bundle.getString(key))

  describe("test pre-defined field mappings") {

    describe("text") {
      val text = trim() >-: Mappings.text()

      it("valid data") {
        val data = Map("text" -> "tett ")
        text.validate("text", data, messages, Options.apply()) match {
          case Nil => text.convert("text", data) should be ("tett")
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        text.validate("text", nullData, messages, Options.apply()) match {
          case Nil => text.convert("text", nullData) should be (null)
          case err => err should be (Nil)
        }
      }

      it("w/ eager check") {
        val text1 = Mappings.text(maxLength(20, "%s: length > %s"), email("%s: invalid email"))
          .options(_.eagerCheck(true))
        val data = Map("text" -> "etttt.att#example-1111111.com")
        text1.validate("text", data, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq(
            "text" -> "etttt.att#example-1111111.com: length > 20",
            "text" -> "etttt.att#example-1111111.com: invalid email"))
        }
      }

      it("w/ ignore empty") {
        val nullData = Map[String, String]()

        val text1 = Mappings.text(required("%s is required"))
        text1.validate("text", nullData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("text" -> "text is required"))
        }

        val text2 = Mappings.text(required("%s is required"))
          .options(_.skipUntouched(true))
        text2.validate("text", nullData, messages, Options.apply()) match {
          case Nil => text.convert("text", nullData) should be (null)
          case err => err should be (Nil)
        }
      }

      it("w/ ignore empty and touched") {
        val nullData = Map[String, String]()

        val text1 = Mappings.text(required("%s is required"))
        text1.validate("text", nullData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("text" -> "text is required"))
        }

        val text2 = Mappings.text(required("%s is required"))
          .options(_.skipUntouched(true))
        text2.validate("text", nullData, messages, Options().touchedChecker(Processors.listTouched(List("text")))) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("text" -> "text is required"))
        }
      }

      it("w/ eager check thru verifying") {
        val text1 = Mappings.text(maxLength(20, "%s: length > %s"), email("%s: invalid email")).verifying()
          .options(_.eagerCheck(true))
        val data = Map("text" -> "etttt.att#example-1111111.com")
        text1.validate("text", data, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq(
            "text" -> "etttt.att#example-1111111.com: length > 20",
            "text" -> "etttt.att#example-1111111.com: invalid email"))
        }
      }

      it("w/ ignore empty thru verifying") {
        val nullData = Map[String, String]()

        val text1 = Mappings.text(required("%s is required")).verifying()
        text1.validate("text", nullData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("text" -> "text is required"))
        }

        val text2 = Mappings.text(required("%s is required")).verifying()
          .options(_.skipUntouched(true))
        text2.validate("text", nullData, messages, Options.apply()) match {
          case Nil => text.convert("text", nullData) should be (null)
          case err => err should be (Nil)
        }
      }

      it("w/ ignore empty and touched thru verifying") {
        val nullData = Map[String, String]()

        val text1 = Mappings.text(required("%s is required")).verifying()
        text1.validate("text", nullData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("text" -> "text is required"))
        }

        val text2 = Mappings.text(required("%s is required")).verifying()
          .options(_.skipUntouched(true))
        text2.validate("text", nullData, messages, Options().touchedChecker(Processors.listTouched(List("text")))) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("text" -> "text is required"))
        }
      }

      it("w/ eager check + transform (mapTo)") {
        val text1 = Mappings.text(maxLength(20, "%s: length > %s"), email("invalid email")).map(identity)
          .options(_.eagerCheck(true))
        val data = Map("text" -> "etttt.att#example-1111111.com")
        text1.validate("text", data, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq(
            "text" -> "etttt.att#example-1111111.com: length > 20",
            "text" -> "invalid email"))
        }
      }

      it("w/ ignore empty + transform (mapTo)") {
        val nullData = Map[String, String]()

        val text1 = Mappings.text(required("%s is required")).map(identity)
        text1.validate("text", nullData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("text" -> "text is required"))
        }

        val text2 = Mappings.text(required("%s is required")).map(identity)
          .options(_.skipUntouched(true))
        text2.validate("text", nullData, messages, Options.apply()) match {
          case Nil => text.convert("text", nullData) should be (null)
          case err => err should be (Nil)
        }
      }

      it("w/ ignore empty and touched + transform (mapTo)") {
        val nullData = Map[String, String]()

        val text1 = Mappings.text(required("%s is required")).map(identity)
        text1.validate("text", nullData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("text" -> "text is required"))
        }

        val text2 = Mappings.text(required("%s is required")).map(identity)
          .options(_.skipUntouched(true))
        text2.validate("text", nullData, messages, Options().touchedChecker(Processors.listTouched(List("text")))) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("text" -> "text is required"))
        }
      }
    }

    describe("boolean") {
      val boolean = Mappings.boolean()

      it("invalid data") {
        val invalidData = Map("boolean" -> "teed")
        boolean.validate("boolean", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("boolean" -> "'teed' must be a boolean"))
        }
      }

      it("valid data") {
        val validData = Map("boolean" -> "true")
        boolean.validate("boolean", validData, messages, Options.apply()) match {
          case Nil => boolean.convert("boolean", validData) should be (true)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        boolean.validate("boolean", nullData, messages, Options.apply()) match {
          case Nil => boolean.convert("boolean", nullData) should be (false)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("boolean" -> "")
        boolean.validate("boolean", emptyData, messages, Options.apply()) match {
          case Nil => boolean.convert("boolean", emptyData) should be (false)
          case err => err should be (Nil)
        }
      }
    }

    describe("int") {
      val int = (omit(",") >-: Mappings.int()).verifying(min(1000), max(10000))

      it("invalid data") {
        val int1 = Mappings.int().label("xx")
        val invalidData = Map("int" -> "t12345")
        int1.validate("int", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("int" -> "'t12345' must be a number"))
        }
      }

      it("out-of-scope data") {
        val outScopeData = Map("int" -> "345")
        int.validate("int", outScopeData, messages, Options.apply()) match {
          case Nil => ("out of scope - shouldn't occur!") should be ("")
          case err => err should be (Seq("int" -> "'345' cannot be lower than 1000"))
        }
      }

      it("long number data") {
        val int1 = Mappings.int()
        val longNumberData = Map("int" -> "146894532240")
        int1.validate("int", longNumberData, messages, Options.apply()) match {
          case Nil => ("long number - shouldn't occur!") should be ("")
          case err => err should be (Seq("int" -> "'146894532240' must be a number"))
        }
      }

      it("valid data with comma") {
        val validData = Map("int" -> "3,549")
        int.validate("int", validData, messages, Options.apply()) match {
          case Nil => int.convert("int", validData) should be (3549)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        int.convert("int", nullData) should be (0)
        int.validate("int", nullData, messages, Options.apply()) match {
          case Nil => ("(null->) 0 - shouldn't occur!") should be ("")
          case err => err should be (Seq("int" -> "'0' cannot be lower than 1000"))
        }
      }

      it("empty data") {
        val emptyData = Map("int" -> "")
        int.convert("int", emptyData) should be (0)
        int.validate("int", emptyData, messages, Options.apply()) match {
          case Nil => ("(empty->) 0 - shouldn't occur!") should be ("")
          case err => err should be (Seq("int" -> "'0' cannot be lower than 1000"))
        }
      }
    }

    describe("double") {
      val double = Mappings.double()

      it("invalid datq") {
        val double1 = double.label("xx")
        val invalidData = Map("double" -> "tesstt")
        double1.validate("double", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("double" -> "'tesstt' must be a number"))
        }
      }

      it("valid data") {
        val validData = Map("double" -> "23545.2355")
        double.validate("double", validData, messages, Options.apply()) match {
          case Nil => double.convert("double", validData) should be (23545.2355d)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        double.validate("double", nullData, messages, Options.apply()) match {
          case Nil => double.convert("double", nullData) should be (0d)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("double" -> "")
        double.validate("double", emptyData, messages, Options.apply()) match {
          case Nil => double.convert("double", emptyData) should be (0d)
          case err => err should be (Nil)
        }
      }
    }

    describe("float") {
      val float = Mappings.float()

      it("invalid data") {
        val float1 = float.label("xx")
        val invalidData = Map("float" -> "tesstt")
        float1.validate("float", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("float" -> "'tesstt' must be a number"))
        }
      }

      it("valid data") {
        val validData = Map("float" -> "23545.2355")
        float.validate("float", validData, messages, Options.apply()) match {
          case Nil => float.convert("float", validData) should be (23545.2355f)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        float.validate("float", nullData, messages, Options.apply()) match {
          case Nil => float.convert("float", nullData) should be (0f)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("float" -> "")
        float.validate("float", emptyData, messages, Options.apply()) match {
          case Nil => float.convert("float", emptyData) should be (0f)
          case err => err should be (Nil)
        }
      }
    }

    describe("long") {
      val long = Mappings.long()

      it("invalid data") {
        val invalidData = Map("long" -> "tesstt")
        long.validate("long", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("long" -> "'tesstt' must be a number"))
        }
      }

      it("valid data") {
        val validData = Map("long" -> "235452355")
        long.validate("long", validData, messages, Options.apply()) match {
          case Nil => long.convert("long", validData) should be (235452355L)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        long.validate("long", nullData, messages, Options.apply()) match {
          case Nil => long.convert("long", nullData) should be (0L)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("long" -> "")
        long.validate("long", emptyData, messages, Options.apply()) match {
          case Nil => long.convert("long", emptyData) should be (0L)
          case err => err should be (Nil)
        }
      }
    }

    describe("bigDecimal") {
      val bigDecimal = Mappings.bigDecimal()

      it("invalid data") {
        val invalidData = Map("bigDecimal" -> "tesstt")
        bigDecimal.validate("bigDecimal", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("bigDecimal" -> "'tesstt' must be a number"))
        }
      }

      it("valid data") {
        val validData = Map("bigDecimal" -> "23545.2355")
        bigDecimal.validate("bigDecimal", validData, messages, Options.apply()) match {
          case Nil => bigDecimal.convert("bigDecimal", validData) should be (BigDecimal("23545.2355"))
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        bigDecimal.validate("bigDecimal", nullData, messages, Options.apply()) match {
          case Nil => bigDecimal.convert("bigDecimal", nullData) should be (BigDecimal("0.0"))
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("bigDecimal" -> "")
        bigDecimal.validate("bigDecimal", emptyData, messages, Options.apply()) match {
          case Nil => bigDecimal.convert("bigDecimal", emptyData) should be (BigDecimal("0.0"))
          case err => err should be (Nil)
        }
      }
    }

    describe("bigInt") {
      val bigInt = Mappings.bigInt()

      it("invalid data") {
        val invalidData = Map("bigInt" -> "tesstt")
        bigInt.validate("bigInt", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("bigInt" -> "'tesstt' must be a number"))
        }
      }

      it("valid data") {
        val validData = Map("bigInt" -> "235452355")
        bigInt.validate("bigInt", validData, messages, Options.apply()) match {
          case Nil => bigInt.convert("bigInt", validData) should be (BigInt("235452355"))
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        bigInt.validate("bigInt", nullData, messages, Options.apply()) match {
          case Nil => bigInt.convert("bigInt", nullData) should be (BigInt("0"))
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("bigInt" -> "")
        bigInt.validate("bigInt", emptyData, messages, Options.apply()) match {
          case Nil => bigInt.convert("bigInt", emptyData) should be (BigInt("0"))
          case err => err should be (Nil)
        }
      }
    }

    describe("uuid") {
      val uuid = Mappings.uuid()

      it("invalid data") {
        val invalidData = Map("uuid" -> "tesstt")
        uuid.validate("uuid", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("uuid" -> "'tesstt' missing or not a valid uuid"))
        }
      }

      it("valid data") {
        val uuidObj = UUID.randomUUID()
        val validData = Map("uuid" -> uuidObj.toString)
        uuid.validate("uuid", validData, messages, Options.apply()) match {
          case Nil => uuid.convert("uuid", validData) should be (uuidObj)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val nullData = Map[String, String]()
        uuid.validate("uuid", nullData, messages, Options.apply()) match {
          case Nil => uuid.convert("uuid", nullData) should be (null)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val emptyData = Map("uuid" -> "")
        uuid.validate("uuid", emptyData, messages, Options.apply()) match {
          case Nil => uuid.convert("uuid", emptyData) should be (null)
          case err => err should be (Nil)
        }
      }
    }

    describe("date") {
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val date = Mappings.date("yyyy-MM-dd").verifying(
        min(LocalDate.parse("2000-01-01", formatter), "min failed"),
        max(LocalDate.parse("2015-01-01", formatter), "max failed"))

      it("invalid data") {
        val date1 = Mappings.date("yyyy-MM-dd").label("xx")
        val invalidData = Map("date" -> "5/3/2003")
        date1.validate("date", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("date" -> "'xx' must satisfy any of following: ['5/3/2003' not a date long, '5/3/2003' must be 'yyyy-MM-dd']"))
        }
      }

      it("out-of-scope data") {
        val outScopeData = Map("date" -> "1998-07-01")
        date.validate("date", outScopeData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("date" -> "min failed"))
        }
      }

      it("valid data") {
        val validData = Map("date" -> "2007-08-03")
        date.validate("date", validData, messages, Options.apply()) match {
          case Nil => date.convert("date", validData) should be (LocalDate.parse("2007-08-03", formatter))
          case err => err should be (Nil)
        }
      }

      it("valid data - long") {
        val dateMapping = Mappings.date()
        val ts = System.currentTimeMillis()
        val dateObj = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.of("UTC")).toLocalDate
        val validData = Map("date" -> ts.toString)
        dateMapping.validate("date", validData, messages, Options.apply()) match {
          case Nil => dateMapping.convert("date", validData) should be (dateObj)
          case err => err should be (Nil)
        }
      }

      it("valid data - default format") {
        val dateMapping = Mappings.date()
        val dateObj = LocalDateTime.ofInstant(Instant.now(), ZoneId.of("UTC")).toLocalDate
        val validData = Map("date" -> dateObj.toString)
        dateMapping.validate("date", validData, messages, Options.apply()) match {
          case Nil => dateMapping.convert("date", validData) should be (dateObj)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val date1 = Mappings.date("yyyy-MM-dd")
        val nullData = Map[String, String]()
        date1.validate("date", nullData, messages, Options.apply()) match {
          case Nil => date1.convert("date", nullData) should be (null)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val date1 = Mappings.date("yyyy-MM-dd")
        val emptyData = Map("date" -> "")
        date1.validate("date", emptyData, messages, Options.apply()) match {
          case Nil => date1.convert("date", emptyData) should be (null)
          case err => err should be (Nil)
        }
      }
    }

    describe("datetime") {
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
      val datetime = Mappings.datetime("yyyy-MM-dd'T'HH:mm:ss.SSS").verifying(
        min(LocalDateTime.parse("2001-01-03T13:21:00.223", formatter), "min failed"),
        max(LocalDateTime.parse("2012-01-03T13:21:00.102", formatter), "max failed"))

      it("invalid data") {
        val datetime1 = Mappings.datetime("yyyy-MM-dd'T'HH:mm:ss.SSS").label("xx")
        val invalidData = Map("datetime" -> "5/3/2003 13:21:00")
        datetime1.validate("datetime", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("datetime" -> "'xx' must satisfy any of following: ['5/3/2003 13:21:00' not a date long, '5/3/2003 13:21:00' must be 'yyyy-MM-dd'T'HH:mm:ss.SSS']"))
        }
      }

      it("out-of-scope data") {
        val outScopeData = Map("datetime" -> "1998-07-01T13:21:00.223")
        datetime.validate("datetime", outScopeData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("datetime" -> "min failed"))
        }
      }

      it("valid data") {
        val validData = Map("datetime" -> "2007-08-03T13:21:00.223")
        datetime.validate("datetime", validData, messages, Options.apply()) match {
          case Nil => datetime.convert("datetime", validData) should be (LocalDateTime.parse("2007-08-03T13:21:00.223", formatter))
          case err => err should be (Nil)
        }
      }

      it("valid data - long") {
        val dateMapping = Mappings.datetime()
        val ts = System.currentTimeMillis()
        val dateObj = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.of("UTC"))
        val validData = Map("datetime" -> ts.toString)
        dateMapping.validate("datetime", validData, messages, Options.apply()) match {
          case Nil => dateMapping.convert("datetime", validData) should be (dateObj)
          case err => err should be (Nil)
        }
      }

      it("valid data - default format") {
        val dateMapping = Mappings.datetime()
        val dateObj = LocalDateTime.ofInstant(Instant.now(), ZoneId.of("UTC"))
        val validData = Map("datetime" -> dateObj.toString)
        dateMapping.validate("datetime", validData, messages, Options.apply()) match {
          case Nil => dateMapping.convert("datetime", validData) should be (dateObj)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val datetime1 = Mappings.datetime("yyyy-MM-dd'T'HH:mm:ss.SSS")
        val nullData = Map[String, String]()
        datetime1.validate("datetime", nullData, messages, Options.apply()) match {
          case Nil => datetime1.convert("datetime", nullData) should be (null)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val datetime1 = Mappings.datetime("yyyy-MM-dd'T'HH:mm:ss.SSS")
        val emptyData = Map("datetime" -> "")
        datetime1.validate("datetime", emptyData, messages, Options.apply()) match {
          case Nil => datetime1.convert("datetime", emptyData) should be (null)
          case err => err should be (Nil)
        }
      }
    }

    describe("time") {
      val formatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")
      val time = Mappings.time("HH:mm:ss.SSS").verifying(
        min(LocalTime.parse("02:33:01.101", formatter), "min failed"),
        max(LocalTime.parse("12:33:01.101", formatter), "max failed"))

      it("invalid data") {
        val time1 = Mappings.time("HH:mm:ss.SSS").label("xx")
        val invalidData = Map("time" -> "13:21:00")
        time1.validate("time", invalidData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("time" -> "'xx' must satisfy any of following: ['13:21:00' not a date long, '13:21:00' must be 'HH:mm:ss.SSS']"))
        }
      }

      it("out-of-scope data") {
        val outScopeData = Map("time" -> "13:21:00.333")
        time.validate("time", outScopeData, messages, Options.apply()) match {
          case Nil => ("invalid - shouldn't occur!") should be ("")
          case err => err should be (Seq("time" -> "max failed"))
        }
      }

      it("valid data") {
        val validData = Map("time" -> "11:21:00.213")
        time.validate("time", validData, messages, Options.apply()) match {
          case Nil => time.convert("time", validData) should be (LocalTime.parse("11:21:00.213", formatter))
          case err => err should be (Nil)
        }
      }

      it("valid data - long") {
        val dateMapping = Mappings.time()
        val ts = System.currentTimeMillis()
        val dateObj = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.of("UTC")).toLocalTime
        val validData = Map("time" -> ts.toString)
        dateMapping.validate("time", validData, messages, Options.apply()) match {
          case Nil => dateMapping.convert("time", validData) should be (dateObj)
          case err => err should be (Nil)
        }
      }

      it("valid data - default format") {
        val dateMapping = Mappings.time()
        val dateObj = LocalDateTime.ofInstant(Instant.now(), ZoneId.of("UTC")).toLocalTime
        val validData = Map("time" -> dateObj.toString)
        dateMapping.validate("time", validData, messages, Options.apply()) match {
          case Nil => dateMapping.convert("time", validData) should be (dateObj)
          case err => err should be (Nil)
        }
      }

      it("null data") {
        val time1 = Mappings.time("HH:mm:ss.SSS")
        val nullData = Map[String, String]()
        time1.validate("time", nullData, messages, Options.apply()) match {
          case Nil => time1.convert("time", nullData) should be (null)
          case err => err should be (Nil)
        }
      }

      it("empty data") {
        val time1 = Mappings.date("HH:mm:ss.SSS")
        val emptyData = Map("time" -> "")
        time1.validate("time", emptyData, messages, Options.apply()) match {
          case Nil => time1.convert("time", emptyData) should be (null)
          case err => err should be (Nil)
        }
      }
    }
  }
}
