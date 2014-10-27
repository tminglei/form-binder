package com.github.tminglei.bind

import org.json4s.{JValue, JNothing, JNull}
import org.json4s.jackson.JsonMethods
import org.scalatest._

class ConstraintsSpec extends FunSpec with ShouldMatchers {

  describe("test pre-defined constraints") {
    val dummyMessages: Messages = (key) => Some("dummy")

    describe("required") {
      it("simple use") {
        val required = Constraints.required()
        required("", "", Map("" -> null), dummyMessages).toList should be (List("" -> "dummy"))
        required("", "", Map("" -> ""), dummyMessages).toList should be (List("" -> "dummy"))
        required("", "", Map("" -> "test"), dummyMessages).toList should be (Nil)
      }

      it("with custom message") {
        val required1 = Constraints.required("%s is required")
        required1("haha", "tt.a", Map("tt.a" -> null), dummyMessages).toList should be (List("tt.a" -> "haha is required"))
      }
    }

    describe("maxlength") {
      it("simple use") {
        val maxlength = Constraints.maxlength(10)
        maxlength("", "", Map("" -> "wetyyuu"), dummyMessages).toList should be (Nil)
        maxlength("", "", Map("" -> "wetyettyiiie"), dummyMessages).toList should be (List("" -> "dummy"))
        maxlength("", "", Map("" -> "tuewerri97"), dummyMessages).toList should be (Nil)
      }

      it("with custom message") {
        val maxlength1 = Constraints.maxlength(10, "'%s': length > %d")
        maxlength1("haha", "", Map("" -> "eewryuooerjhy"), dummyMessages).toList should be (List("" -> "'eewryuooerjhy': length > 10"))
      }
    }

    describe("minlength") {
      it("simple use") {
        val minlength = Constraints.minlength(3)
        minlength("", "", Map("" -> "er"), dummyMessages).toList should be (List("" -> "dummy"))
        minlength("", "", Map("" -> "ert6"), dummyMessages).toList should be (Nil)
        minlength("", "", Map("" -> "tee"), dummyMessages).toList should be (Nil)
      }

      it("with custom message") {
        val minlength1 = Constraints.minlength(3, "'%s': length cannot < %d")
        minlength1("haha", "", Map("" -> "te"), dummyMessages).toList should be (List("" -> "'te': length cannot < 3"))
      }
    }

    describe("length") {
      it("simple use") {
        val length = Constraints.length(9)
        length("", "", Map("" -> "123456789"), dummyMessages).toList should be (Nil)
        length("", "", Map("" -> "123"), dummyMessages).toList should be (List("" -> "dummy"))
        length("", "", Map("" -> "1234567890"), dummyMessages).toList should be (List("" -> "dummy"))
      }

      it("with custom message") {
        val length1 = Constraints.length(9, "'%s': length not equal to %d")
        length1("haha", "", Map("" -> "123"), dummyMessages).toList should be (List("" -> "'123': length not equal to 9"))
      }
    }

    describe("oneOf") {
      it("simple use") {
        val oneof = Constraints.oneOf(Seq("a","b","c"))
        oneof("", "", Map("" -> "a"), dummyMessages).toList should be (Nil)
        oneof("", "", Map("" -> "t"), dummyMessages).toList should be (List("" -> "dummy"))
        oneof("", "", Map("" -> null), dummyMessages).toList should be (List("" -> "dummy"))
      }

      it("with custom message") {
        val oneof1 = Constraints.oneOf(Seq("a","b","c"), "'%s': is not one of %s")
        oneof1("haha", "t.a", Map("t.a" -> "ts"), dummyMessages).toList should be (List("t.a" -> "'ts': is not one of 'a', 'b', 'c'"))
      }
    }

    describe("pattern") {
      it("simple use") {
        val pattern = Constraints.pattern("^(\\d+)$".r)
        pattern("", "", Map("" -> "1234657"), dummyMessages).toList should be (Nil)
        pattern("", "", Map("" -> "32566y"), dummyMessages).toList should be (List("" -> "dummy"))
        pattern("", "", Map("" -> "123,567"), dummyMessages).toList should be (List("" -> "dummy"))
      }

      it("with custom message") {
        val pattern1 = Constraints.pattern("^(\\d+)$".r, "'%s' not match '%s'")
        pattern1("haha", "", Map("" -> "t4366"), dummyMessages).toList should be (List("" -> "'t4366' not match '^(\\d+)$'"))
      }
    }

    describe("patternNot") {
      it("simple use") {
        val pattern = Constraints.patternNot(""".*\[(\d*[^\d\[\]]+\d*)+\].*""".r)
        pattern("", "", Map("" -> "eree.[1234657].eee"), dummyMessages).toList should be (Nil)
        pattern("", "", Map("" -> "errr.[32566y].ereee"), dummyMessages).toList should be (List("" -> "dummy"))
      }

      it("with custom message") {
        val pattern1 = Constraints.pattern("^(\\d+)$".r, "'%s' contains illegal array index")
        pattern1("haha", "", Map("" -> "ewtr.[t4366].eweee"), dummyMessages)
          .toList should be (List("" -> "'ewtr.[t4366].eweee' contains illegal array index"))
      }
    }

    /**
     * test cases copied from:
     * http://en.wikipedia.org/wiki/Email_address
     */
    describe("email") {
      val email = Constraints.email("'%s' not valid")

      it("valid email addresses") {
        List(
          "niceandsimple@example.com",
          "very.common@example.com",
          "a.little.lengthy.but.fine@dept.example.com",
          "disposable.style.email.with+symbol@example.com",
          "other.email-with-dash@example.com"//,
          //        "user@localserver",
          // internationalization examples
          //        "Pelé@example.com",  //Latin Alphabet (with diacritics)
          //        "δοκιμή@παράδειγμα.δοκιμή", //Greek Alphabet
          //        "我買@屋企.香港",  //Traditional Chinese Characters
          //        "甲斐@黒川.日本",  //Japanese Characters
          //        "чебурашка@ящик-с-апельсинами.рф"  //Cyrillic Characters
        ).map { emailAddr =>
          email("", "", Map("" -> emailAddr), dummyMessages).toList should be (Nil)
        }
      }

      it("invalid email addresses") {
        List(
          "Abc.example.com", //(an @ character must separate the local and domain parts)
          "A@b@c@example.com", //(only one @ is allowed outside quotation marks)
          "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com", //(none of the special characters in this local part is allowed outside quotation marks)
          "just\"not\"right@example.com", //(quoted strings must be dot separated or the only element making up the local-part)
          """this is"not\allowed@example.com""", //(spaces, quotes, and backslashes may only exist when within quoted strings and preceded by a backslash)
          """this\ still\"not\\allowed@example.com""", //(even if escaped (preceded by a backslash), spaces, quotes, and backslashes must still be contained by quotes)
          "john..doe@example.com", //(double dot before @)
          "john.doe@example..com" //(double dot after @)
        ).map { emailAddr =>
          email("", "", Map("" -> emailAddr), dummyMessages).toList should be (List("" -> s"'$emailAddr' not valid"))
        }
      }
    }

    describe("numArrayIndex") {
      it("simple use") {
        val numArrayIndex = Constraints.numArrayIndex()
        numArrayIndex("xx", "a", Map("a[0]" -> "aaa"), dummyMessages).toList should be (Nil)
        numArrayIndex("", "a", Map("a[t0]" -> "aaa", "a[3]" -> "tew"), dummyMessages).toList should be (List("a[t0]" -> "name: dummy"))
        numArrayIndex("xx", "a", Map("a[t1]" -> "aewr", "a[t4]" -> "ewre"), dummyMessages)
          .toList should be (List("a[t1]" -> "name: dummy", "a[t4]" -> "name: dummy"))
      }

      it("w/ custom message") {
        val numArrayIndex = Constraints.numArrayIndex("illegal array index")
        numArrayIndex("xx", "a", Map("a[0]" -> "aaa"), dummyMessages).toList should be (Nil)
        numArrayIndex("", "a", Map("a[t0]" -> "aaa", "a[3]" -> "tew"), dummyMessages).toList should be (List("a[t0]" -> "name: illegal array index"))
        numArrayIndex("xx", "a", Map("a[t1]" -> "aewr", "a[t4].er" -> "ewre"), dummyMessages)
          .toList should be (List("a[t1]" -> "name: illegal array index", "a[t4].er" -> "name: illegal array index"))
      }
    }
  }

  describe("test pre-defined extra constraints") {
    val dummyMessages: Messages = (key) => Some("dummy")

    describe("notEmpty") {
      it("for seq, with custom message") {
        val notEmpty = Constraints.notEmpty[List[Int]](message = "%s is required")
        notEmpty("xx", List(1), dummyMessages) should be (Nil)
        notEmpty("xx", Nil, dummyMessages) should be (Seq("" -> "xx is required"))
      }

      it("for map, with custom message") {
        val notEmpty = Constraints.notEmpty[Map[String,Int]](message = "%s is required")
        notEmpty("xx", Map("aa" -> 1), dummyMessages) should be (Nil)
        notEmpty("xx", Map(), dummyMessages) should be (Seq("" -> "xx is required"))
      }

      it("for json, with custom message") {
        val notEmpty = Constraints.notEmpty[JValue](message = "%s is required")
        notEmpty("xx", JsonMethods.parse("{}"), dummyMessages) should be (Nil)
        notEmpty("xx", JNull, dummyMessages) should be (Seq("" -> "xx is required"))
        notEmpty("xx", JNothing, dummyMessages) should be (Seq("" -> "xx is required"))
      }

      it("for string, with custom message") {
        val notEmpty = Constraints.notEmpty[String](message = "%s is required")
        notEmpty("xx", "sere", dummyMessages) should be (Nil)
        notEmpty("xx", "", dummyMessages) should be (Seq("" -> "xx is required"))
        notEmpty("xx", null, dummyMessages) should be (Seq("" -> "xx is required"))
      }

      it("for seq, with custom isEmpty") {
        val notEmpty = Constraints.notEmpty[List[Int]](_.isEmpty)
        notEmpty("xx", List(1), dummyMessages) should be (Nil)
        notEmpty("xx", Nil, dummyMessages) should be (Seq("" -> "dummy"))
      }
    }

    describe("min") {
      it("for int, with custom message") {
        val min = Constraints.min(5, "%s cannot < %s")
        min("xx", 6, dummyMessages) should be (Nil)
        min("xx", 3, dummyMessages) should be (Seq("" -> "xx cannot < 5"))
      }

      it("for double, with custom message") {
        val min1 = Constraints.min(5.5d, "%s cannot < %s")
        min1("xx", 6d, dummyMessages) should be (Nil)
        min1("xx", 3d, dummyMessages) should be (Seq("" -> "xx cannot < 5.5"))
      }
    }

    describe("max") {
      it("for int, with custom message") {
        val max = Constraints.max(15, "%s cannot > %s")
        max("xx", 6, dummyMessages) should be (Nil)
        max("xx", 23, dummyMessages) should be (Seq("" -> "xx cannot > 15"))
      }

      it("for double, with custom message") {
        val max1 = Constraints.max(35.5d, "%s cannot > %s")
        max1("xx", 26d, dummyMessages) should be (Nil)
        max1("xx", 37d, dummyMessages) should be (Seq("" -> "xx cannot > 35.5"))
      }
    }
  }
}
