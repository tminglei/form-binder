package com.github.tminglei.bind

import java.util.UUID
import java.util.regex.Pattern

trait Mappings {

  ////////////////////////////////////////////  pre-defined field mappings  ///////////////////////////////////

  def text(constraints: Constraint*): FieldMapping[String] =
    new FieldMapping[String](constraints, convert = identity)

  def boolean(constraints: Constraint*): FieldMapping[Boolean] =
    new FieldMapping[Boolean](parsing(_.toBoolean, "error.boolean") +: constraints,
      convert = (value: String) => value match {
        case null|"" => false
        case x => x.toBoolean
      })

  def number(constraints: Constraint*): FieldMapping[Int] =
    new FieldMapping[Int](parsing(_.toInt, "error.number") +: constraints,
      convert = (value: String) => value match {
        case null|"" => 0
        case x => x.toInt
      })

  def double(constraints: Constraint*): FieldMapping[Double] =
    new FieldMapping[Double](parsing(_.toDouble, "error.double") +: constraints,
      convert = (value: String) => value match {
        case null|"" => 0d
        case x => x.toDouble
      })

  def float(constraints: Constraint*): FieldMapping[Float] =
    new FieldMapping[Float](parsing(_.toFloat, "error.float") +: constraints,
      convert = (value: String) => value match {
        case null|"" => 0f
        case x => x.toFloat
      })

  def long(constraints: Constraint*): FieldMapping[Long] =
    new FieldMapping[Long](parsing(_.toLong, "error.long") +: constraints,
      convert = (value: String) => value match {
        case null|"" => 0l
        case x => x.toLong
      })

  def bigDecimal(constraints: Constraint*): FieldMapping[BigDecimal] =
    new FieldMapping[BigDecimal](parsing(BigDecimal.apply, "error.bigdecimal") +: constraints,
      convert = (value: String) => value match {
        case null|"" => 0d
        case x => BigDecimal(x)
      })

  def bigInt(constraints: Constraint*): FieldMapping[BigInt] =
    new FieldMapping[BigInt](parsing(BigInt.apply, "error.bigint") +: constraints,
      convert = (value: String) => value match {
        case null|"" => 0l
        case x => BigInt(x)
      })

  def uuid(constraints: Constraint*): FieldMapping[UUID] =
    new FieldMapping[UUID](parsing(UUID.fromString, "error.uuid") +: constraints,
      convert = (value: String) => value match {
        case null|"" => null
        case x => UUID.fromString(x)
      })

  def date(pattern: String, constraints: Constraint*): FieldMapping[java.util.Date] = {
    val dateFormatter = new java.text.SimpleDateFormat(pattern)
    new FieldMapping[java.util.Date](parsing(dateFormatter.parse, "error.pattern", pattern) +: constraints,
      convert = (value: String) => value match {
        case null|"" => null
        case x => dateFormatter.parse(x)
      })
  }

  /** make a Constraint which will try to parse and collect errors */
  protected def parsing[T](parse: String => T, messageKey: String, pattern: String = ""): Constraint =
    (label, value, messages) => value match {
      case null|"" => None
      case x => {
        try { parse(x); None }
        catch {
          case e: Exception => Some(messages(messageKey).get.format(label, pattern))
        }
      }
    }

  ///////////////////////////////////////// pre-defined general usage mappings  ///////////////////////////////

  def ignored[T](instead: T): Mapping[T] = new Mapping[T]() {
    override def convert(name: String, data: Map[String, String]): T = instead
    override def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = Nil
  }

  def default[T](base: Mapping[T], value: T): Mapping[T] = optional(base).mapTo(_.getOrElse(value))

  def optional[T](base: Mapping[T]): Mapping[Option[T]] = new Mapping[Option[T]]() {
    override def convert(name: String, data: Map[String, String]): Option[T] =
      if (data.keys.find(_.startsWith(name)).isEmpty ||
        (data.contains(name) && data.get(name).map {v => (v == null || v.isEmpty)} == Some(true))) None
      else {
        val dummyMessages: Messages = (key) => Some("dummy")
        base.validate(name, data, dummyMessages, Options.apply()) match {
          case Nil => Option(base.convert(name, data))
          case _   => None
        }
      }
    override def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] = Nil
  }

  def list[T](base: Mapping[T]): Mapping[List[T]] = seq(base).mapTo(_.toList)

  def seq[T](base: Mapping[T]): Mapping[Seq[T]] = new Mapping[Seq[T]] {
    override def convert(name: String, data: Map[String, String]): Seq[T] =
      indexes(name, data).map { i =>
        base.convert(name + "[" + i + "]", data)
      }

    override def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] =
      indexes(name, data).map { i =>
        base.validate(name + "[" + i + "]", data, messages, parentOptions)
      }.flatten

    /** Computes the available indexes for the given key in this set of data. */
    private def indexes(key: String, data: Map[String, String]): Seq[Int] = {
      val KeyPattern = ("^" + Pattern.quote(key) + """\[(\d+)\].*$""").r
      data.toSeq.collect { case (KeyPattern(index), _) => index.toInt }.sorted.distinct
    }
  }

  def map[V](valueBinding: Mapping[V]): Mapping[Map[String, V]] = map(text(), valueBinding)

  def map[K, V](keyBinding: FieldMapping[K], valueBinding: Mapping[V]): Mapping[Map[K, V]] = new Mapping[Map[K, V]] {
    override def convert(name: String, data: Map[String, String]): Map[K, V] =
      Map.empty ++ keys(name, data).map { key =>
        val pureKey = key.replaceAll("^\"", "").replaceAll("\"$", "")
        (keyBinding.convert(pureKey), valueBinding.convert(name + "." + key, data))
      }

    override def validate(name: String, data: Map[String, String], messages: Messages, parentOptions: Options): Seq[(String, String)] =
      keys(name, data).map { key =>
        val pureKey = key.replaceAll("^\"", "").replaceAll("\"$", "")
        keyBinding.validate(name + "." + key, pureKey, messages, parentOptions).map {
          case (name, err) => (name, "key: " + err)
        } ++ valueBinding.validate(name + "." + key, data, messages, parentOptions)
      }.flatten

    /** Computes the available keys for the given prefix in this set of data. */
    private def keys(prefix: String, data: Map[String, String]): Seq[String] = {
      val KeyPattern = ("^" + Pattern.quote(prefix) + """\.("?[^."]+"?).*$""").r
      data.toSeq.collect { case (KeyPattern(key), _) => key }.distinct
    }
  }

  ////////////////////////////////////////////  pre-defined group mappings  ///////////////////////////////////

  // tuple version
  def tmapping[P1](f1: (String, Mapping[P1])) = mapping[(P1), P1](f1)(identity)
  // normal version
  def mapping[T, P1](f1: (String, Mapping[P1]))(factory: (P1) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data))
    )

  // tuple version
  def tmapping[P1, P2](f1: (String, Mapping[P1]), f2: (String, Mapping[P2])) = mapping[(P1, P2), P1, P2](f1, f2)(Tuple2[P1,P2])
  // normal version
  def mapping[T, P1, P2](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]))(factory: (P1, P2) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3])) = mapping[(P1, P2, P3), P1, P2, P3](f1, f2, f3)(Tuple3[P1,P2,P3])
  // normal version
  def mapping[T, P1, P2, P3](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]))(factory: (P1, P2, P3) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4])) = mapping[(P1, P2, P3, P4), P1, P2, P3, P4](f1, f2, f3, f4)(Tuple4[P1,P2,P3,P4])
  // normal version
  def mapping[T, P1, P2, P3, P4](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]))(factory: (P1, P2, P3, P4) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5])) = mapping[(P1, P2, P3, P4, P5), P1, P2, P3, P4, P5](f1, f2, f3, f4, f5)(Tuple5[P1,P2,P3,P4,P5])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]))(factory: (P1, P2, P3, P4, P5) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6])) = mapping[(P1, P2, P3, P4, P5, P6), P1, P2, P3, P4, P5, P6](f1, f2, f3, f4, f5, f6)(Tuple6[P1, P2, P3, P4, P5, P6])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]))(factory: (P1, P2, P3, P4, P5, P6) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7])) = mapping[(P1, P2, P3, P4, P5, P6, P7), P1, P2, P3, P4, P5, P6, P7](f1, f2, f3, f4, f5, f6, f7)(Tuple7[P1, P2, P3, P4, P5, P6, P7])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]))(factory: (P1, P2, P3, P4, P5, P6, P7) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8), P1, P2, P3, P4, P5, P6, P7, P8](f1, f2, f3, f4, f5, f6, f7, f8)(Tuple8[P1, P2, P3, P4, P5, P6, P7, P8])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P1, P2, P3, P4, P5, P6, P7, P8, P9](f1, f2, f3, f4, f5, f6, f7, f8, f9)(Tuple9[P1, P2, P3, P4, P5, P6, P7, P8, P9])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)(Tuple10[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)(Tuple11[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)(Tuple12[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)(Tuple13[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)(Tuple14[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)(Tuple15[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)(Tuple16[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)(Tuple17[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]), f18: (String, Mapping[P18])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)(Tuple18[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]), f18: (String, Mapping[P18]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data), p(f18, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]), f18: (String, Mapping[P18]), f19: (String, Mapping[P19])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)(Tuple19[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]), f18: (String, Mapping[P18]), f19: (String, Mapping[P19]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data), p(f18, name, data), p(f19, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]), f18: (String, Mapping[P18]), f19: (String, Mapping[P19]), f20: (String, Mapping[P20])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)(Tuple20[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]), f18: (String, Mapping[P18]), f19: (String, Mapping[P19]), f20: (String, Mapping[P20]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data), p(f18, name, data), p(f19, name, data), p(f20, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]), f18: (String, Mapping[P18]), f19: (String, Mapping[P19]), f20: (String, Mapping[P20]), f21: (String, Mapping[P21])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21)(Tuple21[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]), f18: (String, Mapping[P18]), f19: (String, Mapping[P19]), f20: (String, Mapping[P20]), f21: (String, Mapping[P21]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data), p(f18, name, data), p(f19, name, data), p(f20, name, data), p(f21, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]), f18: (String, Mapping[P18]), f19: (String, Mapping[P19]), f20: (String, Mapping[P20]), f21: (String, Mapping[P21]), f22: (String, Mapping[P22])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22)(Tuple22[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](f1: (String, Mapping[P1]), f2: (String, Mapping[P2]), f3: (String, Mapping[P3]), f4: (String, Mapping[P4]), f5: (String, Mapping[P5]), f6: (String, Mapping[P6]), f7: (String, Mapping[P7]), f8: (String, Mapping[P8]), f9: (String, Mapping[P9]), f10: (String, Mapping[P10]), f11: (String, Mapping[P11]), f12: (String, Mapping[P12]), f13: (String, Mapping[P13]), f14: (String, Mapping[P14]), f15: (String, Mapping[P15]), f16: (String, Mapping[P16]), f17: (String, Mapping[P17]), f18: (String, Mapping[P18]), f19: (String, Mapping[P19]), f20: (String, Mapping[P20]), f21: (String, Mapping[P21]), f22: (String, Mapping[P22]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data), p(f18, name, data), p(f19, name, data), p(f20, name, data), p(f21, name, data), p(f22, name, data))
    )

  /** convert param string to value with given binding */
  private def p[T](field: (String, Mapping[T]), name: String, data: Map[String, String]): T =
    field match { case (fieldName, binding) =>
      val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
      binding.convert(fullName, data)
    }
}

object Mappings extends Mappings