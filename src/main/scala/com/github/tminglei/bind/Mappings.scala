package com.github.tminglei.bind

import java.util.{Date, UUID}

trait Mappings {
  import FrameworkUtils._

  /////////////////////////////////////////  pre-defined field mappings  //////////////////////////////

  def text(constraints: Constraint*): Mapping[String] =
    new FieldMapping[String](
      doConvert = mkSimpleConverter(identity)
    ).>+:(constraints: _*)

  def boolean(constraints: Constraint*): Mapping[Boolean] =
    new FieldMapping[Boolean](
      doConvert = mkSimpleConverter {
        case null|"" => false
        case x => x.toBoolean
      }).>+:(checking(_.toBoolean, Right("error.boolean")))
        .>+:(constraints: _*)

  def number(constraints: Constraint*): Mapping[Int] =
    new FieldMapping[Int](
      doConvert = mkSimpleConverter {
        case null|"" => 0
        case x => x.toInt
      }).>+:(checking(_.toInt, Right("error.number")))
        .>+:(constraints: _*)

  def double(constraints: Constraint*): Mapping[Double] =
    new FieldMapping[Double](
      doConvert = mkSimpleConverter {
        case null|"" => 0d
        case x => x.toDouble
      }).>+:(checking(_.toDouble, Right("error.double")))
        .>+:(constraints: _*)

  def float(constraints: Constraint*): Mapping[Float] =
    new FieldMapping[Float](
      doConvert = mkSimpleConverter {
        case null|"" => 0f
        case x => x.toFloat
      }).>+:(checking(_.toFloat, Right("error.float")))
        .>+:(constraints: _*)

  def long(constraints: Constraint*): Mapping[Long] =
    new FieldMapping[Long](
      doConvert = mkSimpleConverter {
        case null|"" => 0l
        case x => x.toLong
      }).>+:(checking(_.toLong, Right("error.long")))
        .>+:(constraints: _*)

  def bigDecimal(constraints: Constraint*): Mapping[BigDecimal] =
    new FieldMapping[BigDecimal](
      doConvert = mkSimpleConverter {
        case null|"" => 0d
        case x => BigDecimal(x)
      }).>+:(checking(BigDecimal.apply, Right("error.bigdecimal")))
        .>+:(constraints: _*)

  def bigInt(constraints: Constraint*): Mapping[BigInt] =
    new FieldMapping[BigInt](
      doConvert = mkSimpleConverter {
        case null|"" => 0l
        case x => BigInt(x)
      }).>+:(checking(BigInt.apply, Right("error.bigint")))
        .>+:(constraints: _*)

  def uuid(constraints: Constraint*): Mapping[UUID] =
    new FieldMapping[UUID](
      doConvert = mkSimpleConverter {
        case null|"" => null
        case x => UUID.fromString(x)
      }).>+:(checking(UUID.fromString, Right("error.uuid")))
        .>+:(constraints: _*)

  def date(constraints: Constraint*): Mapping[java.util.Date] =
    date("yyyy-MM-dd HH:mm:ss.SSS", constraints: _*)
  def date(pattern: String, constraints: Constraint*): Mapping[java.util.Date] = {
    val dateFormatter = new java.text.SimpleDateFormat(pattern)
    new FieldMapping[java.util.Date](
      doConvert = mkSimpleConverter {
        case null|"" => null
        case x => if (x.matches("^[\\d]+$")) new Date(x.toLong) else dateFormatter.parse(x)
      }).>+:(anyPassed(
          checking(s => new Date(s.toLong), Left("'%s' not a date long")),
          checking(dateFormatter.parse, Right("error.pattern"), pattern)
        )).>+:(constraints: _*)
  }

  ///////////////////////////////////////// pre-defined general usage mappings  ///////////////////////////////

  def ignored[T](instead: T): Mapping[T] =
    FieldMapping[T](
      inputMode = PolyInput,
      doConvert = (name, data) => instead,
      moreValidate = PassValidating
    ).options(_.copy(_ignoreConstraints = true))

  def default[T](base: Mapping[T], value: T): Mapping[T] = optional(base).mapTo(_.getOrElse(value))

  def optional[T](base: Mapping[T]): Mapping[Option[T]] =
    FieldMapping[Option[T]](
      inputMode = base.options._inputMode,
      doConvert = (name, data) => {
        if (isEmptyInput(name, data, base.options._inputMode)) None
        else Some(base.convert(name, data))
      },
      moreValidate = (name, data, messages, options) => {
        if (isEmptyInput(name, data, base.options._inputMode)) Nil
        else { // merge the optional's constraints/label to base mapping then do validating
          base.options(_.copy(_constraints = options._constraints ++ base.options._constraints))
            .options(o => o.copy(_label = o._label.orElse(options._label)))
            .validate(name, data, messages, options)
        }
      }
    ).options(_.copy(_ignoreConstraints = true))

  def list[T](base: Mapping[T], constraints: Constraint*): Mapping[List[T]] =
    seq(base, constraints: _*).mapTo(_.toList)
  
  def seq[T](base: Mapping[T], constraints: Constraint*): Mapping[Seq[T]] =
    FieldMapping[Seq[T]](
      inputMode = BulkInput,
      doConvert = (name, data) => {
        indexes(name, data).map { i =>
          base.convert(name + "[" + i + "]", data)
        }
      },
      moreValidate = (name, data, messages, theOptions) => {
        indexes(name, data).map { i =>
          base.validate(name + "[" + i + "]", data, messages, theOptions)
        }.flatten
      }
    ).>+:(constraints: _*)

  def map[V](valueBinding: Mapping[V], constraints: Constraint*): Mapping[Map[String, V]] =
    map(text(), valueBinding, constraints: _*)

  def map[K, V](keyBinding: Mapping[K], valueBinding: Mapping[V],
          constraints: Constraint*): Mapping[Map[K, V]] =
    FieldMapping[Map[K, V]](
      inputMode = BulkInput,
      doConvert = (name, data) => {
        Map.empty ++ keys(name, data).map { key =>
          val keyName = if (isEmptyStr(name)) key else name + "." + key
          (keyBinding.convert(key, Map(key -> key)), valueBinding.convert(keyName, data))
        }
      },
      moreValidate = (name, data, messages, theOptions) => {
        keys(name, data).map { key =>
          val keyName = if (isEmptyStr(name)) key else name + "." + key
          keyBinding.validate(key, Map(key -> key), messages, theOptions).map {
            case (name, err) => (name, err)
          } ++ valueBinding.validate(keyName, data, messages, theOptions)
        }.flatten
      }
    ).>+:(constraints: _*)

  ////////////////////////////////////////////  pre-defined group mappings  ///////////////////////////////////

  // tuple version
  def tmapping[P1](fm1: (String, Mapping[P1])) = mapping[(P1), P1](fm1)(identity)
  // normal version
  def mapping[T, P1](fm1: (String, Mapping[P1]))(create: (P1) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data))
    )

  // tuple version
  def tmapping[P1, P2](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2])) = mapping[(P1, P2), P1, P2](fm1, fm2)(Tuple2[P1,P2])
  // normal version
  def mapping[T, P1, P2](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]))(create: (P1, P2) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3])) = mapping[(P1, P2, P3), P1, P2, P3](fm1, fm2, fm3)(Tuple3[P1,P2,P3])
  // normal version
  def mapping[T, P1, P2, P3](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]))(create: (P1, P2, P3) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4])) = mapping[(P1, P2, P3, P4), P1, P2, P3, P4](fm1, fm2, fm3, fm4)(Tuple4[P1,P2,P3,P4])
  // normal version
  def mapping[T, P1, P2, P3, P4](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]))(create: (P1, P2, P3, P4) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5])) = mapping[(P1, P2, P3, P4, P5), P1, P2, P3, P4, P5](fm1, fm2, fm3, fm4, fm5)(Tuple5[P1,P2,P3,P4,P5])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]))(create: (P1, P2, P3, P4, P5) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6])) = mapping[(P1, P2, P3, P4, P5, P6), P1, P2, P3, P4, P5, P6](fm1, fm2, fm3, fm4, fm5, fm6)(Tuple6[P1, P2, P3, P4, P5, P6])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]))(create: (P1, P2, P3, P4, P5, P6) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7])) = mapping[(P1, P2, P3, P4, P5, P6, P7), P1, P2, P3, P4, P5, P6, P7](fm1, fm2, fm3, fm4, fm5, fm6, fm7)(Tuple7[P1, P2, P3, P4, P5, P6, P7])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]))(create: (P1, P2, P3, P4, P5, P6, P7) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8), P1, P2, P3, P4, P5, P6, P7, P8](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8)(Tuple8[P1, P2, P3, P4, P5, P6, P7, P8])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]))(create: (P1, P2, P3, P4, P5, P6, P7, P8) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P1, P2, P3, P4, P5, P6, P7, P8, P9](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9)(Tuple9[P1, P2, P3, P4, P5, P6, P7, P8, P9])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10)(Tuple10[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11)(Tuple11[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12)(Tuple12[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13)(Tuple13[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14)(Tuple14[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15)(Tuple15[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16)(Tuple16[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17)(Tuple17[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]), fm18: (String, Mapping[P18])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18)(Tuple18[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]), fm18: (String, Mapping[P18]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data), conv(fm18, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]), fm18: (String, Mapping[P18]), fm19: (String, Mapping[P19])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19)(Tuple19[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]), fm18: (String, Mapping[P18]), fm19: (String, Mapping[P19]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data), conv(fm18, name, data), conv(fm19, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]), fm18: (String, Mapping[P18]), fm19: (String, Mapping[P19]), fm20: (String, Mapping[P20])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20)(Tuple20[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]), fm18: (String, Mapping[P18]), fm19: (String, Mapping[P19]), fm20: (String, Mapping[P20]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data), conv(fm18, name, data), conv(fm19, name, data), conv(fm20, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]), fm18: (String, Mapping[P18]), fm19: (String, Mapping[P19]), fm20: (String, Mapping[P20]), fm21: (String, Mapping[P21])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21)(Tuple21[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]), fm18: (String, Mapping[P18]), fm19: (String, Mapping[P19]), fm20: (String, Mapping[P20]), fm21: (String, Mapping[P21]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data), conv(fm18, name, data), conv(fm19, name, data), conv(fm20, name, data), conv(fm21, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]), fm18: (String, Mapping[P18]), fm19: (String, Mapping[P19]), fm20: (String, Mapping[P20]), fm21: (String, Mapping[P21]), fm22: (String, Mapping[P22])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21, fm22)(Tuple22[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](fm1: (String, Mapping[P1]), fm2: (String, Mapping[P2]), fm3: (String, Mapping[P3]), fm4: (String, Mapping[P4]), fm5: (String, Mapping[P5]), fm6: (String, Mapping[P6]), fm7: (String, Mapping[P7]), fm8: (String, Mapping[P8]), fm9: (String, Mapping[P9]), fm10: (String, Mapping[P10]), fm11: (String, Mapping[P11]), fm12: (String, Mapping[P12]), fm13: (String, Mapping[P13]), fm14: (String, Mapping[P14]), fm15: (String, Mapping[P15]), fm16: (String, Mapping[P16]), fm17: (String, Mapping[P17]), fm18: (String, Mapping[P18]), fm19: (String, Mapping[P19]), fm20: (String, Mapping[P20]), fm21: (String, Mapping[P21]), fm22: (String, Mapping[P22]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21, fm22),
      doConvert = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data), conv(fm18, name, data), conv(fm19, name, data), conv(fm20, name, data), conv(fm21, name, data), conv(fm22, name, data))
    )

  /** convert param string to value with given binding */
  private def conv[T](fieldMapping: (String, Mapping[T]), name: String, data: Map[String, String]): T =
    fieldMapping match { case (fieldName, mapping) =>
      val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
      mapping.convert(fullName, data)
    }
}

object Mappings extends Mappings