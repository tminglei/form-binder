package com.github.tminglei.bind

import java.util.UUID
import FrameworkUtils._

trait Mappings {

  ////////////////////////////////////////////  pre-defined field mappings  ///////////////////////////////////

  def text(constraints: (Constraint with OneInput)*): Mapping[String, OneInput] =
    new FieldMapping[String, OneInput](
      convert0 = mkSimpleConverter(identity)
    ).>+:(constraints: _*)

  def boolean(constraints: (Constraint with OneInput)*): Mapping[Boolean, OneInput] =
    new FieldMapping[Boolean, OneInput](
      convert0 = mkSimpleConverter {
        case null|"" => false
        case x => x.toBoolean
      }).>+:((parsing(_.toBoolean, "error.boolean") +: constraints): _*)

  def number(constraints: (Constraint with OneInput)*): Mapping[Int, OneInput] =
    new FieldMapping[Int, OneInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0
        case x => x.toInt
      }).>+:((parsing(_.toInt, "error.number") +: constraints): _*)

  def double(constraints: (Constraint with OneInput)*): Mapping[Double, OneInput] =
    new FieldMapping[Double, OneInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0d
        case x => x.toDouble
      }).>+:((parsing(_.toDouble, "error.double") +: constraints): _*)

  def float(constraints: (Constraint with OneInput)*): Mapping[Float, OneInput] =
    new FieldMapping[Float, OneInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0f
        case x => x.toFloat
      }).>+:((parsing(_.toFloat, "error.float") +: constraints): _*)

  def long(constraints: (Constraint with OneInput)*): Mapping[Long, OneInput] =
    new FieldMapping[Long, OneInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0l
        case x => x.toLong
      }).>+:((parsing(_.toLong, "error.long") +: constraints): _*)

  def bigDecimal(constraints: (Constraint with OneInput)*): Mapping[BigDecimal, OneInput] =
    new FieldMapping[BigDecimal, OneInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0d
        case x => BigDecimal(x)
      }).>+:((parsing(BigDecimal.apply, "error.bigdecimal") +: constraints): _*)

  def bigInt(constraints: (Constraint with OneInput)*): Mapping[BigInt, OneInput] =
    new FieldMapping[BigInt, OneInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0l
        case x => BigInt(x)
      }).>+:((parsing(BigInt.apply, "error.bigint") +: constraints): _*)

  def uuid(constraints: (Constraint with OneInput)*): Mapping[UUID, OneInput] =
    new FieldMapping[UUID, OneInput](
      convert0 = mkSimpleConverter {
        case null|"" => null
        case x => UUID.fromString(x)
      }).>+:((parsing(UUID.fromString, "error.uuid") +: constraints): _*)

  def date(pattern: String, constraints: (Constraint with OneInput)*): Mapping[java.util.Date, OneInput] = {
    val dateFormatter = new java.text.SimpleDateFormat(pattern)
    new FieldMapping[java.util.Date, OneInput](
      convert0 = mkSimpleConverter {
        case null|"" => null
        case x => dateFormatter.parse(x)
      }).>+:((parsing(dateFormatter.parse, "error.pattern", pattern) +: constraints): _*)
  }

  ///////////////////////////////////////// pre-defined general usage mappings  ///////////////////////////////

  def ignored[T](instead: T): Mapping[T, OneInput with MultiInput] =
    FieldMapping[T, OneInput with MultiInput](
      convert0 = (name, data) => instead,
      myValidate = PassValidating
    ).options(_.copy(_ignoreConstraints = true))

  def default[T, M <: InputMode](base: Mapping[T, M], value: T): Mapping[T, M] = optional(base).mapTo(_.getOrElse(value))

  def optional[T, M <: InputMode](base: Mapping[T, M]): Mapping[Option[T], M] =
    FieldMapping[Option[T], M](
      convert0 = (name, data) => {
        if (isEmptyInput(name, data, base.options._multiInput)) None
        else Some(base.convert(name, data))
      },
      myValidate = (name, data, messages, parentOptions) => {
        if (isEmptyInput(name, data, base.options._multiInput)) Nil
        else
          base.validate(name, data, messages, parentOptions)
      }
    ).options(_.copy(_ignoreConstraints = true))

  def list[T](base: Mapping[T, _], constraints: (Constraint with MultiInput)*): Mapping[List[T], MultiInput] =
    seq(base, constraints: _*).mapTo(_.toList)
  
  def seq[T](base: Mapping[T, _], constraints: (Constraint with MultiInput)*): Mapping[Seq[T], MultiInput] =
    FieldMapping[Seq[T], MultiInput](
      convert0 = (name, data) => {
        indexes(name, data).map { i =>
          base.convert(name + "[" + i + "]", data)
        }
      },
      myValidate = (name, data, messages, parentOptions) => {
        indexes(name, data).map { i =>
          base.validate(name + "[" + i + "]", data, messages, parentOptions)
        }.flatten
      }
    ).options(_.copy(_multiInput = true))
      .>+:(constraints: _*)

  def map[V](valueBinding: Mapping[V, _], constraints: (Constraint with MultiInput)*): Mapping[Map[String, V], MultiInput] =
    map(text(), valueBinding, constraints: _*)

  def map[K, V](keyBinding: Mapping[K, OneInput], valueBinding: Mapping[V, _],
          constraints: (Constraint with MultiInput)*): Mapping[Map[K, V], MultiInput] =
    FieldMapping[Map[K, V], MultiInput](
      convert0 = (name, data) => {
        Map.empty ++ keys(name, data).map { key =>
          val keyName = name + "." + key
          val pureKey = key.replaceAll("^\"", "").replaceAll("\"$", "")
          (keyBinding.convert(keyName, Map(keyName -> pureKey)), valueBinding.convert(keyName, data))
        }
      },
      myValidate = (name, data, messages, parentOptions) => {
        keys(name, data).map { key =>
          val keyName = name + "." + key
          val pureKey = key.replaceAll("^\"", "").replaceAll("\"$", "")
          keyBinding.validate(keyName, Map(keyName -> pureKey), messages, parentOptions).map {
            case (name, err) => (name, NAME_ERR_PREFIX + err)
          } ++ valueBinding.validate(keyName, data, messages, parentOptions)
        }.flatten
      }
    ).options(_.copy(_multiInput = true))
      .>+:(constraints: _*)

  ////////////////////////////////////////////  pre-defined group mappings  ///////////////////////////////////

  // tuple version
  def tmapping[P1, _](f1: (String, Mapping[P1, _])) = mapping[(P1), P1](f1)(identity)
  // normal version
  def mapping[T, P1](f1: (String, Mapping[P1, _]))(factory: (P1) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data))
    )

  // tuple version
  def tmapping[P1, P2](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _])) = mapping[(P1, P2), P1, P2](f1, f2)(Tuple2[P1,P2])
  // normal version
  def mapping[T, P1, P2](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]))(factory: (P1, P2) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _])) = mapping[(P1, P2, P3), P1, P2, P3](f1, f2, f3)(Tuple3[P1,P2,P3])
  // normal version
  def mapping[T, P1, P2, P3](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]))(factory: (P1, P2, P3) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _])) = mapping[(P1, P2, P3, P4), P1, P2, P3, P4](f1, f2, f3, f4)(Tuple4[P1,P2,P3,P4])
  // normal version
  def mapping[T, P1, P2, P3, P4](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]))(factory: (P1, P2, P3, P4) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _])) = mapping[(P1, P2, P3, P4, P5), P1, P2, P3, P4, P5](f1, f2, f3, f4, f5)(Tuple5[P1,P2,P3,P4,P5])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]))(factory: (P1, P2, P3, P4, P5) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _])) = mapping[(P1, P2, P3, P4, P5, P6), P1, P2, P3, P4, P5, P6](f1, f2, f3, f4, f5, f6)(Tuple6[P1, P2, P3, P4, P5, P6])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]))(factory: (P1, P2, P3, P4, P5, P6) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7), P1, P2, P3, P4, P5, P6, P7](f1, f2, f3, f4, f5, f6, f7)(Tuple7[P1, P2, P3, P4, P5, P6, P7])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]))(factory: (P1, P2, P3, P4, P5, P6, P7) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8), P1, P2, P3, P4, P5, P6, P7, P8](f1, f2, f3, f4, f5, f6, f7, f8)(Tuple8[P1, P2, P3, P4, P5, P6, P7, P8])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P1, P2, P3, P4, P5, P6, P7, P8, P9](f1, f2, f3, f4, f5, f6, f7, f8, f9)(Tuple9[P1, P2, P3, P4, P5, P6, P7, P8, P9])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)(Tuple10[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)(Tuple11[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)(Tuple12[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)(Tuple13[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)(Tuple14[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)(Tuple15[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)(Tuple16[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)(Tuple17[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]), f18: (String, Mapping[P18, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)(Tuple18[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]), f18: (String, Mapping[P18, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data), p(f18, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]), f18: (String, Mapping[P18, _]), f19: (String, Mapping[P19, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)(Tuple19[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]), f18: (String, Mapping[P18, _]), f19: (String, Mapping[P19, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data), p(f18, name, data), p(f19, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]), f18: (String, Mapping[P18, _]), f19: (String, Mapping[P19, _]), f20: (String, Mapping[P20, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)(Tuple20[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]), f18: (String, Mapping[P18, _]), f19: (String, Mapping[P19, _]), f20: (String, Mapping[P20, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data), p(f18, name, data), p(f19, name, data), p(f20, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]), f18: (String, Mapping[P18, _]), f19: (String, Mapping[P19, _]), f20: (String, Mapping[P20, _]), f21: (String, Mapping[P21, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21)(Tuple21[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]), f18: (String, Mapping[P18, _]), f19: (String, Mapping[P19, _]), f20: (String, Mapping[P20, _]), f21: (String, Mapping[P21, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data), p(f18, name, data), p(f19, name, data), p(f20, name, data), p(f21, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]), f18: (String, Mapping[P18, _]), f19: (String, Mapping[P19, _]), f20: (String, Mapping[P20, _]), f21: (String, Mapping[P21, _]), f22: (String, Mapping[P22, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22)(Tuple22[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](f1: (String, Mapping[P1, _]), f2: (String, Mapping[P2, _]), f3: (String, Mapping[P3, _]), f4: (String, Mapping[P4, _]), f5: (String, Mapping[P5, _]), f6: (String, Mapping[P6, _]), f7: (String, Mapping[P7, _]), f8: (String, Mapping[P8, _]), f9: (String, Mapping[P9, _]), f10: (String, Mapping[P10, _]), f11: (String, Mapping[P11, _]), f12: (String, Mapping[P12, _]), f13: (String, Mapping[P13, _]), f14: (String, Mapping[P14, _]), f15: (String, Mapping[P15, _]), f16: (String, Mapping[P16, _]), f17: (String, Mapping[P17, _]), f18: (String, Mapping[P18, _]), f19: (String, Mapping[P19, _]), f20: (String, Mapping[P20, _]), f21: (String, Mapping[P21, _]), f22: (String, Mapping[P22, _]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22),
      convert0 = (name: String, data: Map[String, String]) => factory(p(f1, name, data), p(f2, name, data), p(f3, name, data), p(f4, name, data), p(f5, name, data), p(f6, name, data), p(f7, name, data), p(f8, name, data), p(f9, name, data), p(f10, name, data), p(f11, name, data), p(f12, name, data), p(f13, name, data), p(f14, name, data), p(f15, name, data), p(f16, name, data), p(f17, name, data), p(f18, name, data), p(f19, name, data), p(f20, name, data), p(f21, name, data), p(f22, name, data))
    )

  /** convert param string to value with given binding */
  private def p[T](field: (String, Mapping[T, _]), name: String, data: Map[String, String]): T =
    field match { case (fieldName, binding) =>
      val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
      binding.convert(fullName, data)
    }
}

object Mappings extends Mappings