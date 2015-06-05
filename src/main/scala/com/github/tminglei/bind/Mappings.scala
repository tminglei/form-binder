package com.github.tminglei.bind

import java.util.UUID
import java.util.regex.Pattern
import org.json4s.{JNull, JValue}
import scala.collection.mutable.HashMap

trait Mappings {
  import FrameworkUtils._
  ////////////////////////////////////////////  pre-defined field mappings  ///////////////////////////////////

  def text(constraints: (Constraint with SoloInput)*): Mapping[String, SoloInput] =
    new FieldMapping[String, SoloInput](
      convert0 = mkSimpleConverter(identity)
    ).>+:(constraints: _*)

  def boolean(constraints: (Constraint with SoloInput)*): Mapping[Boolean, SoloInput] =
    new FieldMapping[Boolean, SoloInput](
      convert0 = mkSimpleConverter {
        case null|"" => false
        case x => x.toBoolean
      }).>+:((parsing(_.toBoolean, "error.boolean") +: constraints): _*)

  def number(constraints: (Constraint with SoloInput)*): Mapping[Int, SoloInput] =
    new FieldMapping[Int, SoloInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0
        case x => x.toInt
      }).>+:((parsing(_.toInt, "error.number") +: constraints): _*)

  def double(constraints: (Constraint with SoloInput)*): Mapping[Double, SoloInput] =
    new FieldMapping[Double, SoloInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0d
        case x => x.toDouble
      }).>+:((parsing(_.toDouble, "error.double") +: constraints): _*)

  def float(constraints: (Constraint with SoloInput)*): Mapping[Float, SoloInput] =
    new FieldMapping[Float, SoloInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0f
        case x => x.toFloat
      }).>+:((parsing(_.toFloat, "error.float") +: constraints): _*)

  def long(constraints: (Constraint with SoloInput)*): Mapping[Long, SoloInput] =
    new FieldMapping[Long, SoloInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0l
        case x => x.toLong
      }).>+:((parsing(_.toLong, "error.long") +: constraints): _*)

  def bigDecimal(constraints: (Constraint with SoloInput)*): Mapping[BigDecimal, SoloInput] =
    new FieldMapping[BigDecimal, SoloInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0d
        case x => BigDecimal(x)
      }).>+:((parsing(BigDecimal.apply, "error.bigdecimal") +: constraints): _*)

  def bigInt(constraints: (Constraint with SoloInput)*): Mapping[BigInt, SoloInput] =
    new FieldMapping[BigInt, SoloInput](
      convert0 = mkSimpleConverter {
        case null|"" => 0l
        case x => BigInt(x)
      }).>+:((parsing(BigInt.apply, "error.bigint") +: constraints): _*)

  def uuid(constraints: (Constraint with SoloInput)*): Mapping[UUID, SoloInput] =
    new FieldMapping[UUID, SoloInput](
      convert0 = mkSimpleConverter {
        case null|"" => null
        case x => UUID.fromString(x)
      }).>+:((parsing(UUID.fromString, "error.uuid") +: constraints): _*)

  def date(pattern: String, constraints: (Constraint with SoloInput)*): Mapping[java.util.Date, SoloInput] = {
    val dateFormatter = new java.text.SimpleDateFormat(pattern)
    new FieldMapping[java.util.Date, SoloInput](
      convert0 = mkSimpleConverter {
        case null|"" => null
        case x => dateFormatter.parse(x)
      }).>+:((parsing(dateFormatter.parse, "error.pattern", pattern) +: constraints): _*)
  }

  def json4s(useBigDecimalForDouble: Boolean, constraints: (Constraint with BulkInput with SoloInput)*) =
    new FieldMapping[JValue, InputMode](
      inputMode = PolyInput,
      convert0 = (name, data) => {
        val root = HashMap[String, Any]()
        val workList = HashMap[String, Any]("" -> root)
        data.filter(_._1.startsWith(name)).map { case (key, value) =>
          val key1 = key.replaceFirst("^"+Pattern.quote(name), "")
          val (parent, self, isArray) = splitName(key1)
          val workObj = workObject(workList, parent, isArray)
          workObj += (self -> value)
        }
        if (root.isEmpty) JNull else mapTreeToJson4s(root, useBigDecimalForDouble)
      }
    ).>+:(constraints: _*)

  ///////////////////////////////////////// pre-defined general usage mappings  ///////////////////////////////

  def ignored[T](instead: T): Mapping[T, SoloInput with BulkInput] =
    FieldMapping[T, SoloInput with BulkInput](
      inputMode = PolyInput,
      convert0 = (name, data) => instead,
      myValidate = PassValidating
    ).options(_.copy(_ignoreConstraints = true))

  def default[T, M <: InputMode](base: Mapping[T, M], value: T): Mapping[T, M] = optional(base).mapTo(_.getOrElse(value))

  def optional[T, M <: InputMode](base: Mapping[T, M]): Mapping[Option[T], M] =
    FieldMapping[Option[T], M](
      inputMode = base.options._inputMode.asInstanceOf[M],
      convert0 = (name, data) => {
        if (isEmptyInput(name, data, base.options._inputMode)) None
        else Some(base.convert(name, data))
      },
      myValidate = (name, data, messages, theOptions) => {
        if (isEmptyInput(name, data, base.options._inputMode)) Nil
        else { // merge optional's pre-processors/constraints to base mapping
          base.options(_.copy(_processors = theOptions._processors ++ base.options._processors))
            .options(_.copy(_constraints = theOptions._constraints ++ base.options._constraints))
            .validate(name, data, messages, theOptions)
        }
      }
    ).options(_.copy(_ignoreConstraints = true))

  def list[T](base: Mapping[T, _], constraints: (Constraint with BulkInput)*): Mapping[List[T], BulkInput] =
    seq(base, constraints: _*).mapTo(_.toList)
  
  def seq[T](base: Mapping[T, _], constraints: (Constraint with BulkInput)*): Mapping[Seq[T], BulkInput] =
    FieldMapping[Seq[T], BulkInput](
      inputMode = BulkInput,
      convert0 = (name, data) => {
        indexes(name, data).map { i =>
          base.convert(name + "[" + i + "]", data)
        }
      },
      myValidate = (name, data, messages, theOptions) => {
        indexes(name, data).map { i =>
          base.validate(name + "[" + i + "]", data, messages, theOptions)
        }.flatten
      }
    ).>+:(constraints: _*)

  def map[V](valueBinding: Mapping[V, _], constraints: (Constraint with BulkInput)*): Mapping[Map[String, V], BulkInput] =
    map(text(), valueBinding, constraints: _*)

  def map[K, V](keyBinding: Mapping[K, SoloInput], valueBinding: Mapping[V, _],
          constraints: (Constraint with BulkInput)*): Mapping[Map[K, V], BulkInput] =
    FieldMapping[Map[K, V], BulkInput](
      inputMode = BulkInput,
      convert0 = (name, data) => {
        Map.empty ++ keys(name, data).map { key =>
          val keyName = name + "." + key
          val pureKey = key.replaceAll("^\"", "").replaceAll("\"$", "")
          (keyBinding.convert(keyName, Map(keyName -> pureKey)), valueBinding.convert(keyName, data))
        }
      },
      myValidate = (name, data, messages, theOptions) => {
        keys(name, data).map { key =>
          val keyName = name + "." + key
          val pureKey = key.replaceAll("^\"", "").replaceAll("\"$", "")
          keyBinding.validate(keyName, Map(keyName -> pureKey), messages, theOptions).map {
            case (name, err) => (name, NAME_ERR_PREFIX + err)
          } ++ valueBinding.validate(keyName, data, messages, theOptions)
        }.flatten
      }
    ).>+:(constraints: _*)

  ////////////////////////////////////////////  pre-defined group mappings  ///////////////////////////////////

  // tuple version
  def tmapping[P1](fm1: (String, Mapping[P1, _])) = mapping[(P1), P1](fm1)(identity)
  // normal version
  def mapping[T, P1](fm1: (String, Mapping[P1, _]))(create: (P1) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data))
    )

  // tuple version
  def tmapping[P1, P2](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _])) = mapping[(P1, P2), P1, P2](fm1, fm2)(Tuple2[P1,P2])
  // normal version
  def mapping[T, P1, P2](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]))(create: (P1, P2) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _])) = mapping[(P1, P2, P3), P1, P2, P3](fm1, fm2, fm3)(Tuple3[P1,P2,P3])
  // normal version
  def mapping[T, P1, P2, P3](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]))(create: (P1, P2, P3) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _])) = mapping[(P1, P2, P3, P4), P1, P2, P3, P4](fm1, fm2, fm3, fm4)(Tuple4[P1,P2,P3,P4])
  // normal version
  def mapping[T, P1, P2, P3, P4](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]))(create: (P1, P2, P3, P4) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _])) = mapping[(P1, P2, P3, P4, P5), P1, P2, P3, P4, P5](fm1, fm2, fm3, fm4, fm5)(Tuple5[P1,P2,P3,P4,P5])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]))(create: (P1, P2, P3, P4, P5) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _])) = mapping[(P1, P2, P3, P4, P5, P6), P1, P2, P3, P4, P5, P6](fm1, fm2, fm3, fm4, fm5, fm6)(Tuple6[P1, P2, P3, P4, P5, P6])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]))(create: (P1, P2, P3, P4, P5, P6) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7), P1, P2, P3, P4, P5, P6, P7](fm1, fm2, fm3, fm4, fm5, fm6, fm7)(Tuple7[P1, P2, P3, P4, P5, P6, P7])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]))(create: (P1, P2, P3, P4, P5, P6, P7) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8), P1, P2, P3, P4, P5, P6, P7, P8](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8)(Tuple8[P1, P2, P3, P4, P5, P6, P7, P8])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P1, P2, P3, P4, P5, P6, P7, P8, P9](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9)(Tuple9[P1, P2, P3, P4, P5, P6, P7, P8, P9])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10)(Tuple10[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11)(Tuple11[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12)(Tuple12[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13)(Tuple13[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14)(Tuple14[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15)(Tuple15[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16)(Tuple16[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17)(Tuple17[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]), fm18: (String, Mapping[P18, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18)(Tuple18[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]), fm18: (String, Mapping[P18, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data), conv(fm18, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]), fm18: (String, Mapping[P18, _]), fm19: (String, Mapping[P19, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19)(Tuple19[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]), fm18: (String, Mapping[P18, _]), fm19: (String, Mapping[P19, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data), conv(fm18, name, data), conv(fm19, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]), fm18: (String, Mapping[P18, _]), fm19: (String, Mapping[P19, _]), fm20: (String, Mapping[P20, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20)(Tuple20[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]), fm18: (String, Mapping[P18, _]), fm19: (String, Mapping[P19, _]), fm20: (String, Mapping[P20, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data), conv(fm18, name, data), conv(fm19, name, data), conv(fm20, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]), fm18: (String, Mapping[P18, _]), fm19: (String, Mapping[P19, _]), fm20: (String, Mapping[P20, _]), fm21: (String, Mapping[P21, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21)(Tuple21[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]), fm18: (String, Mapping[P18, _]), fm19: (String, Mapping[P19, _]), fm20: (String, Mapping[P20, _]), fm21: (String, Mapping[P21, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data), conv(fm18, name, data), conv(fm19, name, data), conv(fm20, name, data), conv(fm21, name, data))
    )

  // tuple version
  def tmapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]), fm18: (String, Mapping[P18, _]), fm19: (String, Mapping[P19, _]), fm20: (String, Mapping[P20, _]), fm21: (String, Mapping[P21, _]), fm22: (String, Mapping[P22, _])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21, fm22)(Tuple22[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](fm1: (String, Mapping[P1, _]), fm2: (String, Mapping[P2, _]), fm3: (String, Mapping[P3, _]), fm4: (String, Mapping[P4, _]), fm5: (String, Mapping[P5, _]), fm6: (String, Mapping[P6, _]), fm7: (String, Mapping[P7, _]), fm8: (String, Mapping[P8, _]), fm9: (String, Mapping[P9, _]), fm10: (String, Mapping[P10, _]), fm11: (String, Mapping[P11, _]), fm12: (String, Mapping[P12, _]), fm13: (String, Mapping[P13, _]), fm14: (String, Mapping[P14, _]), fm15: (String, Mapping[P15, _]), fm16: (String, Mapping[P16, _]), fm17: (String, Mapping[P17, _]), fm18: (String, Mapping[P18, _]), fm19: (String, Mapping[P19, _]), fm20: (String, Mapping[P20, _]), fm21: (String, Mapping[P21, _]), fm22: (String, Mapping[P22, _]))(create: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22) => T): GroupMapping[T] =
    new GroupMapping[T](Seq(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21, fm22),
      convert0 = (name: String, data: Map[String, String]) => create(conv(fm1, name, data), conv(fm2, name, data), conv(fm3, name, data), conv(fm4, name, data), conv(fm5, name, data), conv(fm6, name, data), conv(fm7, name, data), conv(fm8, name, data), conv(fm9, name, data), conv(fm10, name, data), conv(fm11, name, data), conv(fm12, name, data), conv(fm13, name, data), conv(fm14, name, data), conv(fm15, name, data), conv(fm16, name, data), conv(fm17, name, data), conv(fm18, name, data), conv(fm19, name, data), conv(fm20, name, data), conv(fm21, name, data), conv(fm22, name, data))
    )

  /** convert param string to value with given binding */
  private def conv[T](fieldMapping: (String, Mapping[T, _]), name: String, data: Map[String, String]): T =
    fieldMapping match { case (fieldName, mapping) =>
      val fullName = if (name.isEmpty) fieldName else name + "." + fieldName
      mapping.convert(fullName, data)
    }
}

object Mappings extends Mappings