package com.github.tminglei.bind

trait Bindings {


  ///////////////////////////////  pre-defined compound bindings  /////////////////////////////

  // tuple version
  def mapping[P1](f1: (String, Binding[P1])) = mapping[(P1), P1](f1)(identity)
  // normal version
  def mapping[T, P1](f1: (String, Binding[P1]))(factory: (P1) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params))
  }

  // tuple version
  def mapping[P1, P2](f1: (String, Binding[P1]), f2: (String, Binding[P2])) = mapping[(P1, P2), P1, P2](f1, f2)(Tuple2[P1,P2])
  // normal version
  def mapping[T, P1, P2](f1: (String, Binding[P1]), f2: (String, Binding[P2]))(factory: (P1, P2) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3])) = mapping[(P1, P2, P3), P1, P2, P3](f1, f2, f3)(Tuple3[P1,P2,P3])
  // normal version
  def mapping[T, P1, P2, P3](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]))(factory: (P1, P2, P3) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4])) = mapping[(P1, P2, P3, P4), P1, P2, P3, P4](f1, f2, f3, f4)(Tuple4[P1,P2,P3,P4])
  // normal version
  def mapping[T, P1, P2, P3, P4](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]))(factory: (P1, P2, P3, P4) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5])) = mapping[(P1, P2, P3, P4, P5), P1, P2, P3, P4, P5](f1, f2, f3, f4, f5)(Tuple5[P1,P2,P3,P4,P5])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]))(factory: (P1, P2, P3, P4, P5) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6])) = mapping[(P1, P2, P3, P4, P5, P6), P1, P2, P3, P4, P5, P6](f1, f2, f3, f4, f5, f6)(Tuple6[P1, P2, P3, P4, P5, P6])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]))(factory: (P1, P2, P3, P4, P5, P6) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7])) = mapping[(P1, P2, P3, P4, P5, P6, P7), P1, P2, P3, P4, P5, P6, P7](f1, f2, f3, f4, f5, f6, f7)(Tuple7[P1, P2, P3, P4, P5, P6, P7])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]))(factory: (P1, P2, P3, P4, P5, P6, P7) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8), P1, P2, P3, P4, P5, P6, P7, P8](f1, f2, f3, f4, f5, f6, f7, f8)(Tuple8[P1, P2, P3, P4, P5, P6, P7, P8])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P1, P2, P3, P4, P5, P6, P7, P8, P9](f1, f2, f3, f4, f5, f6, f7, f8, f9)(Tuple9[P1, P2, P3, P4, P5, P6, P7, P8, P9])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)(Tuple10[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)(Tuple11[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)(Tuple12[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)(Tuple13[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params), p(f13, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)(Tuple14[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params), p(f13, name, params), p(f14, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)(Tuple15[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params), p(f13, name, params), p(f14, name, params), p(f15, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)(Tuple16[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params), p(f13, name, params), p(f14, name, params), p(f15, name, params), p(f16, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)(Tuple17[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params), p(f13, name, params), p(f14, name, params), p(f15, name, params), p(f16, name, params), p(f17, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]), f18: (String, Binding[P18])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)(Tuple18[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]), f18: (String, Binding[P18]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params), p(f13, name, params), p(f14, name, params), p(f15, name, params), p(f16, name, params), p(f17, name, params), p(f18, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]), f18: (String, Binding[P18]), f19: (String, Binding[P19])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)(Tuple19[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]), f18: (String, Binding[P18]), f19: (String, Binding[P19]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params), p(f13, name, params), p(f14, name, params), p(f15, name, params), p(f16, name, params), p(f17, name, params), p(f18, name, params), p(f19, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]), f18: (String, Binding[P18]), f19: (String, Binding[P19]), f20: (String, Binding[P20])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)(Tuple20[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]), f18: (String, Binding[P18]), f19: (String, Binding[P19]), f20: (String, Binding[P20]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params), p(f13, name, params), p(f14, name, params), p(f15, name, params), p(f16, name, params), p(f17, name, params), p(f18, name, params), p(f19, name, params), p(f20, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]), f18: (String, Binding[P18]), f19: (String, Binding[P19]), f20: (String, Binding[P20]), f21: (String, Binding[P21])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21)(Tuple21[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]), f18: (String, Binding[P18]), f19: (String, Binding[P19]), f20: (String, Binding[P20]), f21: (String, Binding[P21]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params), p(f13, name, params), p(f14, name, params), p(f15, name, params), p(f16, name, params), p(f17, name, params), p(f18, name, params), p(f19, name, params), p(f20, name, params), p(f21, name, params))
  }

  // tuple version
  def mapping[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]), f18: (String, Binding[P18]), f19: (String, Binding[P19]), f20: (String, Binding[P20]), f21: (String, Binding[P21]), f22: (String, Binding[P22])) = mapping[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22)(Tuple22[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22])
  // normal version
  def mapping[T, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](f1: (String, Binding[P1]), f2: (String, Binding[P2]), f3: (String, Binding[P3]), f4: (String, Binding[P4]), f5: (String, Binding[P5]), f6: (String, Binding[P6]), f7: (String, Binding[P7]), f8: (String, Binding[P8]), f9: (String, Binding[P9]), f10: (String, Binding[P10]), f11: (String, Binding[P11]), f12: (String, Binding[P12]), f13: (String, Binding[P13]), f14: (String, Binding[P14]), f15: (String, Binding[P15]), f16: (String, Binding[P16]), f17: (String, Binding[P17]), f18: (String, Binding[P18]), f19: (String, Binding[P19]), f20: (String, Binding[P20]), f21: (String, Binding[P21]), f22: (String, Binding[P22]))(factory: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22) => T): CompoundBinding[T] = new CompoundBinding[T] {
    def fields = Seq(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22)
    def convert(name: String, params: Map[String, String]) = factory(p(f1, name, params), p(f2, name, params), p(f3, name, params), p(f4, name, params), p(f5, name, params), p(f6, name, params), p(f7, name, params), p(f8, name, params), p(f9, name, params), p(f10, name, params), p(f11, name, params), p(f12, name, params), p(f13, name, params), p(f14, name, params), p(f15, name, params), p(f16, name, params), p(f17, name, params), p(f18, name, params), p(f19, name, params), p(f20, name, params), p(f21, name, params), p(f22, name, params))
  }

  private def p[T](field: (String, Binding[T]), name: String, params: Map[String, String]): T =
    field match { case (fieldName, binding) =>
      val fullname = if (name.isEmpty) fieldName else name + "." + fieldName
      binding.convert(fullname, params)
    }
}
