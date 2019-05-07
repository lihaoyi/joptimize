package joptimize.model


object CType{
  def toJType(self: IType, fallback: JType): JType = self match{
    case j: JType => j
    case CType.I(_) => JType.Prim.I
    case CType.J(_) => JType.Prim.J
    case CType.F(_) => JType.Prim.F
    case CType.D(_) => JType.Prim.D
    case CType.Intersect(classes) => fallback
    case IType.Bottom => fallback
  }

  case class Intersect(classes: Seq[JType.Cls]) extends IType{
    def size = 1
    def internalName: String = "?"
    def name = s"R${classes.length}${classes.map(_.name.replace('/', '_')).mkString("__")}"
    def isRef = true
    def widen = this
    def isConstant = false
  }
  trait Constant[T] extends IType{
    def isConstant = true
    def value: T
  }
  trait ConstantCompanion[T] {
    def apply(value: T): Constant[T]
//    def unapply(value: IType): Option[T]
    def wide: JType
  }
  object I extends ConstantCompanion[Int]{
    def apply(value: Int) = new I(value)
//    def unapply(value: IType) = value match{
//      case x: I => Some(x.value)
//      case _ => None
//    }
    def wide = JType.Prim.I
  }
  case class I(value: Int) extends Constant[Int]{
    def size = 1
    def isRef = false
    def internalName = s"TI$value;"
    def name = s"TI$value"
    def widen = JType.Prim.I
  }
  object J extends ConstantCompanion[Long]{
    def apply(value: Long) = new J(value)
//    def unapply(value: IType) = value match{
//      case x: J => Some(x.value)
//      case _ => None
//    }
    def wide = JType.Prim.J
  }
  case class J(value: Long) extends Constant[Long]{
    def size = 2
    def isRef = false
    def internalName = s"TJ$value;"
    def name = s"TJ$value"
    def widen = JType.Prim.J
  }
  object F extends ConstantCompanion[Float]{
    def apply(value: Float) = new F(value)
    def unapply(value: IType) = value match{
      case x: F => Some(x.value)
      case _ => None
    }
    def wide = JType.Prim.F
  }
  case class F(value: Float) extends Constant[Float]{
    def size = 1
    def isRef = false
    def internalName = s"TF$value;"
    def name = s"TF$value"
    def widen = JType.Prim.F
  }
  object D extends ConstantCompanion[Double]{
    def apply(value: Double) = new D(value)
    def unapply(value: IType) = value match{
      case x: D => Some(x.value)
      case _ => None
    }
    def wide = JType.Prim.D
  }
  case class D(value: Double) extends Constant[Double]{
    def size = 2
    def isRef = false
    def internalName = s"TD$value;"
    def name = s"TD$value"
    def widen = JType.Prim.D
  }

  /**
    * Weird "meaningless type" sentinel value. Unlike Void, it has a size of 1
    * and can take up spots in the stack or locals, but doesn't really have any
    * meaning beyond a placeholder and all operations on it should fail
    */
  object Null extends IType {
    override def name = "Null"

    def widen = Null

    def isConstant = true
  }
}
