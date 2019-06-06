package joptimize.model

trait CType extends IType
object CType {
  def toJType(self: IType): JType = self match {
    case j: JType => j
    case CType.I(_) => JType.Prim.I
    case CType.J(_) => JType.Prim.J
    case CType.F(_) => JType.Prim.F
    case CType.D(_) => JType.Prim.D
//    case CType.Intersect(classes) => fallback
    case JType.Bottom => JType.Bottom
  }

//  case class Intersect(classes: Seq[JType.Cls]) extends IType {
//    def size = 1
//    def internalName: String = "?"
//    def name = s"R${classes.length}${classes.map(_.name.replace('/', '_')).mkString("__")}"
//  }
  case class I(value: Int) extends CType {
    def size = 1
    def internalName = s"TI$value;"
    def name = s"TI$value"
  }
  case class J(value: Long) extends CType {
    def size = 2
    def internalName = s"TJ$value;"
    def name = s"TJ$value"
  }
  case class F(value: Float) extends CType {
    def size = 1
    def internalName = s"TF$value;"
    def name = s"TF$value"
  }
  case class D(value: Double) extends CType {
    def size = 2
    def internalName = s"TD$value;"
    def name = s"TD$value"
  }

  /**
    * Weird "meaningless type" sentinel value. Unlike Void, it has a size of 1
    * and can take up spots in the stack or locals, but doesn't really have any
    * meaning beyond a placeholder and all operations on it should fail
    */
  object Null extends CType {
    override def name = "Null"
  }
}
