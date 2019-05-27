package joptimize.model

import collection.mutable
import reflect.ClassTag

object JType {
  def fromJavaCls(c: Class[_]): JType =
    if (c.isPrimitive) Prim.allJava(c.getTypeName)
    else if (c.isArray) Arr(fromJavaCls(c.getComponentType))
    else Cls(c.getName)

  def read(s: String): JType = s match {
    case x if Prim.all.contains(x(0)) => Prim.all(x(0))
    case s if s.startsWith("L") && s.endsWith(";") => Cls.apply(s.drop(1).dropRight(1))
    case s if s.startsWith("[") => Arr.read(s)
    case s => Cls.apply(s)
  }

  /**
    * Reference types, which can either be Class or Array types
    */
  trait Ref extends JType
  object Arr {
    def read(s: String) = Arr(JType.read(s.drop(1)))
  }

  /**
    * Array Types
    *
    * @param innerType The type of the components of the array
    */
  case class Arr(innerType: JType) extends Ref {
    def size = 1
    def name = innerType match {
      case tpe: Cls => "[L" + tpe.name + ";"
      case tpe: Prim => "[" + tpe.internalName
      case tpe => "[" + tpe.javaName
    }
    def javaName = innerType match {
      case tpe: Cls => "[L" + tpe.javaName + ";"
      case tpe: Prim => "[" + tpe.internalName
      case tpe => "[" + tpe.javaName
    }
    def internalName = "[" + innerType.internalName
  }
  object Cls {
    implicit def apply(name: String): Cls = new Cls(name.replace('.', '/'))
    def unapply(cls: Cls): Option[String] = Some(cls.name)
  }

  /**
    * Class Types
    *
    * @param name the fully qualified name of the class
    */
  class Cls private (val name: String) extends Ref {
    def size = 1

    override val hashCode = name.hashCode
    override def equals(other: Any) = other match {
      case o: Cls => o.name == name
      case _ => false
    }
    override def toString = s"Cls($name)"
    def internalName = "L" + name + ";"
    def javaName = name.replace('/', '.')
  }

  class Prim(val size: Int, val javaName: String)(implicit sname: sourcecode.Name) extends JType {
    def name = sname.value
    def internalName = sname.value
  }
  object Prim extends {
    val V = new Prim(0, "void")
    val Z = new Prim(1, "boolean")
    val B = new Prim(1, "byte")
    val C = new Prim(1, "char")
    val S = new Prim(1, "short")
    val I = new Prim(1, "int")
    val F = new Prim(1, "float")
    val J = new Prim(2, "long")
    val D = new Prim(2, "double")

    def read(s: String) = all(s(0))
    val all: Map[Char, Prim] = Map(
      'V' -> (V: Prim),
      'Z' -> (Z: Prim),
      'B' -> (B: Prim),
      'C' -> (C: Prim),
      'S' -> (S: Prim),
      'I' -> (I: Prim),
      'F' -> (F: Prim),
      'J' -> (J: Prim),
      'D' -> (D: Prim)
    )

    val allJava: Map[String, Prim] = Map(
      "void" -> (V: Prim),
      "boolean" -> (Z: Prim),
      "byte" -> (B: Prim),
      "char" -> (C: Prim),
      "short" -> (S: Prim),
      "int" -> (I: Prim),
      "float" -> (F: Prim),
      "long" -> (J: Prim),
      "double" -> (D: Prim)
    )

    def unapply(p: Prim) = Some(p.javaName)

  }

  case object Bottom extends JType {
    def name = "Bottom"

    def internalName = "Lscala/runtime/Nothing$;"

    def javaName = "scala.runtime.Nothing$"

    def size = 0
  }
}

/**
  * Represents all variable types within the Metascala VM
  */
sealed trait JType extends IType {

  /**
    * The JVMs internal representation
    * - V Z B C S I F J D
    * - Ljava/lang/Object; [Ljava/lang/String;
    */
  def internalName: String

  /**
    * Nice name to use for most things
    * - V Z B C S I F J D
    * - java/lang/Object [java/lang/String
    */
  def name: String

  override def toString = name

  /**
    * The thing that's returned by Java's getName method
    * - void boolean byte char short int float long double
    * - java.lang.Object [java.lang.String;
    */
  def javaName: String

  /**
    * 0, 1 or 2 for void, most things and double/long
    */
  def size: Int

}

object Desc {
  def read(s: String) = {
    val scala.Array(argString, ret) = s.drop(1).split(')')
    val args = mutable.Buffer.empty[String]
    var index = 0
    while (index < argString.length) {
      val firstChar = argString.indexWhere(x => "BCDFIJSZL".contains(x), index)
      val split = argString(firstChar) match {
        case 'L' => argString.indexWhere(x => ";".contains(x), index)
        case _ => argString.indexWhere(x => "BCDFIJSZ".contains(x), index)
      }

      args.append(argString.substring(index, split + 1))
      index = split + 1
    }
    Desc(
      args.map(JType.read),
      JType.read(ret) match {
        case l => if (l.name == "scala/runtime/Nothing$") JType.Bottom else l
      }
    )
  }
  def render(t: JType): String = {
    t match {
      case t: JType.Cls => t.internalName
      case t: JType.Arr => "[" + render(t.innerType)
      case x => x.internalName
    }
  }
}

/**
  * Represents the signature of a method.
  */
case class Desc(args: Seq[JType], ret: JType) {
  def render = "(" + args.map(Desc.render).foldLeft("")(_ + _) + ")" + Desc.render(ret)

  override def toString = render
}

case class MethodSig(cls: JType.Cls, name: String, desc: Desc, static: Boolean) {
  override def toString = {
    cls.javaName + (if (static) "." else "#") + name + desc.render
  }
}

case class InferredSig(method: MethodSig, inferred: Seq[IType]) {
  assert(method.desc.args.length == inferred.length, method + " " + inferred)
  assert(!inferred.contains(JType.Bottom), inferred)
  override def toString = {
    method.toString + inferred.map(_.name).mkString("(", ", ", ")")
  }
}
