import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object helloMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
//    import Flag._
    def defaultValFromValDef(valDef: ValDef): Option[String] = {
      val value = valDef.toString().split(" = ")(1)
      if (value == "_") None else Some(value)
    }
    val result = {
      annottees.map(_.tree).toList match {
        case q"object $name extends ..$parents { ..$body }" :: Nil =>
          val newBody = body.map {
            case q"case class $name(..$args)" => {
              args.foreach { arg: ValDef =>
//                println(defaultValFromValDef(arg))
              }
              println("==== " + args)
              q"""
              case class $name(..$args)
              """
            }
            case x => x
          }
          q"""
            object $name extends ..$parents {
              def hello = "hello"
              ..$newBody
            }
          """
      }
    }
    c.Expr[Any](result)
  }
}

object Macros {
  def impl[T](c: Context)(T: c.WeakTypeTag[T]): c.Expr[Map[String, Any]] = {
    import c.universe._
    val classSym = T.tpe.typeSymbol
    val moduleSym = classSym.companionSymbol
    val apply = moduleSym.typeSignature.declaration(newTermName("apply")).asMethod
    // can handle only default parameters from the first parameter list
    // because subsequent parameter lists might depend on previous parameters
    println("====================================")
    val kvps = apply.paramss.head.map(_.asTerm).zipWithIndex.flatMap{ case (p, i) =>
      println(p.getClass)
      if (!p.isParamWithDefault) None
      else {
        val getterName = newTermName("apply$default$" + (i + 1))
        println("= " + getterName)
        Some(q"${p.name.toString} -> $moduleSym.$getterName")
      }
    }
    c.Expr[Map[String, Any]](q"Map[String, Any](..$kvps)")
  }

  def extractor[T]: Map[String, Any] = macro impl[T]
}

class hello extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro helloMacro.impl
}
