package org.speg

import scala.reflect.macros.Context

abstract class Node
case class StringMatch() extends Node
case class CharMatch() extends Node
case class FirstOfMatch[+L <: Node, +R <: Node]() extends Node
case class SeqMatch[+L <: Node, +R <: Node]() extends Node
case class LiteralSingleton[In <: Singleton]() extends Node { type T = In }

object LiteralSingleton {
  def applyImpl[A: c.WeakTypeTag](c: Context)(a: c.Expr[A]) = {
    import c.universe._

    def `new`(t: Type) = Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), Nil)

    a.tree match {
      case Literal(const: Constant) ⇒ c.Expr[Rule[LiteralSingleton[_]]](
        q"""org.speg.Rule(${
          `new`(
            appliedType(
              typeOf[LiteralSingleton[_]].typeConstructor,
              ConstantType(const) :: Nil
            ))
        })"""
      )
      case _ ⇒ c.abort(c.enclosingPosition, "Not a literal!")
    }
  }
}
