/*
 * Copyright (C) 2014 Alexander Myltsev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.speg

import scala.reflect.macros.Context

abstract class Parser {
  import Parser._
  import support._

  val EOI = '\uFFFF'

  implicit def str(s: String): Rule[StringMatch] = `n/a`

  implicit def ch(c: Char): Rule[CharMatch] = `n/a`

  def input: ParserInput

  var _cursor: Int = 0
  def cursorChar() =
    if (_cursor == input.length) EOI
    else input charAt _cursor

  def advance(): Boolean =
    if (_cursor < input.length) {
      _cursor += 1
      true
    } else false

  def stringMatch(s: String) = {
    var ix = 0
    while (ix < s.length && s.charAt(ix) == cursorChar()) {
      ix += 1
      advance()
    }
    ix == s.length
  }

  def charMatch(ch: Char) =
    if (ch == cursorChar()) {
      advance()
      true
    } else false

  def rule[I <: Node](r: Rule[I]): Rule[I] = macro ruleImpl[I]

  def ^[A](a: A) = macro LiteralSingleton.applyImpl[A]
}

object Parser {
  type ParserContext = Context { type PrefixType = Parser }

  def ruleImpl[I <: Node: ctx.WeakTypeTag](ctx: ParserContext)(r: ctx.Expr[Rule[I]]): ctx.Expr[Rule[I]] = {
    import ctx.universe._

    def render(tree: Tree): Tree = tree match {
      case q"$a.this.str($s)" ⇒ q"""Rule(p.stringMatch($s))"""
      case q"$a.this.ch($ch)" ⇒ q"""Rule(p.charMatch($ch))"""
      case q"$lhs.|[$t]($rhs)" ⇒ q"""
          val mark = p._cursor
          if (${render(lhs)}.matched) Rule.MATCH
          else {
            p._cursor = mark
            ${render(rhs)}
          }
        """
      case q"$lhs.~[$t]($rhs)" ⇒ q"""
          Rule(${render(lhs)}.matched && ${render(rhs)}.matched)
        """
      case q"org.speg.Rule.apply[$t]($ls)" ⇒ {
        val tpRE = """org.speg.LiteralSingleton\[String\("(\w+)"\)\]""".r
        val tpRE(tp) = t.toString
        q"""
          Rule(p.stringMatch($tp))
        """
      }
      case call @ (Apply(_, _) | Select(_, _) | Ident(_)) ⇒ {
        println(s" >> Inner rule call:\n  $call\n  > Structure: ${call.tpe}")
        def prettyPrint(tp: Type, indent: Int = 0): Unit = {
          val is = " " * indent
          tp match {
            case x if x <:< typeOf[StringMatch] ⇒
              println(s"$is* StringMatch")
            case x @ TypeRef(_, _, List(lhs, rhs)) if x <:< typeOf[FirstOfMatch[_, _]] ⇒
              println(s"$is* FirstOfMatch")
              prettyPrint(lhs, indent + 1)
              prettyPrint(rhs, indent + 1)
            case x @ TypeRef(_, _, List(lhs, rhs)) if x <:< typeOf[SeqMatch[_, _]] ⇒
              println(s"$is* SeqMatch")
              prettyPrint(lhs, indent + 1)
              prettyPrint(rhs, indent + 1)
            case x @ TypeRef(_, _, List(t)) if x <:< typeOf[Rule[_]] ⇒
              println(s"$is* Rule[$t]")
              prettyPrint(t, indent + 1)
            case x if x <:< typeOf[Node] ⇒
              println(s"$is* Node")
            case _ ⇒
              ctx.abort(call.pos, s"Unknown type structure: $tp")
          }
        }
        prettyPrint(call.tpe)
        call
      }
      case x ⇒ ctx.abort(tree.pos, s"Unexpected expression: $tree")
    }

    val res = ctx.Expr[Rule[I]](q"""
      val p = ${ctx.prefix}
      ${render(r.tree)}
    """)
    val s = "-" * 10 + "\n"
    println(s"$s$s${res.toString.replaceAll("org.speg.", "")}\n$s")
    res
  }
}
