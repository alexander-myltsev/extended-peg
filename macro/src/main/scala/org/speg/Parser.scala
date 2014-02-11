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

  implicit def str(s: String): Rule = `n/a`

  implicit def ch(c: Char): Rule = `n/a`

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

  def rule(r: Rule): Rule = macro ruleImpl
}

object Parser {
  type ParserContext = Context {type PrefixType = Parser}

  def ruleImpl(ctx: ParserContext)(r: ctx.Expr[Rule]): ctx.Expr[Rule] = {
    import ctx.universe._

    def render(tree: Tree): Tree = tree match {
      case q"$a.this.str($s)" => q"""
          var ix = 0
          while (ix < $s.length && $s.charAt(ix) == p.cursorChar()) {
            ix += 1
            p.advance()
          }
          Rule(ix == $s.length)
        """
      case q"$a.this.ch($ch)" => q"""
          if ($ch == p.cursorChar()) {
            p.advance()
            Rule.MATCH
          } else Rule.MISMATCH
        """
      case q"$lhs.|($rhs)" => q"""
          val mark = p._cursor
          if (${render(lhs)}.matched) {
            Rule.MATCH
          } else {
            p._cursor = mark
            ${render(rhs)}
          }
        """
      case q"$lhs.~($rhs)" => q"""
          Rule(${render(lhs)}.matched && ${render(rhs)}.matched)
        """
      case call @ (Apply(_, _) | Select(_, _) | Ident(_)) => call
      case x => ctx.abort(tree.pos, s"Unexpected expression: $tree")
    }

    ctx.Expr[Rule](q"""
      val p = ${ctx.prefix}
      ${render(r.tree)}
    """)
  }
}
