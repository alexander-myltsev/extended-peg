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

import support._

class Rule[T <: Node](val matched: Boolean) {
  def |[U <: Node](r: Rule[U]): Rule[FirstOfMatch[T, U]] = `n/a`

  def ~[U <: Node](r: Rule[U]): Rule[SeqMatch[T, U]] = `n/a`
}

object Rule {
  def apply[T <: Node](matched: Boolean) = new Rule[T](matched)

  def MATCH[T <: Node] = Rule[T](true)

  def MISMATCH[T <: Node] = Rule[T](false)
}
