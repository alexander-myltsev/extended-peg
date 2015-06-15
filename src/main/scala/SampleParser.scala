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

package org.speg.examples

import org.speg._
import scala.annotation.tailrec

object SampleParser extends App {
  repl()

  @tailrec
  def repl(): Unit = {
    print("---\nEnter expression for sample-parser > ")
    Console.out.flush()
    readLine() match {
      case "" ⇒
      case line ⇒
        val parser = new SampleParser(line)
        println(s"Expression matched is '${parser.InputRule.matched}'")
        repl()
    }
  }
}

class SampleParser(val input: ParserInput) extends Parser {
  //def b = rule { "b" }
  //def bc = rule { b | "c" }
  //def InputRule: Rule[SeqMatch[FirstOfMatch[StringMatch, StringMatch], Node]] = rule { bc ~ InputRule }
  def ab = rule { ^("ab") }
  def InputRule = rule { ab ~ "c" }
}
