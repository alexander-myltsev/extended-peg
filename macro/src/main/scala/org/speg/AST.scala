package org.speg

abstract class Node
case class StringMatch() extends Node
case class CharMatch() extends Node
case class FirstOfMatch[+L <: Node, -R <: Node]() extends Node
case class SeqMatch[+L <: Node, -R <: Node]() extends Node
