package com.omatz.coredumpinspector

import org.json4s.JsonDSL._
import org.json4s.JValue

import scala.collection.mutable

case class ReferenceGraph(target: Long, edges: mutable.HashMap[Long, mutable.HashSet[Long]])
{
  def toJson(depth: Int)(implicit coreDump: CoreDump) = {
    def loop(target: Long, depth: Int): JValue = {
      ("target" -> coreDump.getObject(target).toJson(depth - 1)) ~
      ("referredBy" -> edges.getOrElse(target, Seq.empty).map(loop(_, depth - 1)))
    }
    loop(target, depth)
  }
}

