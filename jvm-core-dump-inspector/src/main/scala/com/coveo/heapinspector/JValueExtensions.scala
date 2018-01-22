package com.omatz.coredumpinspector

import java.nio.file.{Files, Paths}

import org.json4s.native.JsonMethods._
import org.json4s.JsonAST.JValue

class JValueExtensions(value: JValue) {
  def writeToFile(path: String): Unit =
    Files.write(
      Paths.get(path),
      pretty(render(value)).getBytes)
}

object JValueExtensions {
  implicit def JValueExtensions(value: JValue) = new JValueExtensions(value)
}

