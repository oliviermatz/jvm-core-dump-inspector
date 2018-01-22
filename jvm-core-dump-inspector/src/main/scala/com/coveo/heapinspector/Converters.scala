package com.omatz.coredumpinspector

import java.util.UUID

import scala.collection.JavaConverters._
import org.netbeans.lib.profiler.heap.{Instance, PrimitiveArrayInstance}

import scala.annotation.tailrec

class InvalidCastException(instance: Instance, expectedClassName: String)
  extends Exception(s"Invalid cast. Expected: ${expectedClassName}, Actual: ${instance.getJavaClass.getName}")

trait ObjectConverter[T] {
  def convert(target: Object): T
}

object Converters {
  private def assertIsOfClass(instance: Instance, className: String) =
    if (instance.getJavaClass.getName != className)
      throw new InvalidCastException(instance, className)

  implicit object StringConverter extends ObjectConverter[String] {
    override def convert(target: Object) =
      target("value").instance.asInstanceOf[PrimitiveArrayInstance]
        .getValues.asScala.map(_.asInstanceOf[java.lang.String])
        .mkString
  }

  implicit object BooleanConverter extends ObjectConverter[Boolean] {
    override def convert(target: Object) = {
      assertIsOfClass(target.instance, "java.lang.Boolean")
      target.instance.getValueOfField("value").asInstanceOf[Boolean]
    }
  }

  implicit object IntegerConverter extends ObjectConverter[Int] {
    override def convert(target: Object) = {
      assertIsOfClass(target.instance, "java.lang.Integer")
      target.instance.getValueOfField("value").asInstanceOf[Int]
    }
  }

  implicit object ByteConverter extends ObjectConverter[Byte] {
    override def convert(target: Object) = {
      assertIsOfClass(target.instance, "java.lang.Byte")
      target.instance.getValueOfField("value").asInstanceOf[Byte]
    }
  }

  implicit object ShortConverter extends ObjectConverter[Short] {
    override def convert(target: Object) =
      target.instance.getValueOfField("value").asInstanceOf[Short]
  }

  implicit object CharConverter extends ObjectConverter[Char] {
    override def convert(target: Object) = {
      assertIsOfClass(target.instance, "java.lang.Char")
      target.instance.getValueOfField("value").asInstanceOf[Char]
    }
  }

  implicit object LongConverter extends ObjectConverter[Long] {
    override def convert(target: Object) = {
      assertIsOfClass(target.instance, "java.lang.Long")
      target.instance.getValueOfField("value").asInstanceOf[Long]
    }
  }

  implicit object FloatConverter extends ObjectConverter[Float] {
    override def convert(target: Object) = {
      assertIsOfClass(target.instance, "java.lang.Float")
      target.instance.getValueOfField("value").asInstanceOf[Float]
    }
  }

  implicit object DoubleConverter extends ObjectConverter[Double] {
    override def convert(target: Object) = {
      assertIsOfClass(target.instance, "java.lang.Double")
      target.instance.getValueOfField("value").asInstanceOf[Double]
    }
  }

  implicit object UUIDConverter extends ObjectConverter[UUID] {
    override def convert(target: Object) = {
      assertIsOfClass(target.instance, "java.util.UUID")
      target.instance.getValueOfField("value").asInstanceOf[UUID]
    }
  }

  implicit object ListConverter extends ObjectConverter[List[Object]] {
    override def convert(target: Object) = {
      @tailrec
      def loop(current: Object, acc: List[Object]): List[Object] = current match {
        case null => acc.reverse
        case x: Object => loop(x("tl"), x("head") +: acc)
      }
      loop(target, Nil)
    }
  }
}
