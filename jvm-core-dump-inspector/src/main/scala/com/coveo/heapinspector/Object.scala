package com.omatz.coredumpinspector

import java.util.UUID

import scala.collection.JavaConverters._
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.netbeans.lib.profiler.heap._

class UnexpectedFieldValueException(anyRef: Any)
  extends Exception(s"Unexpected field value type ${anyRef.getClass.getName}")

case class Object(className: String,
                  instance: Instance)
{
  def apply(fieldName: String) = {
    val v = instance.getValueOfField(fieldName)
    if (v != null) {
      Object(v.asInstanceOf[Instance])
    } else {
      null
    }
  }

  def address = instance.getInstanceId

  def as[T]()(implicit converter: ObjectConverter[T]): T =
    converter.convert(this)

  def references =
    instance.getFieldValues.asScala
      .map(_.asInstanceOf[FieldValue].getField.getName)
      .map(apply)
      .collect { case x: Object => x }

  def refersTo =
    instance.getFieldValues.asScala.toStream
      .map(_.asInstanceOf[FieldValue])
      .filter(_.getField.getType.getName == "object")
      .map { field => instance.getValueOfField(field.getField.getName) }
      .flatMap {
        case null => Seq()

        case x: ObjectFieldValue => Seq(x.getInstance)

        case xs: ObjectArrayInstance => xs.getValues.asScala.map(_.asInstanceOf[Instance])

        case instance: Instance => Seq(instance)

        case _ => Seq()
      }
      .filter(_ != null)
      .map(_.getInstanceId)

  def toJson(depth: Int): JValue = {
    import Converters._

    def instanceToJson(maybeInstance: Option[Instance]) = maybeInstance match {
      case None => JNull
      case Some(instance) =>
        instance.getJavaClass.getName match {
          case "java.lang.String" => JString(Object(instance).as[String])
          case "java.lang.Integer" => JInt(BigInt(Object(instance).as[Int]))
          case "java.lang.Short" => JInt(BigInt(Object(instance).as[Short]))
          case "java.lang.Long" => JLong(Object(instance).as[Long])
          case "java.lang.Float" => JDouble(Object(instance).as[Float])
          case "java.lang.Double" => JDouble(Object(instance).as[Double])
          case "java.lang.Boolean" => JBool(Object(instance).as[Boolean])
          case "java.lang.Byte" => JInt(BigInt(Object(instance).as[Byte]))
          case "java.lang.Char" => JString(Object(instance).as[Char].toString)
          case "java.util.UUID" => JString(Object(instance).as[UUID].toString)
          case "scala.collection.immutable.$colon$colon" =>
            JArray(Object(instance)
              .as[List[Object]]
              .map(Option(_))
              .map { x => x.map(_.toJson(depth - 1)).getOrElse(JNull) })
          case _ => Object(instance).toJson(depth - 1)
        }
    }

    def fieldValueToJson(fieldValue: Any): JValue = fieldValue match {
      case null => JNull

      case objectFieldValue: ObjectFieldValue =>
        instanceToJson(Option(objectFieldValue.getInstance))

      case objectArrayInstance: ObjectArrayInstance =>
        JArray(objectArrayInstance
          .getValues.asScala
          .map(Option(_))
          .map {
            case None => JNull
            case Some(anyRef) =>
              Object(anyRef.asInstanceOf[Instance]).toJson(depth - 1)
          }
          .toList)

      case instance: Instance => instanceToJson(Some(instance))

      case x: Boolean => JBool(x)
      case x: Int => JInt(BigInt(x))
      case x: Short => JInt(BigInt(x))
      case x: Long => JLong(x)
      case x: Float => JDouble(x)
      case x: Double => JDouble(x)
      case x: Char => JString(x.toString)
      case x: Byte => JInt(BigInt(x))

      case x => throw new UnexpectedFieldValueException(x)
    }

    instance match {
      case null => JNull
      case _ =>
        ("id" -> instance.getInstanceId) ~
        ("class" -> instance.getJavaClass.getName) ~
        ("fields" -> (depth match {
          case 0 => None
          case _ => Some(JObject(
            instance.getFieldValues.asScala.map(_.asInstanceOf[FieldValue]).map { field =>
              JField(field.getField.getName, fieldValueToJson(instance.getValueOfField(field.getField.getName)))
            }.toList))
        }))
    }
  }
}

object Object {
  def apply(instance: Instance): Object = Object(
    className = instance.getJavaClass.getName,
    instance = instance)
}

class ObjectIterableExtensions(objects: Iterable[Object]) {
  def toJson(depth: Int): JValue = JArray(objects.map(_.toJson(depth)).toList)
}

object ObjectIterableExtensions {
  implicit def ObjectIterableExtensions(objects: Iterable[Object]) = new ObjectIterableExtensions(objects)
}
