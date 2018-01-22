package com.omatz.coredumpinspector

import java.io.File

import scala.collection.JavaConverters._
import scala.collection.mutable
import org.json4s.native.JsonMethods
import org.netbeans.lib.profiler.heap._

case class Thread(threadObjectGCRoot: ThreadObjectHprofGCRoot,
                  stackTrace: Array[StackTraceElement])

class CoreDump(val heap: Heap)
{
  def objects: Iterable[Object] = {
    val seen = new mutable.HashSet[Long]
    val todo = new mutable.Queue[Long]
    val todoSet = new mutable.HashSet[Long]

    heap.getGCRoots.asScala.toStream
      .map(_.asInstanceOf[GCRoot].getInstance.getInstanceId)
      .filter { ref => !todoSet.contains(ref) }
      .foreach { x => todo.enqueue(x); todoSet.add(x) }

    if (todo.isEmpty) {
      Iterable.empty[Object]
    } else {
      new Iterator[Object] {
        override def hasNext = !todo.isEmpty

        override def next() = {
          val id = todo.dequeue
          todoSet.remove(id)
          seen.add(id)

          val instance = heap.getInstanceByID(id)
          val `object` = Object(instance)
          `object`.refersTo
            .filter { ref => !seen.contains(ref) && !todoSet.contains(ref) }
            .foreach { ref => todo.enqueue(ref); todoSet.add(ref) }

          Object(instance)
        }
      }.toIterable
    }
  }

  def getReferenceGraph(target: Long): ReferenceGraph = {
    val referredBy = mutable.HashMap[Long, mutable.HashSet[Long]]()

    objects.foreach { x =>
      x.refersTo.foreach { y =>
        referredBy.getOrElseUpdate(y, mutable.HashSet[Long]()).add(x.address)
      }
    }

    ReferenceGraph(target, referredBy)
  }

  def getObject(address: Long): Object = Object(heap.getInstanceByID(address))

  def threads = heap.getGCRoots.asScala.collect {
    case x: ThreadObjectHprofGCRoot => Thread(x, x.getStackTrace)
  }
}

object CoreDump extends JsonMethods {
  def apply(filename: String): CoreDump =
    new CoreDump(HeapFactory.createHeap(new File(filename)))
}
