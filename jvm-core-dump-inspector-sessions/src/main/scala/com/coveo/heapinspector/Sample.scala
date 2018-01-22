import com.omatz.coredumpinspector._
import com.omatz.coredumpinspector.Converters._

object Sample {
  def main(args: Array[String]): Unit = {
    import com.omatz.coredumpinspector.ObjectIterableExtensions._
    import com.omatz.coredumpinspector.JValueExtensions._

    val homePath = System.getProperty("user.home")
    val path = s"$homePath/Downloads/searchapi.bin"
    implicit val s = CoreDump(path)

    val objects = s.objects
      .filter(_.className == "my.namespace.Class")
      .filter { obj =>
        obj("some")("inner")("field").as[Int] > 100
      }

    objects.toJson(depth = 5).writeToFile(s"$homePath/Desktop/obj.json")
  }
}
