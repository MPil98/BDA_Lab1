import java.io.PrintWriter
import scala.collection.mutable
import scala.io.Source
import scala.sys.process.processInternal.File
object Task2 {
  def main(aegs: Array[String]): Unit = {
    val punatations = List(",", ".", "--", "-", ";", ":", "?", "!", "'", '"')
    val OiM = Source.fromFile("C:/Users/Pc/Desktop/Ognem_i_mieczem.txt").mkString
    var oim = OiM.replaceAll("\\p{Punct}", "").split("\\s+").toList
    val stop_words = Source.fromFile("C:/Users/Pc/Desktop/stop_words_english.txt").mkString.split("\\s+")
    //println(stop_words)
    oim=oim.filter(x=> !(stop_words.contains(x)))
    val keys = oim.distinct
    val values = keys.map(e => oim.count(_==e))
    val map = (keys zip values).toMap
    var cloud = mutable.ListBuffer.empty[String]
    val plik= new PrintWriter(new File("cloud.csv"))
    for(t <-map) {
      if (t._2>500) {
        println(t._1)
        cloud+=t._1
        plik.write(t._1)
      }
    }

  }
}