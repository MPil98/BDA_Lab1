import io.Source
import scala.collection.mutable.Map
import scala.math._

class CLoud_Many(var docs: Array[String]){
  val stop_words = Source.fromFile("C:/Users/Pc/Desktop/stop_words_english.txt").mkString.split("\\s+")
  var maps=Map()
  var all = ""
  for (d<-docs){
    all+=d
    all+=" "
  }
  var Lista=all.replaceAll("\\p{Punct}", "").split("\\s+").toList
  Lista=Lista.filter(x=> !(stop_words.contains(x)))
  val keys = Lista.distinct
  val values = keys.map(e => Lista.count(_==e))
  val map = (keys zip values).toMap

  def cloud_one(input:Int,no_words:Int): Unit ={
    val stop_words = Source.fromFile("C:/Users/Pc/Desktop/stop_words_english.txt").mkString.split("\\s+")
    val all = docs(input-1)
    var Lista=all.replaceAll("\\p{Punct}", "").split("\\s+").toList
    Lista=Lista.filter(x=> !(stop_words.contains(x)))
    val keys = Lista.distinct
    val values = keys.map(e => Lista.count(_==e))
    val map = (keys zip values).toMap
    val sorted_seq = map.toSeq.sortWith(_._2 > _._2)
    println(sorted_seq.take(no_words))
  }
  def print_word_cloud(number_of_words:Int):Unit= {
    val sorted_seq = map.toSeq.sortWith(_._2 > _._2)
    println(sorted_seq.take(number_of_words))
  }
  def print={
    println(map)
  }
  def TDIDF(word:String, doc:Int): Map[Int,Map[String,Double]]={
    var c=docs(doc).split("\\s+").toList
    var sum = c.count(_==word)
    var n_doc=0
    for (i <- docs){
      if (i.contains(word)){
        n_doc+=1
      }
    }
    val TDiDF =sum*log10(docs.size/n_doc)
    val a=Map(word->TDiDF)
    val b = Map(doc+1->a)
    return b
  }


}

object cos{
  def main(array: Array[String]):Unit={
    val input =io.StdIn.readLine("How many files? ").toInt
    var files = Array[String]()
    for (i<-0 to input-1){
      var in_file = io.StdIn.readLine((i+1).toString+" file: ")
      files=files :+ Source.fromFile(in_file).mkString
    }
    var run=true
    while(run) {
      val input2 = io.StdIn.readLine("What do you want do? 1 - calculate TFIDF; 2 - generate cloud for all; 3 - generate cloud for one file; other number - exit ").toInt
      if (input2 == 1) {
        val word = io.StdIn.readLine("Give word ")
        val number = io.StdIn.readLine("Give number of file ").toInt - 1
        val TFIDf = new CLoud_Many(files).TDIDF(word, number)
        println(TFIDf)
      } else if (input2 == 2) {
        val number = io.StdIn.readLine("Give number of words ").toInt
        val cloud = new CLoud_Many(files).print_word_cloud(number)
      } else if (input2 == 3) {
        val number_o_f = io.StdIn.readLine("Give file number ").toInt
        val number = io.StdIn.readLine("Give number of words ").toInt
        val cloud = new CLoud_Many(files).cloud_one(number_o_f, number)
      } else {
        println("CONGRATULATIONS - you exit from this program ;)")
        run=false
      }
    }
  }
}
