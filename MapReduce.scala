import scala.collection.mutable._
class Node(nodes: ListBuffer[(Int,ListBuffer[Int])]){
  def compute:ListBuffer[(Int,ListBuffer[Int])]={
    var a = ListBuffer[(Int,ListBuffer[Int])]()
    for (n<-nodes) {
      for(nn<-n._2) {
        a += Tuple2(nn, ListBuffer(n._1))
      }
    }
    return a
  }
}
class MapReduce(graph: Map[Int,ListBuffer[Int]]){
  def mapa:ListBuffer[(Int,ListBuffer[Int])]={
    var nodes =ListBuffer[(Int,ListBuffer[Int])]()
    for(n<-graph){
      nodes+=n
    }
    return nodes
  }
}
class Reduce(nodes: ListBuffer[(Int,ListBuffer[Int])]){
  def cobination: ListBuffer[(Int,ListBuffer[Int])]={
    var a = ListBuffer[(Int,ListBuffer[Int])]()
    while(nodes.size>0){
      val n=nodes(0)
      var m=ListBuffer(n._2(0))
      nodes-=n
      for (i<-nodes){
        if(i._1==n._1){
          m+=i._2(0)
        }
      }
      a+=Tuple2(n._1,m)
      val b=nodes.filter(_._1==n._1)
      for (i<-b){
        nodes-=i
      }
      }

    return a
    }
  }


object MapReduce {
def main(args:Array[String]):Unit={
    val graf = Map(1->ListBuffer(2,3),2->ListBuffer(2,3),3->ListBuffer(4))
    val a = new Reduce(new Node(new MapReduce(graf).mapa).compute).cobination
  println(a)


  }
}
