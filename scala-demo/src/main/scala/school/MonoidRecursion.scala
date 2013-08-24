package school

object MonoidRecursion extends App {
  def sum(l: List[Int]): Int = {
    val head::tail = l
    println(head)
    tail match {
      case Nil => head + 0
      case _ => head + sum(tail)
    }
    
  }
  
  // println(sum(List(1,2,3)))
  val l = List(1,2,3)
  val l2 = l.flatMap((i: Int) => List(i, 5))
  println(l2)
}