package s99

object Solutions {
   // P01
  def last(list: List[Int]): Option[Int] = {
    list match {
      case Nil => None
      case last :: Nil => Some(last)
      case head :: tail => last(tail)
    }
  }
}