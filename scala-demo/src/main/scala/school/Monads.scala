package school

object Monads extends App {
  
  val fu = Some("fu")
  val ga = Some("ga")
  val zi = Some("zi")
  val no = None
  
  val word = for {
    a <- fu
    b <- no
    c <- zi
  } yield a + b + c
  
  println(word)
}