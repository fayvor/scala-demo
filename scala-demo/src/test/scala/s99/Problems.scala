package s99

import org.junit.runner._
import org.specs2.runner._
import org.specs2.mutable._
import s99.Solutions._

@RunWith(classOf[JUnitRunner])
class Problems extends Specification {
     
  "P01: last" should {
    "return last element" in {
      last(List(1, 1, 2, 3, 5, 8)) must_== Some(8)
      last(List(1, 1, 2, 3, 5, 8, 7)) must_== Some(7)
    }
    
    "return None if list is empty" in {
      last(List()) must_== None
    } 
  }
}