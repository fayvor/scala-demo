package lsh

import org.junit.runner._
import org.specs2.runner._
import org.specs2.mutable._
import lsh.Jaccard._

@RunWith(classOf[JUnitRunner])
class JaccardTest extends Specification {
  "similarity" should {
    val set1 = Set(1, 2, 3, 4)
    val set2 = Set(1, 2, 3, 4)
    val set3 = Set(3, 4)
    val set4 = Set(3, 4, 5, 6)
    val set5 = Set(7, 8, 9, 10)
    
    "be 1 for total match" in {
      similarity(set1, set2) must_== 1
    }
  }
}