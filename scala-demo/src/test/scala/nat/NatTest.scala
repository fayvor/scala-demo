package nat

import org.junit.runner._
import org.specs2.runner._
import org.specs2.mutable._
import nat._

@RunWith(classOf[JUnitRunner])
class NatTest extends Specification {
  "nat map to & from integer" should {
    
    "be 0 for 0" in {
      Zero.toInt must_== 0
      Nat.fromInt(0) must_== Zero
    }
    
    "be 1 for 1" in {
      S(Zero).toInt must_== 1
      Nat.fromInt(1) must_== S(Zero)
    }
    
    "be 2 for 2" in {
      S(S(Zero)).toInt must_== 2
      Nat.fromInt(2) must_== S(S(Zero))
    }
  }
  
  "addition" should {
    "0 + 0 = 0" in {
      Add(Zero, Zero).toInt must_== 0
    }
    
    "1 + 0 = 1" in {
      Add(S(Zero), Zero).toInt must_== 1
    }
    
    "1 + 1 = 2" in {
      Add(S(Zero), S(Zero)).toInt must_== 2
    }
    
    "(1 + 2) + 4 = 7" in {
      Add(Add(S(Zero), S(S(Zero))), S(S(S(S(Zero))))).toInt must_== 7
      Add(Add(Nat.fromInt(1), Nat.fromInt(2)), Nat.fromInt(4)).toInt must_==7
    }
    
    "1 + (2 + 4) = 7" in {
      Add(S(Zero), Add(S(S(Zero)), S(S(S(S(Zero)))))).toInt must_== 7
      Add(Nat.fromInt(1), Add(Nat.fromInt(2), Nat.fromInt(4))).toInt must_==7
    }
  }
  
  "Successor" should {
    "be able to contain Add" in {
      val sa = S(Add(Zero, Zero))
    }
  }
  
  "Add" should {
    "be associative" in {
      // val
    }
  }
}
