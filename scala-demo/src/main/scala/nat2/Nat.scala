// package nat2

import algebra._
import func.EqF

trait Nat

case class Succ(n: Nat) extends Nat

object Zero extends Nat

// object NatAdd {
  
    sealed trait ANat {
	  // def flatten: ANat
	  // def reduce: Nat
	}
    
	sealed trait AZero extends ANat
	
	object AZero extends AZero {
	  // def flatten: ANat = this
	  // def reduce = Zero
	}
	
	case class ASucc[S <: ANat](n: S) extends ANat {
	  // def flatten = ASucc(n.flatten)
	  // def reduce = Succ(n.reduce)
	}
	
	case class Add[+S <: ANat, +T <: ANat](left: S, right: T) extends ANat {
//	  def flatten = right match {
//	    case AZero => left.flatten
//	    case ASucc(n) => ASucc(Add(left, n).flatten)
//	    case _: Add => Add(left, right.flatten)
//	  }
	  // def reduce = this.flatten.reduce
	}
	
	
	object Add {
	  type AEqF[S <: ANat, T <: ANat] = EqF[ANat, S, T]
	  
	  // a => a + 0
	  def addZero[A <: ANat] = new EqF[ANat, A, Add[A, AZero]] {
	    def apply(a: A) = Add(a, AZero)
	    def unapply(add: Add[A, AZero]) = add.left
	  }
	  
	  // a + S(b) => S(a+b)
	  def raiseSucc[A <: ANat, B <: ANat] = new EqF[ANat, Add[A, ASucc[B]], ASucc[Add[A, B]]] {
	    def apply(a: Add[A, ASucc[B]]) = ASucc(Add(a.left, a.right match { case ASucc(x) => x }))
	    def unapply(a: ASucc[Add[A, B]]) = a match { case ASucc(x: Add[A, B]) => Add(x.left, ASucc(x.right))}
	  }
	  
	  // a + b => a + f(b)
	  def applyToRight[U <: ANat, V <: ANat] = (f: AEqF[U, V]) => new AEqF[Add[ANat, U], Add[ANat, V]] {
	    def apply(a: Add[ANat, U]): Add[ANat, V] = Add(a.left, f.apply(a.right))
	    def unapply(a: Add[ANat, V]): Add[ANat, U] = Add(a.left, f.unapply(a.right))
	  }
	  
	  // S(a) => S(f(a))
	  def applyToPred[U <: ANat, V <: ANat] = (f: AEqF[U, V]) => new AEqF[ASucc[U], ASucc[V]] {
	    def apply(s: ASucc[U]): ASucc[V] = s match { case ASucc(u) => ASucc(f.apply(u))}
	    def unapply(s: ASucc[V]): ASucc[U] = s match { case ASucc(v) => ASucc(f.unapply(v))}
	  }
	  
	  // a + b => a + (b + 0)
	  def addZeroToRight = applyToRight(addZero[ANat])
	  
	  // (a + b) + 0 => a + (b + 0)
	  def associateRightZero = addZeroToRight compose addZero[Add[ANat, ANat]].invert
	  
	  // (a + (b + S(c)) => a + S(b + c)
	  def raiseSuccOnRight[B <: ANat, C <: ANat] = applyToRight(raiseSucc[B, C])
	  
	  /*
	   * steps of associativity proof
	   * (a + b) + c = a + (b + c)
	   */
	  
	  // base case
	  // (a + b) + 0 = a + (b + 0)
	  def base = associateRightZero
	  
	  // induction
	  // (a + b) + S(c) = a + (b + S(c))
	  def indy = i4 compose i3 compose i2 compose i1
	  
	  // (a + b) + S(c) = S((a + b) + c)
	  def i1 = raiseSucc[Add[ANat, ANat], ANat]
	  
	  // S((a + b) + c) = S(a + (b + c))
	  def i2 = applyToPred(associateRight) // (associateRightZero, associateRightSucc, associateRightAdd)
	  
	  // S(a + (b + c)) = a + S(b + c)
	  def i3 = raiseSucc[ANat, Add[ANat, ANat]].invert
	  
	  // a + S(b + c) = a + (b + S(c))
	  def i4 = raiseSuccOnRight[ANat, ANat].invert
	  
	  // (a + b) + c = a + (b + c)
	  def associateRight: AEqF[Add[Add[ANat, ANat], ANat], Add[ANat, Add[ANat, ANat]]] = new AEqF[Add[Add[ANat, ANat], ANat], Add[ANat, Add[ANat, ANat]]] {
	    def apply(a: Add[Add[ANat, ANat], ANat]) = a match {
	      case x: Add[Add[ANat, ANat], AZero] => base.apply(x)
 	      case x: Add[Add[ANat, ANat], ASucc[ANat]] => indy.apply(x)
	    }
	  }
	  
	  // a + (b + c) = (a + b) + c
	  def associateLeft = associateRight.invert
	}
	
	object Associate {
	  def associateZero = new EqF[ANat, Add[Add[ANat, ANat], AZero], Add[ANat, Add[ANat, AZero]]] {
	    def apply(a: Add[Add[ANat, ANat], AZero]) = Add(a.left.left, Add(a.left.right, AZero))
	    def unapply(a: Add[ANat, Add[ANat, AZero]]) = Add(Add(a.left, a.right.left), AZero)
	  }
	  
	}
	
//	def associateRight(n: Add[Add[_, _], ANat]): Add[ANat, Add[_, _]] = n match {
//	  case x: Add[ANat, AZero] => Add(Add.addZero.unapply(x)
//	  case _: ASucc => n
//	  case x: Add => Add(x.left, Add(x.right, n.right)) // doesn't make use of definition
//	}
// }
