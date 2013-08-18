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
	
	case class ASucc(n: ANat) extends ANat {
	  // def flatten = ASucc(n.flatten)
	  // def reduce = Succ(n.reduce)
	}
	
	case class Add[S <: ANat, T <: ANat](left: S, right: T) extends ANat {
//	  def flatten = right match {
//	    case AZero => left.flatten
//	    case ASucc(n) => ASucc(Add(left, n).flatten)
//	    case _: Add => Add(left, right.flatten)
//	  }
	  // def reduce = this.flatten.reduce
	}
	
	
	object Add {
	  type AEqF[S <: ANat, T <: ANat] = EqF[ANat, S, T]
	  
	  def addZero = new EqF[ANat, ANat, Add[ANat, AZero]] {
	    def apply(a: ANat) = Add(a, AZero)
	    def unapply(a: Add[ANat, AZero]) = a.left
	  }
	  
	  def addSucc = new EqF[ANat, Add[ANat, ASucc], ASucc] {
	    def apply(a: Add[ANat, ASucc]) = ASucc(Add(a.left, a.right match { case ASucc(x) => x }))
	    def unapply(a: ASucc) = a match { case ASucc(x: Add[_, _]) => Add(x.left, ASucc(x.right))}
	  }
	  
	  def applyToRight[U <: ANat, V <: ANat] = (f: AEqF[U, V]) => new AEqF[Add[ANat, U], Add[ANat, V]] {
	    def apply(a: Add[ANat, U]): Add[ANat, V] = Add(a.left, f.apply(a.right))
	    def unapply(a: Add[ANat, V]): Add[ANat, U] = Add(a.left, f.unapply(a.right))
	  }
	  
	  def addZeroToRight = applyToRight(addZero)
	  
	  
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
