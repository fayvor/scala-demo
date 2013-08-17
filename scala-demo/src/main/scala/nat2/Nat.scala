package nat2

import algebra._
import func.EqF

trait Nat

case class Succ(n: Nat) extends Nat

object Zero extends Nat

object NatAdd {
  
    sealed trait ANat {
	  def flatten: ANat
	  def reduce: Nat
	}
	
	case class ASucc(n: ANat) extends ANat {
	  def flatten = ASucc(n.flatten)
	  def reduce = Succ(n.reduce)
	}
	
	object AZero extends ANat {
	  def flatten: ANat = this
	  def reduce = Zero
	}
	
	case class Add(left: ANat, right: ANat) extends ANat {
	  def flatten = right match {
	    case AZero => left.flatten
	    case ASucc(n) => ASucc(Add(left, n).flatten)
	    case _: Add => Add(left, right.flatten)
	  }
	  def reduce = this.flatten.reduce
	}
	
	object Add {
	  def addZero = new EqF[ANat, ANat, Add] {
	    def apply(a: ANat) = Add(a, AZero)
	    def unapply(a: Add) = a.left
	  }
	  
	  def addSucc = new EqF[ANat, Add, ASucc] {
	    def apply(a: Add) = ASucc(a.right match { case ASucc(x) => x })
	    def unapply(a: ASucc) = a match { case ASucc(x: Add) => Add(x.left, ASucc(x.right))}
	  }
	}
	
	def associateRight(n: Add): Add = n.left match {
	  case AZero => n
	  case _: ASucc => n
	  case x: Add => Add(x.left, Add(x.right, n.right)) // doesn't make use of definition
	}
}
