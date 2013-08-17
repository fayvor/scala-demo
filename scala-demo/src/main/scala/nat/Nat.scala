package nat

sealed trait Nat {
  def toInt: Int
  def reduce: Rnat
}

sealed trait Rnat extends Nat // Reduced Nat

object Nat {
  def fromInt(i: Int): Rnat = i match {
    case 0 => Zero
    case n => Sred(fromInt(n - 1))
  }
}

class Succ(n: Nat)

case class S(n: Nat) extends Nat {
  def toInt = n.toInt + 1
  def reduce = Sred(n.reduce)
}

case class Sred(n: Rnat) extends Rnat {
  def toInt = n.toInt + 1
  def reduce = this
}

object Zero extends Rnat {
  def toInt = 0
  def reduce = this
}

case class Add(n1: Nat, n2: Nat) extends Nat {
  def reduce: Rnat = n2 match {
    case Zero => n1.reduce
    case S(pn2) => Sred(Add(n1, pn2).reduce) // A2: S(a+b) <= a + S(b)
    case Sred(pn2) => Sred(Add(n1, pn2).reduce) // A2: S(a+b) <= a + S(b)
    case n: Add => Add(n1, n.reduce).reduce
  }

  def toInt = this.reduce.toInt
}

object Add {
  // val
  //def associateLeft
  val yo = Add(Zero, Zero)
  // val fu = AddEq(Zero, Zero)
}

abstract class Eq[T](a: T, b: T)

// Equivalence Function
trait EqF[T] extends Function[T, T] { self =>
  // reflexive: there is an EqF that takes x => x for any x of type T.  this is the identity function, idEqF[T].
  // composing a function with its inverse should result in identity.  can we prove this?
  // TODO: demonstrate that (eqF compose eqF.invert) == idEqF
  
  // symmetric: for any EqF, its inverse exists -- along with a proof of their equivalence?
  def apply(u: T): T
  def unapply(v: T): T
  
  // forward and reverse are swapped during inversion
  def invert: EqF[T] = 
    new EqF[T] {
      def apply(v: T) = self.apply(v)
      def unapply(u: T) = self.unapply(u)
    }
  
  // transitive: EqFs are composable
  def compose(er: EqF[T]) = 
    new EqF[T] {
      def apply(s: T) = self.apply(er.apply(s))
      def unapply(v: T) = er.unapply(self.unapply(v))
    }
  
}

case class AddEq private(n1: Nat, n2: Nat) extends Eq[Nat](n1, n2)
object AddEq {
  def create(n: Nat, f: EqF[Nat]) = AddEq(n, f(n))
}

object AddZero extends EqF[Nat] {
  def apply(n: Nat): Nat = Add(n, Zero)
  def unapply(a: Nat): Nat = a match {
    case Add(n, Zero) => n
    case x => x
  }
}

object SuccAdd extends EqF[Nat] {
  def apply(n: Nat): Nat = n match {
    case S(Add(n1, n2)) => Add(n1, S(n2))
    case x => x
  }
  def unapply(a: Nat): Nat = a match {
    case Add(n1, S(n2)) => S(Add(n1, n2))
    case x => x
  }
}

//object AssocAdd extends EqF[Nat] {
//  def apply(n: Nat): Nat = n match {
//    // base case: (a + b) + 0 = a + b = a + (b + 0)
//    case Add(Add(a, b), Zero) => AddZero.unapply(n)
//  }
//}

// a + b = a + (b = (b + 0)) = a + (b + 0)

