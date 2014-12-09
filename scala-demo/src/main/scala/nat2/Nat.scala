package nat2

import algebra._
import func.EqF
import scala.reflect.runtime.universe._

/*
 * Using scala's compiler to check a proof of the associativity of addition
 * http://en.wikipedia.org/wiki/Proofs_involving_the_addition_of_natural_numbers#Proof_of_associativity
 */

	
object Nat extends App {
  val z = AZero
  def s(a: ANat) = ASucc(a)
  val one = s(z)
  val two = s(s(z))
  val three = s(s(s(z)))
  // val a = Add(Add(z, z), z)
  val a = Add(Add(one, one), one)
  println(a)
  println(Add.associateRight(a))
}
  
sealed trait ANat

sealed trait AZero extends ANat

object AZero extends AZero

case class ASucc[S <: ANat](n: S) extends ANat

case class Add[+S <: ANat, +T <: ANat](left: S, right: T) extends ANat

object Add {
  type AEqF[S <: ANat, T <: ANat] = EqF[ANat, S, T]
  
  // a => a + 0
  def addZero[A <: ANat] = new EqF[ANat, A, Add[A, AZero]] {
    def apply(a: A) = Add(a, AZero)
    def unapply(add: Add[A, AZero]) = { println("hey: " + add.right); add.left }
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
   * below are the steps of the associativity proof
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
  
  // use the steps of the associativity proof to re-associate from left hand to right hand
  // (a + b) + c = a + (b + c)
  def associateRight: AEqF[Add[Add[ANat, ANat], ANat], Add[ANat, Add[ANat, ANat]]] = new AEqF[Add[Add[ANat, ANat], ANat], Add[ANat, Add[ANat, ANat]]] {
    def apply(a: Add[Add[ANat, ANat], ANat]) = a match {
      case x: Add[Add[ANat, ANat], AZero] => { // proof is incomplete since these are unchecked due to type erasure
        println(typeOf[a.type])
        println(typeOf[AZero])
        println(typeOf[ASucc[ANat]])
        base.apply(x)
        } //  if typeOf[x.right.type] =:= typeOf[AZero]
      case x: Add[Add[ANat, ANat], ASucc[ANat]] => indy.apply(x) //  if typeOf[x.right.type] =:= typeOf[ASucc[ANat]]
    }
    def unapply(a: Add[ANat, Add[ANat, ANat]]) = a match {
      case x: Add[ANat, Add[ANat, AZero]] => base.unapply(x)
      case x: Add[ANat, Add[ANat, ASucc[ANat]]] => indy.unapply(x)
    }
  }
  
  // a + (b + c) = (a + b) + c
  def associateLeft = associateRight.invert
}

