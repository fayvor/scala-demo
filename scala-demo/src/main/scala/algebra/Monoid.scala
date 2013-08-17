package algebra

import func.EqF

trait Monoid[S] { // extends SemiGroup[S] {
  type Zero <: Rep
  
  sealed trait Rep {
    def flatten: Rep
    def reduce: S
  }
    
  abstract class BinOp(left: Rep, right: Rep) extends Rep
  
  // identity element, aka Zero
  val idElem: Zero
  
//  def applyIdRight: EqF[Rep] = new EqF[Rep] {
//    def apply(r: Rep) = new BinOp(r, idElem)
//  }
  
//  def unapplyIdLeft: EqF[Rep] = new EqF[Rep] {
//    def apply(b: BinOp) = b.right
//  }
//  
//  def idUniquenessLeft(z1: Zero): EqF[Rep] = unapplyIdLeft compose applyIdRight 
  
}