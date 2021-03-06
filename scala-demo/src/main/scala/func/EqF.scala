package func

// Equivalence Function
trait EqF[Z, S <: Z, T <: Z] extends Function[S, T] { self =>
  // reflexive: there is an EqF that takes x => x for any x of type T.  this is the identity function, idEqF[T].
  // composing a function with its inverse should result in identity.  can we prove this?
  // TODO: demonstrate that (eqF compose eqF.invert) == idEqF
  
  // symmetric: for any EqF, its inverse exists -- along with a proof of their equivalence?
  def apply(s: S): T
  def unapply(t: T): S
  
  // forward and reverse are swapped during inversion
  def invert: EqF[Z, T, S] = 
    new EqF[Z, T, S] {
      def apply(t: T) = self.unapply(t)
      def unapply(s: S) = self.apply(s)
    }
  
  // transitive: EqFs are composable
  def compose[R <: Z](er: EqF[Z, R, S]): EqF[Z, R, T] = 
    new EqF[Z, R, T] {
      def apply(r: R) = self.apply(er.apply(r))
      def unapply(t: T) = er.unapply(self.unapply(t))
    }
  
}