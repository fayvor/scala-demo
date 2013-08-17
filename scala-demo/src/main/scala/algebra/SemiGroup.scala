package algebra

trait SemiGroup[S] {
  
    sealed trait Rep {
	  def flatten: Rep
	  def reduce: S
	}
    
    class BinOp(left: Rep, right: Rep)

}

abstract class BinaryOperator[S](left: S, right: S) {
  
  def reduce: S
}