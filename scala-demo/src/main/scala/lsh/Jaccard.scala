package lsh

// infolab.stanford.edu/~ullman/mmds/ch3a.pdf
object Jaccard {
  // The Jaccard similarity of sets S and T is |S ∩ T| / |S ∪ T| 
  def similarity[T](s1: Set[T], s2: Set[T]) = {
    (s1 intersect s2 size) / (s1 union s2 size)
  }
  
  // define the Jaccard distance of sets by d ( x, y ) = 1 − SIM ( x, y ).
  def distance[T](s1: Set[T], s2: Set[T]) = 1 - similarity(s1, s2)
}