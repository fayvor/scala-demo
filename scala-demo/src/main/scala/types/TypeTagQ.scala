package types
import scala.reflect.runtime.universe._
import scala.reflect._

//object TypeErasureQ {
//
//  def fu(i: Tuple2[Int, Int]) = i._1
//
//  def ga(s: Tuple2[String, String]) = s._2
//
//  def zi(v: Tuple2[Any, Any]) = v match {
//    case x: Tuple2[Int, Int] => fu(x)
//    case x: Tuple2[String, String] => ga(x)
//  }
//}

//object TypeTagQ {
//
//  def fu(i: Tuple2[Int, Int]) = i._1
//
//  def ga(s: Tuple2[String, String]) = s._2
//
//  def zi[T: ClassTag](v: Tuple2[T, T])(implicit tag: TypeTag[T]) = tag.tpe match {
//    case t if t =:= typeOf[Int] => fu(v).toString
//    case t if t =:= typeOf[String] => ga(v)
//  }
//}