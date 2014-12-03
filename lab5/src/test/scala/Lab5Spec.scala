import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class Lab5Spec extends FlatSpec {
  
  "mapFirstDoWith" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val l2 = List()
     val gold1 = List(1, 2, 3, 4, -5)
     
     def dowith[W]: DoWith[W,List[Int]] = mapFirstWith[W,Int] { (i: Int) => if (i < 0) Some(doreturn(-i)) else None } (l1)
     def dowith1[W]: DoWith[W,List[Int]] = mapFirstWith[W,Int] { (i: Int) => if (i < 0) Some(doreturn(-i)) else None } (l2)

     assertResult((true,l2)) { dowith1(true) }
     assertResult((true,gold1)) { dowith(true) }
     assertResult((42,gold1)) { dowith(42) }
  }

  // Probably want to write some tests for castOk, typeInfer, substitute, and step.
}
