object Lab1 extends jsy.util.JsyApplication {
  import jsy.lab1.ast._
  import jsy.lab1.Parser
  
  /*
   * CSCI 3155: Lab 1
   * <Your Name>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /*
   * Example with a Unit Test
   * 
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter.
   * 
   * To run a selection in the interpreter in Eclipse, highlight the code of interest
   * and type Ctrl+Shift+X (on Windows) or Cmd+Shift+X (on Mac).
   * 
   * Highlight the next few lines below to try it out.  The assertion passes, so
   * it appears that nothing happens.  You can uncomment the "bad test specification"
   * and see that a failed assert throws an exception.
   * 
   * You can try calling 'plus' with some arguments, for example, plus(1,2).  You
   * should get a result something like 'res0: Int = 3'.
   * 
   * As an alternative, the testPlus2 function takes an argument that has the form
   * of a plus function, so we can try it with different implementations.  For example,
   * uncomment the "testPlus2(badplus)" line, and you will see an assertion failure.
   * 
   * Our convention is that these "test" functions are testing code that are not part
   * of the "production" code.
   * 
   * While writing such testing snippets are convenient, it is not ideal.  For example,
   * the 'testPlus1()' call is run whenever this object is loaded, so in practice,
   * it should probably be deleted for "release".  A more robust way to maintain
   * unit tests is in a separate file.  For us, we use the convention of writing
   * tests in a file called LabXSpec.scala (i.e., Lab1Spec.scala for Lab 1).
   */
  
  def plus(x: Int, y: Int): Int = x + y
  def testPlus1() {
    assert(plus(1,1) == 2)
    //assert(plus(1,1) == 3) // bad test specification
  }
  testPlus1()

  def badplus(x: Int, y: Int): Int = x - y
  def testPlus2(plus: (Int, Int) => Int) {
    assert(plus(1,1) == 2)
  }
  //testPlus2(badplus)

  /* Exercises */

  def abs(n: Double): Double =
  {
    if (n < 0 )     // test if n is less than 0; if so, return the negative of n
      (-n)
    else            // if n is greater than or equal to 0, just return n
      n

    /*
    alternate method:
    n match
    {
      case x if x < 0 => -x
      case x => x
    }
    */
  }

  def xor(a: Boolean, b: Boolean): Boolean =
  {
    // have to be opposite values to return true

    if (a)
      if (b) false else true
    else
      if (b) true else false

    /*
    alternate method:
    (a, b) match
    {
      case (true, true) => false
      case (true, false) => true
      case (false, true) => true
      case (false, false) => false
    }
    */
  }

  def repeat(s: String, n: Int): String = 
  {
    require (n >= 0)      // require makes sure the user is inputting a valid input
    n match
    {
      case 0 => ""    // test for whether n = 0
      case 1 => s     // may not even need this - base case is case 0 where n = 0
      case _ => s + repeat(s, n-1)
    }

    /*
    alternate method:
    if (n = 0)
      return ""
    else
      s + repeat(s, n-1)
    */
  }
  
  def sqrtStep(c: Double, xn: Double): Double =
  {
    (xn - ((Math.pow(xn, 2) - c) / (2*xn)))     // math function from handout. literally.
  }

  def sqrtN(c: Double, x0: Double, n: Int): Double =
  {
    require (c >= 0)    // can't find sqrt of negative number
    require (n >= 0)    // can't find less than 0 approximations
    def helper (c : Double, x : Double, n : Int): Double =
    // recommended to use a helper function in the handout, but theoretically, should just be able to keep overwriting val approx?
    {
      val approx = sqrtStep(c, x)     // set the approximation each time through helper to sqrtStep() of the new x
      n match 
      {
        case 0 => x       // if n = 0, meaning there are no approximation steps
        case 1 => approx    // return first step of approximation, which is what you do in function sqrtStep()
        case _ => helper(c, approx, n-1)    // use prior approximation of sqrt(c) in the recursive call of helper, recurse until n = 1
      }
    }
    helper(c, x0, n)      // call helper on the original c, the initial guess xO (given by the tester), and n (the number of times we should approximate)
  }
  
  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double =
  {
    require (epsilon > 0)
    def helper(c : Double, x : Double): Double =
    {
      val approx = sqrtStep(c, x)     // set the approximation each time through helper to sqrtStep of the new x
      val error = abs(Math.pow(approx, 2) - c)    // calculate the error with |x^2 - c| - squares the approximation and compares it to c
      if (error < epsilon) approx     // if the error is less than epsilon, it's what we want
      else helper(c, approx)      // otherwise recurse with the new approximation
      
      /*
      alternate method:
      error match       // match to error to test if error is less than epsilon
      {
        case error if error < epsilon => approx
        case _ => helper(c, approx)     // if it's not, recurse with the new approximation
      }
      */

    }
    helper(c, x0)
  }

  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }
  
  /* Search Tree */
  
  sealed abstract class SearchTree
  case object Empty extends SearchTree
  case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree
  
  def repOk(t: SearchTree): Boolean = 
  {
    def check(t: SearchTree, min: Int, max: Int): Boolean = 
    {
      t match     // matches the instance of IntTree, t 
      {
        case Empty => true      // if t is empty, it's automatically valid
        case Node(l, d, r) => (d >= min) && (d < max) && check(l, min, d) && check(r, d, max)     
        // tests if 
        // 1) data at node is greater than or equal to the min
        // 2) data at node is less than the max 
        // 3) whether the node to the left is a valid node, with the left node as the new tree, the min as the min, and the data at 
          // the current node as the new max; since we're moving left, everything should be smaller than the current node 
        // 4) whether the node to the right is a valid node, with the right node as the new tree, the data at the current node as the 
          // new min, and the max as the max; since we're moving right, everything should be bigger than or equal to the current node
        // ALL 4 CASES MUST BE TRUE TO RETURN TRUE
      }
    /*
    alternate method:
    if (t == Empty) true      // valid syntax?
    else
      if (d >= min) && (d < max) && check(l, min, d) && check(r, d, max) true
      else false
    */

    }
    check(t, Int.MinValue, Int.MaxValue)      // call check with the absolute min and absolute max of Int
  }
  
  def insert(t: SearchTree, n: Int): SearchTree =     // want to return a new tree with the new node n
  {
    t match
    {
      case Empty => Node(Empty, n, Empty)     // if t is empty or you've hit the end, then add n with no left or right nodes
      case Node(l, d, r) => if (n >= d) Node(l, d, insert(r, n)) else Node(insert(l, n), d, r)    
      // otherwise, recurse through t and compare n to the data at each node - if it's greater then call insert() again on the right child
      // if it's less then call insert() again on the left child
      // have to call insert() inside Node() because we can't change vals, so we have to return nodes pointing to nodes pointing to nodes
      // and creating a new tree instead of just adjusting pointers
    }
    /*
    alternate method:
    if (t == Empty) Node(Empty, n, Empty)
    else
      if (n >= d) Node(, d, insert(r, n))
      else Node(insert(l, n), d, r)
    */
  }
  
  def deleteMin(t: SearchTree): (SearchTree, Int) =     // want to return a tuple of the new tree and the node you delete
  {
      // WHAT IF THE TREE ISN'T A VALID SERACH TREE???
      // Shouldn't we call repOk() first to make sure the nodes are all in the right places?
      // this comes into play with the base case, since if the tree isn't valid the last node on the left may not be the min

    require(t != Empty)     // because can't delete anything from an empty tree
    (t: @unchecked) match     // tells the compiler to ignore the fact that t is not exhaustive (i.e. t could be empty, but we don't care
                              // because we required t != Empty above)
    {
      case Node(Empty, d, r) => (r, d)    // if the left node is empty, then the node you're at is the min, so delete it and return the value 
                                          // of the node to the right and the current node (so that the new tree we create is still connected)
      case Node(l, d, r) =>       // otherwise, the left is not empty
        val (l1, m) = deleteMin(l)      // set the tuple of the new left value (which is the right node of the node we just deleted)
        (Node(l1, d, r), m)
    }
    /*
    alternate method:
    if Node(Empty, d, r) (r, d)
    else
    {
      val (l1, m) = deleteMin(l)
      (Node(l1, d, r), m)
    }
    */
  }
 
  def delete(t: SearchTree, n: Int): SearchTree =     // removes the first node it finds with data value equal to n
  {
    (t: @unchecked) match
    {
      case Empty => Empty       // return empty since there is no node
      case Node(Empty, d, Empty) => if (d == n) Empty else t      // only one node or finished recursing
                                                                  // if the data equals what we want, remove it and return empty tree/rest of the tree
      case Node(l, d, r) => if (n < d) Node(delete(l, n), d, r)   // if n < d 
                  else if (n == d) 
                  {
                    val (l1, m) = deleteMin(r)
                    Node(l, m, l1)
                  }
                  else Node(l, d, delete(r, n))
    }
  }
  
  /* JavaScripty */
  
  def eval(e: Expr): Double = e match {
    case N(n) => n
    case Unary(uop, e1) => uop match {
                        case Neg => (-eval(e1)) 
                        case _ => throw new UnsupportedOperationException
                     }
    case Binary(bop, e1, e2) => bop match {
                          case Plus => (eval(e1) + eval(e2))
                          case Minus => (eval(e1) - eval(e2))
                          case Times => (eval(e1) * eval(e2))
                          case Div => (eval(e1) / eval(e2))
                          case _ => throw new UnsupportedOperationException
                        }
  }
  
 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(Parser.parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */ 
  
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = Parser.parseFile(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(v)
  }

}
