object Lab2 extends jsy.util.JsyApplication {
  import jsy.lab2.Parser
  import jsy.lab2.ast._
  
  /*
   * CSCI 3155: Lab 2
   */

  /*
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
  
  /* We represent a variable environment is as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */
  
  type Env = Map[String, Expr]    // map from variables to values, basically all your bound variables
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {     // extends the environment to include the new map stuff, new variables and such
    require(isValue(v))
    env + (x -> v)  
    
    /* Env is a Map => syntax key -> value OR (key, value)
     * therefore x is the key and v is some value associated with it
     * extend(env, x, v) takes the env and adds it to the value associated with key x?
     */
  }
  
  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(true) => 1
      case B(false) => 0
      case S(s) => try s.toDouble catch{case _:Throwable => Double.NaN}
      case Undefined => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case null => false
      case Undefined => false
      case N(0) => false
      //case N(Double.NaN) => false
      case N(n) => if n.isNaN() => false
      case N(_) => true
      case S("") => false // empty string is false
      case S(_) => true // everything else is true
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) if n.isWhole() => val x = n.toInt; x.toString     // convert number to string
      case N(n) => n.toString
      case S(s) => s
      case N(n) => n.toString
      //case B(true) => "true"
      //case B(false) => "false"
      case B(b) => b.toString
      case S(s) => s
      case Undefined => "undefined"
    }
  }
  
  def eval(env: Env, e: Expr): Expr = {
    /* Some helper functions for convenience. */
    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      /* Base Cases */
      case _ if isValue(e) => e // if e itself is already a value
      case Undefined => Undefined
      
      /* Inductive Cases */
      case Print(e1) => println(pretty(eToVal(e1))); Undefined
      
      case Var(x) => get(env, x) // get returns env(x); env is basically a map from strings to expressions 

     /*
      * ConstDecl should extend the environment with the first expression results bound to the 
      * identifier (WHAT DOES THIS MEAN?), and then eval the second expression WITH THE NEW ENVIRONMENT
      * extend declared above: env + (x -> v)
      */
      
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eval(env, e1)), e2)
      
      
      // Unary: (uop, e1)
      case Unary(Neg, e1) => N(-toNumber(eval(env, e1)))
      
      case Unary(Not, e1) => B(!toBoolean(eval(env, e1)))

      case Unary(_, e1) => throw new UnsupportedOperationException
      
      // Binary: (bop, e1, e2)
      case Binary(Plus, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (N(e1), N(e2)) => N(e1 + e2)
        case (S(e1), S(e2)) => S(e1 + e2) // need to handle string cases for autograder
        case (_, S(e2)) => S(toStr(e1) + e2) 
        case (S(e1), _) => S(e1 + toStr(e2))
        case (_, _) => N(toNumber(eval(env,e1)) + toNumber(eval(env, e2))) // original case 
      }

      case Binary(Minus, e1, e2) => N(toNumber(eval(env, e1)) - toNumber(eval(env, e2)))
      
      case Binary(Times, e1, e2) => N(toNumber(eval(env, e1)) * toNumber(eval(env, e2)))

      case Binary(Div, e1, e2) => N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)))
      
      case Binary(Eq, e1, e2) => B(eval(env, e1) == eval(env, e2))

      case Binary(Ne, e1, e2) => B(eval(env, e1) != eval(env, e2))

      case Binary(Lt, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (N(e1), N(e2)) => B(e1 < e2)
        case (S(e1), S(e2)) => B(e1 < e2)
        case (N(e1), _) => B(e1 < toNumber(eval(env, e2)))
        case (_, N(e2)) => B(toNumber(eval(env, e1)) < e2) //need to handle this for test
        case (_, _) => B(toNumber(eval(env, e1)) < toNumber(eval(env, e2)))
      }

      case Binary(Le, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (N(e1), N(e2)) => B(e1 <= e2)
        case (S(e1), S(e2)) => B(e1 <= e2)
        case (N(e1), _) => B(e1 <= toNumber(eval(env, e2)))
        case (_, N(e2)) => B(toNumber(eval(env, e1)) <= e2)
        case (_,_) => B(toNumber(eval(env, e1)) <= toNumber(eval(env, e2)))
      }

      case Binary(Gt, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (N(e1), N(e2)) => B(e1 > e2)
        case (S(e1), S(e2)) => B(e1 > e2)
        case (N(e1), _) => B(e1 > toNumber(eval(env, e2)))
        case (_, N(e2)) => B(toNumber(eval(env, e1)) > e2)
        case (_, _) => B(toNumber(eval(env, e1)) > toNumber(eval(env, e2))) //must compare numbers
      }

      case Binary(Ge, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (N(e1), N(e2)) => B(e1 >= e2)
        case (S(e1), S(e2)) => B(e1 >= e2)
        case (N(e1), _) => B(e1 >= toNumber(eval(env, e2)))
        case (_, N(e2)) => B(toNumber(eval(env, e1)) >= e2)
        case (_,_) => B(toNumber(eval(env, e1)) >= toNumber(eval(env, e2)))
      }

      /*
       * And and Or return either the first value that makes the expression false
       * or the last value that makes the expression true
       * as said by Erik Eakins
       */
      
      case Binary(And, e1, e2) => if (toBoolean(eval(env, e1)) == false) eval(env, e1) else (eval(env, e2))

      case Binary(Or, e1, e2) => if (toBoolean(eval(env, e1)) == true) eval(env, e1) else (eval(env, e2))

      /*
       * the comma operator evaluates each operand from left to right, discards
       * the results, then returns the value (and type?) of the last operand
       * http://en.wikipedia.org/wiki/Comma_operator
       */
      
      case Binary(Seq, e1, e2) => eval(env, e1); eval(env, e2)

      // e1 ? e2 : e3

      case If(e1, e2, e3) => if(toBoolean(eval(env, e1))) eval(env, e2) else eval(env, e3)
 
      case _ => throw new UnsupportedOperationException
    }
  }
    
  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = {
    /*debugging */
    /*
    println("Expression start")
    println(e)
    println("Expression end")
    */
    eval(emp, e)
  }

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Expr = eval(Parser.parse(s))

 /* Interface to run your interpreter from the command-line.  You can ignore what's below. */ 
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = Parser.parseFile(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(pretty(v))
  }

}