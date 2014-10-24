object Lab3 extends jsy.util.JsyApplication {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * Olivia Abrant
   * 
   * Partner: Catherine Dewerd
   * Collaborators: Jessica Lynch, Max Harris, Michelle Soult, Erik Eakins, Alok Joshi
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace 'YourIdentiKey' in the object name above with your IdentiKey.
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
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(false) => 0
      case B(true) => 1
      case Undefined => Double.NaN
      case S(s) => try s.toDouble catch { case _: Throwable => Double.NaN }
      case Function(_, _, _) => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) if (n compare 0.0) == 0 || (n compare -0.0) == 0 || n.isNaN => false
      case N(_) => true
      case B(b) => b
      case Undefined => false
      case S("") => false
      case S(_) => true
      case Function(_, _, _) => true
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => if (n.isWhole) "%.0f" format n else n.toString
      case B(b) => b.toString
      case Undefined => "undefined"
      case S(s) => s
      case Function(_, _, _) => "function"
    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
  require(isValue(v1))
  require(isValue(v2))
  require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case _ =>
        val (n1, n2) = (toNumber(v1), toNumber(v2))
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }
  
  // write equalityVal
  def equalityVal(e: Expr, bop: Bop, v1: Expr, v2: Expr): Boolean = {
  require(isValue(v1))
  require(isValue(v2))
  //require(bop == Eq || bop == Ne)
    (v1, v2) match {
    case (Function(_, _, _), _) => throw DynamicTypeError(e)    // entire original expression passed in was a problem, so return full expression (e)
    case (_, Function(_, _, _)) => throw DynamicTypeError(e)    // same thing
    case _ =>
        (bop: @unchecked) match {
        case Eq => v1 == v2
        case Ne => v1 != v2
        }
    }
  }

  /* Big-Step Interpreter with Dynamic Scoping */
  
  /*
   * This code is a reference implementation of JavaScripty without
   * strings and functions (i.e., Lab 2).  You are to welcome to
   * replace it with your code from Lab 2.
   */
  def eval(env: Env, e: Expr): Expr = {
    
    def eToN(e: Expr): Double = toNumber(eval(env, e))
    def eToB(e: Expr): Boolean = toBoolean(eval(env, e))
    def eToVal(e: Expr): Expr = eval(env, e)
    
    e match {
      /* Base Cases */
      case _ if isValue(e) => e
      case Var(x) => get(env, x)
      
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      
      case Unary(Neg, e1) => N(- eToN(e1))
      case Unary(Not, e1) => B(! eToB(e1))
      
      case Binary(Plus, e1, e2) => (eToVal(e1), eToVal(e2)) match {
        case (S(s1), v2) => S(s1 + toStr(v2))
        case (v1, S(s2)) => S(toStr(v1) + s2)
        case (v1, v2) => N(toNumber(v1) + toNumber(v2))
      }      
      case Binary(Minus, e1, e2) => N(eToN(e1) - eToN(e2))
      case Binary(Times, e1, e2) => N(eToN(e1) * eToN(e2))
      case Binary(Div, e1, e2) => N(eToN(e1) / eToN(e2))
      
      case Binary(bop @ (Eq | Ne), e1, e2) => {
        println("inside Eq or Ne")
        B(equalityVal(e, bop, eToVal(e1), eToVal(e2)))
      }
      case Binary(bop @ (Lt|Le|Gt|Ge), e1, e2) => B(inequalityVal(bop, eToVal(e1), eToVal(e2)))
      
      case Binary(And, e1, e2) => 
        val v1 = eToVal(e1)
        if (toBoolean(v1)) eToVal(e2) else v1
      case Binary(Or, e1, e2) =>
        val v1 = eToVal(e1)
        if (toBoolean(v1)) v1 else eToVal(e2)
      
      case Binary(Seq, e1, e2) => eToVal(e1); eToVal(e2)
      
      case If(e1, e2, e3) => if (eToB(e1)) eToVal(e2) else eToVal(e3)
      
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eToVal(e1)), e2)
      
      case Call(e1, e2) => eToVal(e1) match {
        // where s1 in Function is e' in the judgement form - body of function
        // Function evaluates using the value of e2 as its parameter
        // case class Function(p: Option[String], x: String, e1: Expr) extends Expr
      // def extend(env: Env, x: String, v: Expr): Env =
        case Function(p, x, s1) => p match {    // p is of type Option, testing for whether the function name exists - refer to Function definition
            case None => {
              println("gets to case None")
              eval(extend(env, x, eToVal(e2)), s1)    // evaluate body of function with parameter set to the thing being passed in, non-recursive functions
            }
            case Some(p1) => eval(extend(extend(env, p1, eToVal(e1)), x, eToVal(e2)), s1)
          // p1 is of type string - name of the function
          // if the function has a name, extend the environment with the extended
          // environment of the name being set to e1, and the argument x being set
          // to e2
        }
        case _ => throw new DynamicTypeError(e)   // parameter e passed into eval()
      }
      
      case _ => throw new UnsupportedOperationException
    }
  }
    

  /* Small-Step Interpreter with Static Scoping */
  
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    /* Simple helper that calls substitute on an expression
     * with the input value v and variable name x in function body. */
    //if (debug)
    //  println("Substituting " + v + "for " + x + "in " + e)

    def subst(e: Expr): Expr = substitute(e, v, x)
    /* Body */
    e match {
      case N(_) | B(_) | S(_) | Undefined => e
      case Print(e1) => Print(subst(e1))
      case Var(x1) => if(x1 == x) v else Var(x1)

      case Function(name, x1, fbody) => name match {
        case None => if (x1 == x) e else Function(None, x1, subst(fbody))
        case Some(f) => if (f == x) e else if (x1 == x) e else Function(name, x1, subst(fbody))
      }
      // why does this work?

      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case ConstDecl(x1, e1, e2) => ConstDecl(x1, subst(e1), if (x1 == x) e2 else subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Call(e1, e2) => Call(subst(e1), subst(e2))
      
      case _ => throw new UnsupportedOperationException
    }
  }
    
  def step(e: Expr): Expr = {
    println(e)
    e match {
      /* Base Cases: Do Rules */
            
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      
        // ****** Your cases here
      // isValue(v) makes sure that expression is a value - base case
      case Unary(Neg, v1) if isValue(v1) => N(-toNumber(v1))
      
      case Unary(Not, v1) if isValue(v1) => B(!toBoolean(v1))
      
      case Binary(Seq, v1, e2) if isValue(v1) => e2
      
      case Binary(Plus, v1, v2) if isValue(v1) && isValue(v2) => {
        (v1, v2) match {
          case (S(v1), _) => S(v1 + toStr(v2))
          case (_, S(v2)) => S(toStr(v1) + v2)
          case (_,_) => N(toNumber(v1) + toNumber(v2))
        }
      }
      
      case Binary(Minus, v1, v2) if isValue(v1) && isValue(v2) => N(toNumber(v1) - toNumber(v2))
      case Binary(Times, v1, v2) if isValue(v1) && isValue(v2) => N(toNumber(v1) * toNumber(v2))
      case Binary(Div, v1, v2) if isValue(v1) && isValue(v2) => N(toNumber(v1) / toNumber(v2))
      
      case Binary(bop @ (Lt|Le|Gt|Gt), v1, v2) if isValue(v1) && isValue(v2) => B(inequalityVal(bop, v1, v2))   
      // because inequalityVal() takes care of the case where they are both strings and compares the strings, otherwise converts to numbers and compares
      
      case Binary(bop @ (Eq|Ne), v1, v2) if isValue(v1) && isValue(v2) => B(equalityVal(e, bop, v1, v2))
      // equalityVal() takes care of the case where either v1 or v2 is a Function, in which case it throws a DynamicTypeError, otherwise compares
      
      case Binary(And, v1, e2) if isValue(v1) => if(toBoolean(v1)) e2 else v1
      case Binary(Or, v1, e2) if isValue(v1) => if(toBoolean(v1)) v1 else e2
      
      case If(v1, e2, e3) if isValue(v1) => if(toBoolean(v1)) e2 else e3
      
      case ConstDecl(x, v1, e1) if isValue(v1) => {
        println("Inside ConstDecl")
        substitute(e1, v1, x)
      }
      
      /*
      case Call(e1, e2) => eToVal(e1) match {
        // where s1 in Function is e' in the judgement form - body of function
        // Function evaluates using the value of e2 as its parameter
        // case class Function(p: Option[String], x: String, e1: Expr) extends Expr
      // def extend(env: Env, x: String, v: Expr): Env =
        case Function(p, x, s1) => p match {    // p is of type Option, testing for whether the function name exists - refer to Function definition
            case None => {
              println("gets to case None")
              eval(extend(env, x, eToVal(e2)), s1)    // evaluate body of function with parameter set to the thing being passed in, non-recursive functions
            }
            case Some(p1) => eval(extend(extend(env, p1, eToVal(e1)), x, eToVal(e2)), s1)
          // p1 is of type string - name of the function
          // if the function has a name, extend the environment with the extended
          // environment of the name being set to e1, and the argument x being set
          // to e2
        }
        case _ => throw new DynamicTypeError(e)   // parameter e passed into eval()
      }
      */
      
      case Call(v1, v2) if isValue(v1) && isValue(v2) => {
        println("Hits base case Call")
        v1 match {
        case Function(None, x1, fbody) => substitute(fbody, v2, x1)
        case Function(Some(name), x2, fbody) => substitute(substitute(fbody, v1, name), v2, x2)
        
        case _ => throw DynamicTypeError(e)
        }
      }
      
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      
        // ****** Your cases here
      case Unary(uop, e1) => Unary(uop, step(e1))
        
      case Binary(bop, e1, e2) => e1 match {
        case N(_) | B(_) | S(_) | Function(_,_,_) | Undefined => Binary(bop, e1, step(e2))
        case _ => Binary(bop, step(e1), e2)
      }
        
      case If(e1, e2, e3) => If(step(e1), e2, e3)
          
      case ConstDecl(x1, e1, e2) => ConstDecl(x1, step(e1), e2)
        
      case Call(e1, e2) => {
        println("Hits inductive Call")
        (e1, e2) match {
        case (Function(_,_,_), s2) => Call(e1, step(s2))
        case (s1, _) => Call(step(s1), e2)
        }
      }
      
      // what the heck is this doing???
      
      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }
  

  /* External Interfaces */
  
  this.debug = true // comment this out or set to false if you don't want print debugging information
  
  // Interface to run your big-step interpreter starting from an empty environment and print out
  // the test input if debugging.
  def evaluate(e: Expr): Expr = {
    require(closed(e))
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with eval ...")
      println("\nExpression:\n  " + e)
    }
    val v = eval(emp, e)
    if (debug) {
      println("Value: " + v)
    }
    v
  } 
  
  // Convenience to pass in a jsy expression as a string.
  def evaluate(s: String): Expr = evaluate(jsy.lab3.Parser.parse(s))
   
  // Interface to run your small-step interpreter and print out the steps of evaluation if debugging. 
  def iterateStep(e: Expr): Expr = {
    require(closed(e))
    def loop(e: Expr, n: Int): Expr = {
      if (debug) { println("Step %s: %s".format(n, e)) }
      if (isValue(e)) e else loop(step(e), n + 1)
    }
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val v = loop(e, 0)
    if (debug) {
      println("Value: " + v)
    }
    v
  }

  // Convenience to pass in a jsy expression as a string.
  def iterateStep(s: String): Expr = iterateStep(jsy.lab3.Parser.parse(s))
  
  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }
    
    val expr =
      handle(None: Option[Expr]) {Some{
        jsy.lab3.Parser.parseFile(file)
      }} getOrElse {
        return
      }
    
    handle() {
      val v = evaluate(expr)
      println(pretty(v))
    }
    
    handle() {
      val v1 = iterateStep(expr)
      println(pretty(v1))
    }
  }
    
}
