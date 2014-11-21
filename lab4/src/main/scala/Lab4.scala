object Lab4 extends jsy.util.JsyApplication {
  import jsy.lab4.ast._
  import jsy.lab4.Parser
  
  /*
   * CSCI 3155: Lab 4
   * Olivia Abrant
   * 
   * Partner: Catherine Dewerd
   * Collaborators: Kahini Wadhawan, Alok Joshi, Michelle Soult, Erik Eakins, Jessica Lynch
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
  
  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => l
    case h1 :: (t1 @ (h2 :: _)) => if (h1 == h2) compressRec(t1) else h1 :: compressRec(t1)
    // where t1 is the name of the thing after the @ sign
    // and h2 onward is the tail of the list
  }
  
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]) {
    // Nil: List[A] means a list of Nils of type A
    // don't have to write "case" because there is only one case
    (h, acc) => acc match {
                            case Nil => acc
                            case a1 => if (a1 != h) h :: acc else acc
    }
    // start at Nil (i.e. the end of the list) and work backward until your acc is at your list header
    // if acc empty or does head of acc == h

    // if (!(acc.isEmpty) && acc.head == h) acc else h :: acc
  }
  
  def mapFirst[A](f: A => Option[A])(l: List[A]): List[A] = l match {
    case Nil => l
    case h :: t => f(h) match {
                                case Some(thing) => thing :: t
                                case None => h :: mapFirst(f)(t)
    }
  }
  
  /* Search Trees */
  
  sealed abstract class Tree {
    def insert(n: Int): Tree = this match {
      case Empty => Node(Empty, n, Empty)
      case Node(l, d, r) => if (n < d) Node(l insert n, d, r) else Node(l, d, r insert n)
    } 
    
    def foldLeft[A](z: A)(f: (A, Int) => A): A = {
      def loop(acc: A, t: Tree): A = t match {
        case Empty => acc
        case Node(l, d, r) => {
                                val temp = loop(acc, l)
                                val temp2 = f(temp, d)
                                loop(temp2, r)
                                // loop(f(loop(acc, l), d), r) - ugly!
                              }
      }
      loop(z, this)
    }
    
    def pretty: String = {
      def p(acc: String, t: Tree, indent: Int): String = t match {
        case Empty => acc
        case Node(l, d, r) =>
          val spacer = " " * indent
          p("%s%d%n".format(spacer, d) + p(acc, l, indent + 2), r, indent + 2)
      } 
      p("", this, 0)
    }
  }
  case object Empty extends Tree
  case class Node(l: Tree, d: Int, r: Tree) extends Tree
  
  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }
  
  def sum(t: Tree): Int = t.foldLeft(0){ (acc, d) => acc + d }
  
  // takes a tuple of Option Ints - None or Some(p)
  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = t.foldLeft((true, None: Option[Int])){
      case ((newBoolVal, None), element) => (newBoolVal && true, Some(element))
      case ((newBoolVal, Some(thing)), element) => if (thing < element) (newBoolVal && true, None: Option[Int]) else ((newBoolVal && false), None: Option[Int])
    }
    b
  }
  

  /* Type Inference */
  
  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }
  
  def typeInfer(env: Map[String,Typ], e: Expr): Typ = {
    // Some shortcuts for convenience
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typ(e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => env(x)
      case ConstDecl(x, e1, e2) => typeInfer(env + (x -> typ(e1)), e2)
      case Unary(Neg, e1) => typ(e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }

      case Unary(Not, e1) => typ(e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)      // where tgot is just some type, in this case typ(e1) because that's what
        // is being matched
      }

      case Binary(Plus, e1, e2) => typ(e1) match {
        case TString => if (typ(e2) == TString) TString else err(typ(e2), e2)
        case TNumber => if (typ(e2) == TNumber) TNumber else err(typ(e2), e2)
        case _ => err(typ(e1), e1)
      }

      case Binary(Minus|Times|Div, e1, e2) => typ(e1) match {
        case TNumber => if (typ(e2) == TNumber) TNumber else err(typ(e2), e2)
        case _ => err(typ(e1), e1)
      }
      
      case Binary(Eq|Ne, e1, e2) => {
        /*
         * if the expression has a function type => typeerror
         * Note: there is a helper function called hasFunctionType()
         * else => eval and make sure return type is of type bool, return true/false depending whether it is
        */
        if (hasFunctionTyp(typ(e1))) err(typ(e1), e1)
        else
        {
          if (hasFunctionTyp(typ(e2))) err(typ(e2), e2)
          else TBool
        }

        //case TFunction(_,_) => err(typ(e1), e1)
        //case _ => if (typ(e2) == TFunction(_,_)) err(typ(e2), e2) else TBool
      }
      
      case Binary(Lt|Le|Gt|Ge, e1, e2) => typ(e1) match {
        /*
         * (number, number) => bool
         * (string, string) => bool
         * _ => typeerror 
        */
        case TNumber => if (typ(e1) == TNumber) TBool else err(typ(e2), e2)
        case TString => if (typ(e2) == TString) TBool else err(typ(e2), e2)
        case _ => err(typ(e1), e1)
      }

      case Binary(And|Or, e1, e2) => typ(e1) match {
        case TBool => if (typ(e2) == TBool) TBool else err(typ(e2), e2)
        case _ => err(typ(e1), e1)
      }
        
      case Binary(Seq, e1, e2) => typ(e1); typ(e2)

      case If(e1, e2, e3) => typ(e1) match {
        case TBool => {
          if (typ(e2) == typ(e3))
            typ(e2)
          else
            err(typ(e2), e2)
        }
        case _ => err(typ(e1), e1)
      }

      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {      // env1 is the original env extended either with a function name or not
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)
            env + (f -> tprime)
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env1)((tempEnv, y) => y match {       // then env2 is env1 extended with the parameters
          case (currVar, currType) => tempEnv + (currVar -> currType)
          })
        // for example - list.foldLeft(z:B)(b:B, a:A => f(b,a)) : B
        // params takes in two arguments: an environment and a tuple - in this case, env1, which we're extending,
        // and the tuple of the accumulator and y, where y is itself a tuple of each function parameter (some variable
        // of some type)
        // match on y (which is each parameter) and for each current variable and current type extend the accumulator
        // with that mapping
        // at the end the accumulator should have the entire list of variables mapped to their respective types
        
        // Match on whether the return type is specified. Not the return type itself, but WHETHER IT IS SPECIFIED.
        // => tann match (where tann is the type annotation)
        val tau = typeInfer(env2, e1)
        tann match {
          case None => {      // in the case the function is not annotated
            val tauPrime = TFunction(params, tau)     // FROM THE JUDGEMENT FORMS: τ' = (x1 : τ1 ,..., xn : τn) ⇒ τ
            tauPrime
          }
          case Some(tret) => {
            val tauPrime = TFunction(params, tau)     // also the exact same thing as in the first extension ^
            if (tret == typeInfer(env2, e1)) tauPrime else err(typeInfer(env2, e1), e1)
          }
        }
      }

      case Call(e1, args) => typ(e1) match {
        case TFunction(params, tret) if (params.length == args.length) => {
          (params, args).zipped.foreach {
            (paramX, argsY) => (paramX, argsY) match {      // where params is itself a tuple
              case ((str, paramType), argType) => if (paramType != typ(argType)) err(paramType, argType) 
            }
            
          };
          tret
        }
        case tgot => err(tgot, e1)
      }

      case Obj(fields) => TObj(fields.map{case (a,b) => (a, typ(b))})
      // in the case that there is some (a, b) in fields, map it to the type         

      case GetField(e1, f) => typ(e1) match {
        case TObj(fields) => if (fields.contains(f)) fields(f) else err(TObj(fields), e1)
        case _ => err(typ(e1), e1)
      }
    }
  }
  
  
  /* Small-Step Interpreter */
  
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    ((v1, v2): @unchecked) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case (N(n1), N(n2)) =>
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }
  
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    
    def subst(e: Expr): Expr = substitute(e, v, x)
    
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (x == y) v else e
      case ConstDecl(y, e1, e2) => ConstDecl(y, subst(e1), if (x == y) e2 else subst(e2))
      case Function(p, params, tann, e1) =>
        if (params.exists((t1:(String, Typ)) => t1._1 == x) || p == Some(x))
          Function(p, params, tann, e1)
        else
          Function(p, params, tann, subst(e1))
      case Call(e1, args) => Call(subst(e1), args.map(v1 => subst(v1)))
      case Obj(fields) => Obj(fields.map(s => (s._1, subst(s._2))))
      case GetField(e1, f) => GetField(subst(e1), f)
    }
  }
  
  def step(e: Expr): Expr = {
    require(!isValue(e))
    
    def stepIfNotValue(e: Expr): Option[Expr] = if (isValue(e)) None else Some(step(e))
    
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      case Unary(Neg, N(n1)) => N(- n1)
      case Unary(Not, B(b1)) => B(! b1)
      case Binary(Seq, v1, e2) if isValue(v1) => e2
      case Binary(Plus, S(s1), S(s2)) => S(s1 + s2)
      case Binary(Plus, N(n1), N(n2)) => N(n1 + n2)
      case Binary(Minus, N(n1), N(n2)) => N(n1 - n2)
      case Binary(Times, N(n1), N(n2)) => N(n1 * n2) 
      case Binary(Div, N(n1), N(n2)) => N(n1 / n2)

      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => B(inequalityVal(bop, v1, v2))
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => B(v1 == v2)
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => B(v1 != v2)
      case Binary(And, B(b1), e2) => if (b1) e2 else B(false)
      case Binary(Or, B(b1), e2) => if (b1) B(true) else e2
      case ConstDecl(x, v1, e2) if isValue(v1) => substitute(e2, v1, x)
      
      case Call(v1, args) if isValue(v1) && (args forall isValue) =>
        v1 match {
          case Function(p, params, _, e1) => {
            val e1p = (params, args).zipped.foldRight(e1){
              (paramsX, acc) => paramsX match {
                case ((paramName, _), argVal) => substitute(acc, argVal, paramName)
              }
            }
            p match {
              case None => e1p
              case Some(x1) => substitute(e1p, v1, x1) 
            }
          }
          case _ => throw new StuckError(e)
        }
      /*** Fill-in more cases here. ***/

      case If(B(e1), e2, e3) => if (e1) e2 else e3

      case GetField(Obj(fields), value) => fields.get(value) match {
        case Some(v) => v
        case None => throw StuckError(e)
      }
        
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      case Unary(uop, e1) => Unary(uop, step(e1))
      case Binary(bop, v1, e2) if isValue(v1) => Binary(bop, v1, step(e2))
      case Binary(bop, e1, e2) => Binary(bop, step(e1), e2)
      case If(e1, e2, e3) => If(step(e1), e2, e3)
      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)
      /*** Fill-in more cases here. ***/

      case GetField(e1, f) => GetField(step(e1), f)
      case Obj(fields) => Obj(fields.map{case (a, b) => (a, step(b))})
      case Call(v1, listOfExpr) if isValue(v1) => Call(v1, mapFirst(stepIfNotValue)(listOfExpr))
      case Call(e1, listOfExprs) => Call(step(e1), listOfExprs)

      
      /* Everything else is a stuck error. Should not happen if e is well-typed. */
      case _ => throw StuckError(e)
    }
  }
  
  
  /* External Interfaces */
  
  this.debug = true // comment this out or set to false if you don't want print debugging information
  
  def inferType(e: Expr): Typ = {
    if (debug) {
      println("------------------------------------------------------------")
      println("Type checking: %s ...".format(e))
    } 
    val t = typeInfer(Map.empty, e)
    if (debug) {
      println("Type: " + pretty(t))
    }
    t
  }
  
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
  def iterateStep(s: String): Expr = iterateStep(Parser.parse(s))
  
  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }
    
    val expr =
      handle(None: Option[Expr]) {Some{
        Parser.parseFile(file)
      }} getOrElse {
        return
      }
    
    handle() {
      val t = inferType(expr)
    }
    
    handle() {
      val v1 = iterateStep(expr)
      println(pretty(v1))
    }
  }

}