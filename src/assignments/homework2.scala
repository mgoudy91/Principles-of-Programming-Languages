
/*
 * CSCI 3155: Homework 2
 * Tyler Howarth
 * 
 * Partner: Mitch Goudy
 * Collaborators: Damien Burks, Office Hours
 */

/*
 * Replace the 'throw new UnsupportedOperationException' expression with
 * your code in each function, as well the 'false' expression in each
 * test case.
 */

object HomeworkTwo extends App {
  
	sealed abstract class Formula
	case class B(bool: Boolean) extends Formula
	case class Var(name: String) extends Formula
	case class And(left: Formula, right: Formula) extends Formula 
	case class Or(left: Formula, right: Formula) extends Formula
	case class Not(sub: Formula) extends Formula
	case class Forall(binding: String, body: Formula) extends Formula 
	case class Exists(binding: String, body: Formula) extends Formula
  
  
  
  /* A variable assignment represented as a map. */
  type Assignment = Map[String, Boolean]
  
  val empty: Assignment = Map()
  
  def lookup(a: Assignment, x: String): Option[Boolean] =
    a get x
  
  def bind(a: Assignment, x: String, b: Boolean): Assignment =
    a + (x -> b)
    
  /*** Exercises ***/
  
  /* A variable set represented as a list without duplicates. */
  type VarSet = List[String]
  
  val emptyVarSet: List[String] = Nil
  
  /*** Part A: Set of Variables ***/
  def removeVar(x: String, set: VarSet): VarSet = set match{
    case Nil => Nil
    case h :: t => if (h==x) t else h :: removeVar(x, t)
  }
  
  def testRemoveVar(removeVar: (String,VarSet) => VarSet): Boolean = {
    (removeVar("one", List("one", "two", "three")) == List("two", "three")) &&
    (removeVar("one", List("two", "three")) == List("two", "three")) &&
    (removeVar("one", List("one")) == Nil) &&
    (removeVar("one", List()) == Nil)
  }
  
  /* You can run your test in any number of ways, including using an assert expression. */
  assert(testRemoveVar(removeVar))
  
  def insertVar(x: String, set: VarSet): VarSet = set match{
    case Nil => x :: set
    case h :: t => if(h==x) set else h :: insertVar(x,t)
  }
  
  def testInsertVar(insertVar: (String,VarSet) => VarSet): Boolean = {
    (insertVar("one", List("one", "two", "three")) == List("one", "two", "three")) &&
    (insertVar("one", List("two", "three")) == List("one", "two", "three")) &&
    (insertVar("one", List("one")) == List("one")) &&
    (insertVar("one", List()) == List("one"))
  }
  assert(testInsertVar(insertVar))
  /*** End Part A ***/
  
  
  /*** Part B: Negation Normal Form ***/
  def toNNF(f: Formula): Formula = f match{
    case Not(g) => g match{
      case B(value) => B(!value)
      case Var(x) => Not(Var(x))
      case Not(Not(f)) => f
      case And(f1,f2) => Or(toNNF(Not(f1)), toNNF(Not(f2)))
      case Or(f1,f2) => And(toNNF(Not(f1)), toNNF(Not(f2)))
      case Forall(x,f) => Exists(x, toNNF(Not(f)))
      case Exists(x,f) => Forall(x, toNNF(Not(f)))
      case _ => f
    }
    case And(f1,f2) => And(toNNF(f1), toNNF(f2))
    case Or(f1,f2) => Or(toNNF(f1), toNNF(f2)) 
    case Forall(x,h) => Forall(x, toNNF(h)) 
    case Exists(x,h) => Exists(x, toNNF(h))
    case _ => f
  }
  
  def testToNNF(toNNF: Formula => Formula): Boolean = {
    (toNNF(Not(B(true))) == B(false)) &&
    (toNNF(Not(B(false))) == B(true)) &&
    (toNNF(Not(Not(Var("x")))) == Var("x")) &&
    (toNNF(Not(And(Var("x"), Var("y")))) == Or(Not(Var("x")), Not(Var("y")))) &&
    (toNNF(Not(Or(Var("x"),Var("y")))) == And(Not(Var("x")),Not(Var("y")))) &&
    (toNNF(Not(Forall("x",Var("f")))) == Exists("x",Not(Var("f")))) &&
    (toNNF(Not(Exists("x",Var("f")))) == Forall("x", Not(Var("f"))))
  }
  assert(testToNNF(toNNF))
  /*** End Part B ***/
  
  /*** Part C: Local Simplification ***/
  
  // part i
  def simplifyNot(f: Formula): Formula = f match{
    case Not(g) => g match {
      case B(value) => B(!value)
      case _ => f
    }
    case _ => f
  }
  
  def testSimplifyNot(simplifyNot: Formula => Formula): Boolean = {
	(simplifyNot(Not(B(true))) == B(false)) &&
	(simplifyNot(Not(B(false))) == B(true)) &&
	(simplifyNot(Not(Var("x"))) == Not(Var("x")))
  }
  assert(testSimplifyNot(simplifyNot))
  
  // part ii
  def simplifyAnd(f: Formula): Formula = f match {
    case And(B(true), f1) => f1
    case And(f1, B(true)) => f1
    case And(B(false), f1) => B(false)
    case And(f1, B(false)) => B(false)
    case _ => f
  }
  
  def testSimplifyAnd(simplifyAnd: Formula => Formula): Boolean = {
    (simplifyAnd(And(B(true), Var("x"))) == Var("x")) &&
    (simplifyAnd(And(Var("x"), B(true))) == Var("x")) &&
    (simplifyAnd(And(B(false), Var("x"))) == B(false)) &&
    (simplifyAnd(And(Var("x"), B(false))) == B(false))
  }
  assert(testSimplifyAnd(simplifyAnd))
  
  //part iii
  def simplifyOr(f: Formula): Formula = f match{
    case Or(B(true), f1) => B(true)
    case Or(f1, B(true)) => B(true)
    case Or(B(false), f1) => f1
    case Or(f1, B(false)) => f1
    case _ => f
  }
  
  def testSimplifyOr(simplifyOr: Formula => Formula): Boolean = {
	(simplifyOr(Or(B(true), Var("x"))) == Var("x")) &&
    (simplifyOr(Or(Var("x"), B(true))) == Var("x")) &&
    (simplifyOr(Or(B(false), Var("x"))) == Var("x")) &&
    (simplifyOr(Or(Var("x"), B(false))) == Var("x")) 
  }
  /*** End Part C ***/
  
  
  /*** Part D: Global Simplification ***/
  def simplifyM(f: Formula): Formula = f match{
    case Not(g) => simplifyNot(Not(simplifyM(g)))
    case And(f1,f2) => simplifyAnd(And(simplifyM(f1),simplifyM(f2)))
    case Or(f1,f2) => simplifyOr(Or(simplifyM(f1),simplifyM(f2)))
    case Forall(x,m) => Forall(x,simplifyM(m))
    case Exists(x,m) => Exists(x,simplifyM(m))
    case _ => f
  }
  
  //def testSimplifyM(simplifyM: Formula => Formula): Boolean = false
  /** End Part D **/
  
  
  /*** Part E: Global Simplification with a Visitor ***/
  
  // Part i
  def visit(v: Formula => Formula, f: Formula): Formula = f match{
    //do we need these?
  	case B(value) => v(B(value))
    case Var(value) => v(Var(value))
    // ?
    case And(f1,f2) => v(And(visit(v,f1),visit(v,f2)))
    case Or(f1,f2) => v(Or(visit(v,f1),visit(v,f2)))
    case Not(g) => v(Not(visit(v,g)))
    case Forall(x,m) => v(Forall(x, visit(v,m)))
    case Exists(x,m) => v(Exists(x, visit(v,m)))
    case _ => v(f)
  }
  
  //visit(Not, And(B(true),B(false)))
  
  //def testVisit(visit: ((Formula => Formula), Formula) => Formula): Boolean = false
  // Part ii
  def simplifyV(f: Formula): Formula = {
    def simplify_h(f:Formula): Formula = f match{
      case And(f1,f2) => simplifyAnd(f)
      case Or(f1,f2) => simplifyOr(f)
      case Not(f) => simplifyNot(f)
      case _ => f
    }
    visit(simplify_h, f)
  }
  
  def testSimplifyV(simplifyV: Formula => Formula): Boolean = {
    simplifyV(And(B(true), Var("x"))) == Var("x")
    simplifyV(Not(And(Not(Or(Var("x"),B(false))),Not(And(B(true), Or(B(false), Var("b"))))))) == And(Var("x"), Var("b"))
    simplifyV(And(Var("a"), Var("b"))) == And(Var("a"),Var("b"))
    simplifyV(Forall("x", Or(And(B(true), B(false)), Var("z")))) == Forall("x", Var("z"))
  }
  /*** End Part E ***/
  
  
  /*** Part F: Evaluation and Satisfiability ***/
  // part i
  def eval(a: Assignment, f: Formula): Boolean = {
    val b = simplifyV(f)
    b match {
      case(B(false)) => false
      case(B(true)) => true
      case(Var(s)) => a(s)
      case(And(f1,f2)) => eval(a,f1) && eval(a,f2)
      case(Or(f1,f2)) => eval(a,f1) || eval(a, f2)
      case(Not(f)) => !eval(a,f)
      case Forall(x,f) => eval(bind(a,x,true),f) && eval(bind(a,x,false),f)
      case Exists(x,f) => eval(bind(a,x,true),f) || eval(bind(a,x,false),f)
    }
  }

  val xAssign = bind(empty, "x", true)
  val yAssign = bind(xAssign, "y", false)
  
  
  eval(yAssign, And(Var("x"),Var("y")))
  
  
  def testEval(eval: (Assignment,Formula) => Boolean): Boolean = {
    val a = Map("x"-> true, "y"->false, "z"->false)
    val c = empty
    eval(a, And(Or(Var("x"), Var("y")), Var("z"))) == false
    eval(a, Forall("y", And(Var("y"), Var("z") ))) == false
    
  }
  /*
  def freeVars(f: Formula): VarSet = throw new UnsupportedOperationException
  
  def normalizeVarSet(set: VarSet): VarSet = set.sort({_ < _})
  
  def testFreeVars(freeVars: Formula => VarSet): Boolean = false
  
  /* To create a literal VarSet, you can write something like List("x", "y", "z")/
   * 
   * Because freeVars can return a VarSet that can be in any order, call
   * normalizeVarSet before comparing with a literal VarSet.  The function
   * normalizeVarSet simply sorts the list of variables using a library
   * method. You don't need to understand library call at this point, but
   * you are welcome to ask about it.
   * 
   *   List("x", "y") == normalizeVarSet(freeVars( ... ))
   */
  
  def sat(f: Formula): Option[Assignment] = throw new UnsupportedOperationException
  
  def testSat(sat: Formula => Option[Assignment], eval: (Assignment,Formula) => Boolean): Boolean = false
    */
}