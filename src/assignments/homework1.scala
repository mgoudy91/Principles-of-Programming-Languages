/*
 * CSCI 3155: Homework 1
 * Tyler Howarth	
 * 
 * Partner: Mitch Goudy
 * Collaborators: <Any Collaborators>
 */

/*
 * Replace the 'throw new UnsupportedOperationException' expression with
 * your code in each function, as well the 'false' expression in each
 * test case.
 */

object Homework1 extends App {

  /* Exercise 3a */

  
  def abs(n: Int): Int = if(n<0) -n else n
  
  val testAbs = abs(-1) == 1

  /* Exercise 3b */

  def xor(a: Boolean, b: Boolean): Boolean = {
   if(a==true)
      if(b==true) false else true
      else if(b==true) true else false
  }

  val testXor = xor(true,false) == true

  /* Exercise 3c */

  def isDivisibleByThree(four: Boolean, two: Boolean, one: Boolean): Boolean = {
    ((four&&two) && (!one)) || ((two&&one) && (!four)) || ((!four && !two && !one))
    
  }

  val testIsDivisibleByThree = isDivisibleByThree(false, false, false) == true

  /* Exercise 4a */

  def concatn(s: String, n: Int): String = {
    if(n==1 || n<1)
    	s
    	else
    	  s + concatn(s,n-1)
  }
  
  val testConcatn = concatn("foo",-12)

  /* Exercise 4b */

  def cosrectangles(a: Double, b: Double, n: Int): Double = {
    val width = (b-a)/n
    val area = Math.cos(a)*width
    if(n == 1)
    	area
    	else
    	  area + cosrectangles(a+width,b,n-1)
  }

  val testcosrectangles = cosrectangles(0, Math.Pi/2, 2000)
}