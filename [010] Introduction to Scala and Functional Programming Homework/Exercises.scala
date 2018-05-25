// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1: Vlad Paul Cosma - vlco@itu.dk
// AUTHOR2: Irina Alina Gabriela Luca - irlu@itu.dk
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// The extension of App allows writing statements at class top level (the so
// called default constructor). For App objects they will be executed as if they
// were placed in the main method in Java.

object Exercises extends App {

  // Exercise 3
  def power (x: Double, n: Int) : Double = {
    if(n == 0) 1 
      else if(n%2 == 0 && n > 0) power(x, n/2) * power(x, n/2) // this is not in tail position (2)
      else if(n%2 != 0 && n > 0) x * power(x, n-1) // this is not in tail position
      else 1/power(x, -n) // this is not in tail position
  } 
  // *** A very large value for n that is a positive even number could make the stack depth large.
  // *** Yes, especially for the second case(2).
  

  // A few tests, uncomment when your implementation is ready.

  assert (power (2.0, 2) == 4.0)
  assert (power (1.0, 42) == 1.0)
  //
  // The above assertions should pass when you call "scala Exercises".
  //
  // The following one should fail. Uncomment to check that assert works as
  // expected:
  //
  // assert (power (1.0, 42) == 2.0)

  // add 2-3 more tests:
  assert (power (4.0, -1) == 0.25)
  assert (power (5.0, 3) == 125)
  //
  // ...

  // Exercise 4
  def fib (n: Int) : Int = {    
    @annotation.tailrec
    def fibtail(n :Int, acc1:Int, acc2:Int): Int = n match {
      case _ if(n < 2) => acc2
      case _ => fibtail(n-1,acc2,acc1+acc2)
    }
    fibtail(n,0,1)
  }
  

  // assert (fib (7) == 8)
  assert (fib (6) == 8)
  assert (fib (1) == 1)
  assert (fib (0) == 1)

  
  // Exercise 5

  // A simple object describing a cost line; implemented imperatively, Java
  // style (this way until we learn more Scala)
  class Expense {

    // A constructor definition
    def this (tag :String, price :Int) = {
      this()
      this.tag = tag
      this.price = price
    }

    var tag   :String = "" // a tag line in the accounting system
    var price :Int    = 0 // the price is in cents
  }

  // computes the total of expenses in cents

  def total (expenses: Array[Expense]) :Int = {
    @annotation.tailrec
    def go(acc: Int, expenses: Array[Expense]): Int = expenses match {
      case _ if(expenses.length == 0) => acc
      case _ => go(acc + expenses(0).price, expenses.drop(1))
    }
    
    go(0, expenses)
  }

  val testcase1 = Array[Expense](
    new Expense("Coffee", 450),
    new Expense("Cake", 350) )

  assert (total (testcase1) == 800) // uncomment

  // Add one or two more tests
  // ...


  // Exercise 6

  def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean = {
    @annotation.tailrec
    def go(acc: Boolean, as: Array[A]): Boolean = as match {
      case _ if(as.length == 1) => acc
      case _ => go(acc && ordered(as(0), as(1)), as.drop(1))
    }
    
    go(true, as)
  }

  // some tests
  

  assert ( isSorted (Array(1,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  assert (!isSorted (Array(6,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  assert (!isSorted (Array(1,2,3,4,5,1), (a: Int, b: Int)=> a <= b))

  assert ( isSorted (Array(10,20,35,142,522,643), (a: Int, b: Int)=> a <= b))
  assert (!isSorted (Array(6,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  assert (!isSorted (Array(15,32,0,4233,3,1), (a: Int, b: Int)=> a <= b))

  assert (isSorted (Array("asa", "asd", "zzs"), (a: String, b: String)=> a <= b))
  assert (!isSorted (Array("xsa", "asd", "zzs"), (a: String, b: String)=> a <= b))

 

  // Exercise 7: a curried version of solution to exercise 3

  def power1(x: Double) (n: Int) :Double = n match{
    case _ if(n == 0) => 1 
    case _ if(n%2 == 0 && n > 0) => power1(x)(n/2) * power1(x)(n/2) 
    case _ if(n%2 != 0 && n > 0) => x * power1(x)(n-1)
    case _ => 1/power1(x)(-n)
  }

  // Exercise 8

  def curry[A,B,C] (f: (A,B)=>C) : A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }
  
  //
  // test if it type checks by currying power automatically:

  val power_curried: Double => Int => Double = {
    (x: Double) => (n: Int) => power(x, n)
  }

  // Exercise 9

  def uncurry[A,B,C] (f: A => B => C) : (A,B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  val power_uncurried: (Double,Int) => Double = {
    (x: Double, n: Int) => power1(x)(n)
  }

  // Exercise 10

  def compose[A,B,C] (f: B => C, g: A => B) : A => C = {
    (a: A) => f(g(a))
  }

}


