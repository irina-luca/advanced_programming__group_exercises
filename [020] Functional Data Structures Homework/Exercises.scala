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

// An ADT of Lists

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 1
  // Answer: third case >> val x = 3

  // Exercise 2
  def tail[A] (as: List[A]) :List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => t
  } 

  // Exercise 3
  def setHead[A] (as: List[A], newHead: A) : List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(newHead, t) 
  }
  

  // Exercise 4
  def drop[A] (l: List[A], n: Int) : List[A] = l match {
    case Nil => Nil
    case _ if(n == 0) => l
    case Cons(h, t) => drop(t, n-1)
  }

  // Exercise 5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if(f(h)) dropWhile(t, f) else l// could curry dropWhile
  }

 
  // Exercise 6
  def init[A](l: List[A]): List[A] = l match {    
    case Nil => Nil
    case Cons(h, Nil) => tail(l)
    case Cons(h, t) => Cons(h, init(t))
  }
  

  // Exercise 7 is in the bottom of the file

  // Exercise 8
  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }

  def length[A] (as: List[A]): Int = 
    foldRight(as, 0) ((_, acc) => acc+1)

  def lengthLeft[A] (as: List[A]): Int = 
      foldLeft(as, 0) ((acc, _) => acc+1)

  // Exercise 9
  @annotation.tailrec
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs,f(z,x)) (f)
  }

  // Exercise 10
  def sum (as : List[Int]) : Int = 
    foldLeft(as, 0) (_ + _)

  def product (as :List[Int]) : Int = 
    foldLeft(as, 1) (_ * _)

  def length1 (as :List[Int]) : Int = 
    foldLeft(as, 0) ((acc, _) => acc+1)


  // Exercise 11
  def reverse[A] (as :List[A]) :List[A] = 
    foldLeft(as, Nil:List[A]) ((acc, h)=> Cons(h, acc))

  // Exercise 12
  @annotation.tailrec
  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = as match {
    case Nil => z
    case Cons(x,xs) => foldRight1(reverse(xs),f(x,z)) (f) 
  }

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = 
    foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
 

  // Exercise 13
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]) :List[A] = {
    def go(as: List[List[A]] ,acc:List[A]) :List[A] = as match {
      case Nil => acc
      case Cons(h,t) => go(t,append(acc,h))
    }
    go(as,Nil)
  }
  def concat1[A] (as: List[List[A]]) :List[A] = 
    foldRight(as, Nil:List[A])(append)
    // foldLeft(as, Nil:List[A])(append)

  // Exercise 14
  def map[A,B] (a :List[A]) (f :A => B) :List[B] = 
    foldLeft[A,List[B]] (reverse(a), Nil)((z, x) => Cons(f(x),z))

  // Exercise 15 (no coding)
  // If using foldLeft, the the result will come in reversed order and a new computation with foldLeft would be necessary, which means traversing the list two times. Instead, by using foldRight, we might get a stack overflow problem (we allocate a lot of space), but we only traverse the list one time.


  // Exercise 16
  def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = 
    foldRight1(as, Nil:List[A])((h, t) => if(f(h)) Cons(h,t) else t)

  // Exercise 17  
  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = {
    foldLeft[A,List[B]] (as,Nil)((z, x) => append(z,f(x)))
  }

  // Exercise 18
  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = 
    flatMap(l)( a => if(p(a)) List(a) else Nil )

  // Exercise 19
  def add (l: List[Int]) (r: List[Int]): List[Int] = (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(head1, tail1), Cons(head2, tail2)) => Cons((head1+head2), add(tail1)(tail2))
  }

  // Exercise 20
  def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(f(head1, head2), zipWith(f)(tail1, tail2))
  }

  // Exercise 21
  def beginsWith[A] (list: List[A], sub: List[A]) :Boolean = (list, sub) match {
    case (_, Nil) => true
    case (Cons(bighead, bigtail), Cons(smallhead, smalltail)) if(bighead == smallhead) => beginsWith(bigtail, smalltail)
    case _ => false
  }

  def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = sup match {
    case Nil if(sub == Nil) => true
    case _ if(beginsWith(sup, sub)) => true
    case Cons(h, t) => hasSubsequence(t, sub)
    case _ => false
  }

}




// Exercise 7
object Exercise7 {

  case class SalaryLine(name: String, amount: Integer)

  def maximumSalary (salaries: List[SalaryLine]) :Integer = {
    def go(salaries: List[SalaryLine],acc:Integer) : Integer =  salaries match {
      case Nil => -1
      case Cons(h,Nil) => if(h.amount>acc) h.amount else acc
      case Cons(h,xs) => go(xs,if (h.amount>acc) h.amount else acc )  
    }
    
    go(salaries, 0)
  }


  val test_case = List( SalaryLine("John",41),
    SalaryLine("Alice", 42),
    SalaryLine("Bob",40))

   assert(maximumSalary(test_case)==42)
   assert(maximumSalary(test_case)!=102)
   assert(maximumSalary(Nil) == -1)
}


