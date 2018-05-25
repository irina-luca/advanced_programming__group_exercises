// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
// 
// AUTHOR1: Vlad Paul Cosma - vlco@itu.dk
// AUTHOR2: Irina Alina Gabriela Luca - irlu@itu.dk
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

  // Exercise 2
  // ***Note: This is not tailrec and it will stack overflow for large streams
  // def toList :List[A] = this match {
  //   case Empty => List()
  //   case Cons(h,t) => h() :: t().toList
  // }
  // ***Note: This is a tailrec trial
  def toList :List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]) :List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  // Exercise 3
  def take(n :Int) :Stream[A] = this match {
    case Cons(h, t) if n>0 => cons(h(), t().take(n-1))
    case _ => empty
  }

  def drop (n: Int) :Stream[A] = this match {
    case Cons(h, t) if n>0 => t().drop(n-1)
    case _ => this 
  }

  // Exercise 4
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(),t().takeWhile(p))
    case _ => empty
  }

  // Exercise 5
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((h, t) => p(h) && t)

  // Exercise 6
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = 
    this.foldRight(empty[A])((h, t) => if (p(h)) cons(h,t) else empty)

  // Exercise 7
  def headOptionViaFoldRight():Option[A] =
    foldRight(None : Option[A])( (h,_) => Some(h) )

  // Exercise 8
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t)) 

  def filter(f: A => Boolean): Stream[A] =
  foldRight(empty[A])((h,t) => if (f(h)) cons(h, t) else t) 

  def append[A](a1: Stream[A], a2: Stream[A]): Stream[A] = 
    a2.foldRight(a1:Stream[A])((h, t) => cons(h,t))

  def flatMap[A,B](as: Stream[A])(f: A => Stream[B]) : Stream[B] = 
    as.foldRight(empty: Stream[B])((h, _) => f(h))

  // Exercise 9
  def find (p :A => Boolean) :Option[A]= this.filter(p).headOption
  // This implementation is suitable for streams and unefficient for lists because the filter function will go through all list values, meanwhile with the stream, it takes the first value first and already returns the needed headOption, without analyzing the rest of the stream.

  // Exercise 10
  def fibs () : Stream[Int] = {
      def fibtail(acc1:Int, acc2:Int): Stream[Int] = 
        cons(acc1, fibtail(acc2,acc1+acc2))
     
     fibtail(0,1)
  }

  // Exercise 11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match { 
    case Some((e1,e2)) => cons(e1,unfold(e2)(f))
    case None => empty
  }
  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A,S)) => cons(p._1,unfoldViaMap(p._2)(f))).getOrElse(empty[A])
  
  // Exercise 12
  def fib1 () : Stream[Int] =
    unfold((0, 1)) { case (h1, h2) => Some((h1, (h2, h1 + h2))) }

  def from1 (a:Int) : Stream[Int] = 
    unfold(a)(x => Some((x, x+1)))

  // Exercise 13
  def map1[A,B] (a:Stream[A]) (f :A => B) :Stream[B] = 
    unfold(a) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def take1(n :Int) :Stream[A] = 
  unfold((this,n)) { 
    case (Cons(h,t),n) if(n>0) => Some((h(), (t(),(n-1))))
    case _ => None
   }

  def takeWhile1(p: A => Boolean): Stream[A] =
    unfold((this, p)) { 
      case (Cons(h, t), p ) if p(h()) => Some((h(), (t(),p))) 
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some( ( (Some(h1()), Some(h2())) , (t1(), t2()) ) )
      case (Cons(h1, t1), Empty) => Some( ( (Some(h1()), None) , (t1(), Empty) ) )
      case (Empty, Cons(h2, t2)) => Some( ( (None, Some(h2())) , (Empty, t2()) ) )
      case (_, _) => None
    }

  def zipWith[B, C](s2: Stream[B])(g : (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some( ( g(h1(),h2()) , (t1(), t2()) ) )
      case (Cons(h1,t1), _) => None
      case (_, Cons(h2,t2)) => None
      case (_, _) => None
    }

  // Exercise 14
  def startsWith[A](that: Stream[A]): Boolean = 
    this.zipAll(that).takeWhile(!_._2.isEmpty) forAll {
        case (h,h2) => h == h2
      }
  
  // Exercise 15
  // def tails: Stream[Stream[A]] =
  //   unfold(this) {
  //     case (Cons(h, t)) => Some( ( t(), t() ) )
  //     case _ => None 
  //   }
  def tails: Stream[Stream[A]] =
    unfold(this) { 
      case Cons(h,t) => Some(Cons(h,t),t())
      case Empty => None
    } append2 Stream(empty)

 def append2[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def tails2: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append2 Stream(empty)


}




case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq
  
  // Exercise 1
  def to (n: Int) :Stream[Int] =  
    cons(n, to(n-1))

  def from (n: Int) :Stream[Int] =  
    cons(n, from(n+1))
}

// vim:tw=0:cc=80:nowrap
