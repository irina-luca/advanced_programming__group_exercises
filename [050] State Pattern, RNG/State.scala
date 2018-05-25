// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
// 
// AUTHOR1: Vlad Paul Cosma - vlco@itu.dk
// AUTHOR2: Irina Alina Gabriela Luca - irlu@itu.dk

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // Exercise 1 (CB 6.1)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1,rng2) = rng.nextInt 
    i1 match {
      case Int.MinValue => (Int.MaxValue,rng2)
      case _ if(i1<0) => (-i1,rng2)
      case _ => (i1,rng2)
    }
  }

   def nonNegativeInt1(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // Exercise 2 (CB 6.2)

   def double(rng: RNG): (Double, RNG) = { 
    val (i1,rng2) = rng.nextInt
    (1-i1.toDouble/Int.MaxValue,rng2)
   }

  def double1(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 3 (CB 6.3)

   def intDouble(rng: RNG): ((Int, Double), RNG) = { 
    val (i1,rng1) = nonNegativeInt(rng)
    val (i2,rng2) = double(rng1)
    ((i1,i2),rng2)
   }

   def doubleInt(rng: RNG): ((Double, Int), RNG) = { 
    val (i1,rng1) = nonNegativeInt(rng)
    val (i2,rng2) = double(rng1)
    ((i2,i1),rng2)
   }

   def double3(rng: RNG): ((Double, Double, Double), RNG) = { 
    val (i1,rng1) = double(rng)
    val (i2,rng2) = double(rng1)
    val (i3,rng3) = double(rng2)
    ((i1,i2,i3),rng3)
   }

  // def boolean(rng: RNG): (Boolean, RNG) =
  //  rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)

   def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      val (i1,rng1) = rng.nextInt
      if (count>=0) {
        val (i2,rng2) = ints(count-1)(rng1)
        (i1::i2,rng2)
      }
        else (List(),rng1)
    }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

   def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)

   val _double: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // Exercise 6 (CB 6.6)

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (i1, rng1) = ra(rng)
        val (i2, rng2) = rb(rng1)
        (f(i1,i2),rng2)
      } 

  // this is given in the book

   def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
      map2(ra, rb)((_, _))

   val randIntDouble: Rand[(Int, Double)] = both(int, double)

   val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
      fs.foldRight(unit(List[A]()))((r, acc) => map2(r, acc)(_ :: _))

   def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // Exercise 8 (6.8)

  //ASK THE TEACHER
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
  rng => {
          val (i1,rng1) = f(rng)
          g(i1)(rng1)
          }

  def nonNegativeLessThan(n: Int): Rand[Int] = 
    flatMap(unit(n))(i => if (i + (n-1) - (i % n) >= 0) (unit(i % n)) else nonNegativeLessThan(n))

  // Exercise 9 (6.9)

   def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = 
      flatMap(s)(x=> unit(f(x)))

   def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
      flatMap(ra)(a => map(rb)(b => f(a, b)))
}

import State._
import Stream._

case class State[S, +A](run: S => (A, S)) {

  // Exercise 10 (6.10)

   def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

   def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    this.flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 10 (6.10) continued

   def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = 
      sas.foldRight(unit[S, List[A]](List[A]()))((r, acc) => r.map2(acc)(_ :: _))
  //
  // This is given in the book:

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 11
  def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = {
    val (a,s1) = s.run(seed)
    cons(a, state2stream(s)(s1))
  }

  // Exercise 12

  val random_integers = state2stream[RNG,Int](random_int)(_)
}
