// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing

val l1 :Stream[Int] = Empty
val l2 :Stream[Int] = empty

val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))


println (l1.headOption)
println (l2.headOption)
println (l3.headOption)

// Exercise 2: Test
println("l3.toList is: " + l3.toList)

// Exercise 3: Test
val testTake :Stream[Int] = cons(1, cons(2, cons (3, cons (4, cons (5, empty)))))
val testDrop :Stream[Int] = cons(1, cons(2, cons (3, cons (4, cons (5, empty)))))
val naturals :Stream[Int]= from(0)
println("testTake.take(4).toList is: " + testTake.take(4).toList)
println("testDrop.drop(3).toList is: " + testTake.drop(3).toList)
println("naturals.take(1000000000).drop(41).take(10).toList is: " + naturals.take(1000000000).drop(41).take(10).toList)

// Exercise 4: Test
val testTakeWhile :Stream[Int] = from(0)
println("testTakeWhile.takeWhile(_<1000000000).drop(100).take(50).toList is: " + testTakeWhile.takeWhile(_ < 1000000000).drop(100).take(50).toList )

// Exercise 5: Test
val testForAll : Stream[Int] = from(0)
val testForAll2 :Stream[Int] = cons(1, cons(2, cons (3, cons (4, cons (5, empty)))))
// println("testForAll.forAll(_ >= 0) is: " + testForAll.forAll(_ >= 0)) // it should crash 
println("testForAll2.forAll(_ >= 1) is: " + testForAll2.forAll(_ >= 1))
println("testForAll.forAll(_ < 0) is: " + testForAll.forAll(_ < 0))

// Exercise 6: Test
val testTakeWhile_1 :Stream[Int] = from(0)
println("testTakeWhile_1.takeWhileViaFoldRight(_<1000000000).drop(100).take(50).toList" + testTakeWhile_1.takeWhileViaFoldRight(_ < 1000000000).drop(100).take(50).toList )

// Exercise 7: Test
val headOption_1Test1 :Stream[Int] = from(19)
val headOption_1Test2 :Stream[Int] = cons(1, cons(2, cons (3, cons (4, cons (5, empty)))))
println("headOption_1Test1.headOption is: " + headOption_1Test1.headOption)
println("headOption_1Test2.headOption is: " + headOption_1Test2.headOption)

// Exercise 8: Test
val testEx8 :Stream[Int] = cons(1, cons(2, cons (3, cons (4, cons (5, empty)))))
println("testEx8.map(x => x+1) is:" + testEx8.map(x => x+1))
println("testEx8.filter(x => x%2 == 0) is:" + testEx8.filter(x => x%2 == 0))
// println("append(testEx8, l3) is:" + append(testEx8, l3))
// println("flatMap(testEx8)(x => x+1) is:" + flatMap(testEx8)(x => x+1))

// Exercise 11
println(l3.unfold(2)(x => Some((x+1, x+2))))

// Exercise 13
val testTakeWhile1 :Stream[Int] = from(0)
println("testTakeWhile1.takeWhile1(_<1000000000).drop(100).take(50).toList is: " + testTakeWhile1.takeWhile1(_ < 1000000000).drop(100).take(50).toList )
println( l3.take1(2))
println( l3.zipAll(testEx8))
println( l3.zipWith(testEx8)((x, y) => x+y))

// Exercise 14
println( l3.startsWith(testEx8))

// Exercise 15: Test
val testTails :Stream[Int]= cons(1, cons(2, cons (3, empty)))
println(testTails.tails.toList)
println(testTails.tails2.toList)

