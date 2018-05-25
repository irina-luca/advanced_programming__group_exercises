// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
// Vlad Paul Cosma (vlco@itu.dk)
// Irina Luca (irlu@itu.dk)
// Example solutions for Monad exercises, using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monads
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import scala.language.higherKinds
import Monad._


object  MonadSpec extends Properties("Monad[F[_]] laws..") {

  // Note: The law is fine, but remember that scalacheck has presently a very
  // weak function generator (only generates constant functions)
  def associative[A,F[_]] (m: Monad[F]) (implicit a: Arbitrary[F[A]]): Prop =
    forAll { (x: F[A], f: A => F[A], g: A => F[A]) =>
      m.flatMap[A,A] (m.flatMap[A,A] (x) (f)) (g) ==
      m.flatMap (x) (a => m.flatMap (f(a)) (g))
    }

  def identity[A, F[_]] (m: Monad[F]) (implicit arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A]): Prop =
      forAll { (x: F[A], f: A => F[A]) =>
      m.flatMap[A,A] (x) (m.unit[A] (_)) == x } :| "right unit" &&
    forAll { (y :A, f: A => F[A]) =>
      m.flatMap[A,A] (m.unit[A](y)) (f) == f(y) } :| "left unit"

  def monad[A,F[_]] (m :Monad[F]) (implicit arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A]) :Prop =
    associative[A,F] (m) && identity[A,F] (m)

  // uncomment when you have optionMonad
  property ("of optionMonad") = monad[Int,Option] (optionMonad)

  // Exercise 17
  property ("of listMonad") = monad[Int,List] (listMonad)
  property ("of streamMonad over strings") = monad[String,Stream] (streamMonad)
  property ("of streamMonad over integers") = monad[Int,Stream] (streamMonad)


  // Exercise 19

  def kleisliAssociative[A,B,C,D,F[_]] (m:Monad[F]) (implicit arbFB: Arbitrary[F[B]], arbFC: Arbitrary[F[C]], arbFD: Arbitrary[F[D]], arbA: Arbitrary[A]): Prop =
    forAll { (f: A => F[B],g: B => F[C], h: C => F[D], a: A) =>
      m.compose(m.compose(f, g), h)(a) == m.compose(f, m.compose(g, h))(a)
    } :| "kleisliAssociative"

  def kleisliIdentity[A,B,F[_]] (m:Monad[F]) (implicit arbFB: Arbitrary[F[B]],  arbA: Arbitrary[A]): Prop =
    forAll { (f: A => F[B], ma: A) =>
    m.compose((_:Unit) => m.unit[A](ma), f)(()) == f(ma) } :| "kleisliIdentity"


  property ("of kleisliAssociative") = kleisliAssociative[String, Int, Double, Boolean, Stream](streamMonad)
  property ("of kleisliIdentity") = kleisliIdentity[String, Int, Option](optionMonad)
}
