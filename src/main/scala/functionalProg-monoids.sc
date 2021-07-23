/**
 * ref: https://youtu.be/DJyhWAwmGqE
 * Monoid:
 * - Some type A
 * - Binary associative operation: take two values and combine = Semigroup
 *   them into one
 * _ A is an identity for that operation
 * */

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def unit: A
}


object FromMonoid{

  /** List("a", "b", "c").foldLeft("")(_ + _) */
  implicit val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def unit = ""
  }
  /* List("a", "b", "c").foldLeft("")(_ + _)*/
  implicit val intMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def unit = 0
  }
  implicit def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def unit= Nil
  }
  //forming monoid with list concatenation
  def monoidOp[A](l: List[A])(implicit m: Monoid[A]): A ={
    if (l.isEmpty) m.unit
    else m.op(l.head, monoidOp(l.tail))
  }

}

import FromMonoid.{monoidOp, _}

println(monoidOp(List("a", "b", "c")))
println(monoidOp(List(12, 10, 5)))

// //exc 10.1
object FromMonoidExc{

  val intAdditionMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def unit = 0
  }

  val intMultiplicationMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def unit = 1
  }

  val booleanOrMonoid = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def unit = false
  }

  val booleanAndMonoid = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def unit = true
  }

  //forming monoid with list concatenation
  def monoidOpExc[A](op1: A,op2: A)(m: Monoid[A]): A ={
    if ( m.op(op1,op2) == 0 ) m.unit
    else m.op(op1,op2)
  }

}

/** Exc 10.2 */
object OptMonoidExc {
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op (x: Option[A], y: Option[A]) = x orElse y
    def unit: Option[A] = None
  }

  def optionMonoidOp[A](op1: Option[A],op2: Option[A])(m: Monoid[Option[A]]): Option[A] ={
    if ( op1.isDefined && op1.isDefined ) m.op(op1,op2)
    else m.op(op1,op2)
  }
}

/**
 * compose: makes a new function that composes other functions f(g(x))
 */
def f(s: String) = "f(" + s + ")"
def g(s: String) = "g(" + s + ")"
val fComposeG = f _ compose g _
fComposeG("yay")

/**
 * andThen is like compose, but calls the first function and then the second, g(f(x))
 */

/** Exc 10.3 */
def endoMonoid[A] = new Monoid[A => A] {
  def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2 // a1(a2(x))
  def unit = (a: A) => a
}

def dual[A](m: Monoid[A]) = new Monoid[A] {
  def op(a1:A, a2: A): A = m.op(a1, a2)
  def unit = m.unit
}

val intEndoMonoid = endoMonoid[Int]
val intMonoidDual = dual(intEndoMonoid)
val op = intEndoMonoid.op _
val dop = intMonoidDual.op _
val unit = endoMonoid[Int].unit

val inc: Int => Int = x => x + 1
val twice: Int => Int = x => x + x
val square: Int => Int = x => x * x

/** Identity function*/
/**
 * identity element(a kind of element  which leaves any element
 * of the set unchanged when combined with it )
 */

unit(20) == 20

/** monoid law: Binary associative operation */
op(inc,op(twice,square))(3) == op(op(inc,twice),square)(3)
op(inc,unit)(3) == inc(3)
op(unit, inc)(3) == inc(3)

/** the monoid's op compose functions */
/**
 * compose: makes a new function that composes other functions f(g(x))
 */
op(inc, twice)(3) == inc(twice(3))

val incComposeTwice = inc compose twice
incComposeTwice(3) == inc(twice(3))
op(unit,op(inc, twice))(3) == 7
op(twice,op(inc, unit))(3) == 8

