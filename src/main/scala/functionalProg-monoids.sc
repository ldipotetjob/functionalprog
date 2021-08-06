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

val optionMonoid = new Monoid[Option[String]] {
  def op(a1: Option[String], a2: Option[String]) = Option(a1.getOrElse("") + a2.getOrElse(""))
  def unit = None
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
  def op(a1: A => A, a2: A => A): A => A = a1 compose a2 // a1(a2(x))
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

/** Exc 10.5*/
/** List("a", "b", "c").foldLeft("")(_ + _) */
// "hdhd hdhdh hdhdd".split(" ").foldLeft("")((a,b) => ( a + b))
//TODO

/** Exc 10.6 */
def concatenate[A](ls: List[A])(implicit monoid: Monoid[A]): A =
  ls.foldLeft(monoid.unit)(monoid.op)

concatenate(List("Louis","Peter"))

/** Exc 10.7 */
/**
 * The fold method for a List takes two arguments; the start value and a function.
 * This function also takes two arguments; the accumulated value and the current item in the list.
 * So here's what happens:
 * 1. At the start of execution, the start value that you passed as the first argument is given to your function as
 * its first argument. As the function's second argument it is given the first item on the list (in the case of fold
 * this may or may not be the actual first item on your list as you will read about below).
 * 2. The function is then applied to its two arguments, in this case a simple addition, and returns the result.
 * 3. Fold then gives the function the previous return value as its first argument and the next item in the list as its
 * second argument, and applies it, returning the result.
 * 4. This process repeats for each item of the list and returns the return value of the function once all items
 * in the list have been iterated over.
 * */

// ver.1
/** In my opinion It is a better solution because it describe a "foldable" solution */
def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.unit)((a, b) => m.op(a, f(b)))

// ver.2
def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
  if (as.isEmpty)
    m.unit
  else if (as.length == 1)
    f(as(0))
  else {
    val (l, r) = as.splitAt(as.length / 2)
    m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }

assert(
foldMap(List(80,56),stringMonoid)((x: Int) => x.toString)  ==
  foldMapV(List(80,56).toIndexedSeq, stringMonoid)((x: Int) => x.toString)
)
assert(
foldMap(List("Peter","Lois"),stringMonoid)((x: String) => x.toUpperCase) ==
foldMapV(List("Peter","Lois").toIndexedSeq, stringMonoid)((x: String) => x.toUpperCase)
)

assert(
foldMap(List("Peter","Lois"),intMonoid)(_.length) ==
  foldMapV(List("Peter","Lois").toIndexedSeq, intMonoid)(_.length)
)

foldMapV(List("Peter","Lois").toIndexedSeq, stringMonoid)((x: String) => x.toUpperCase)

/** Exc 10.8 */

/** Exc 10.9 */
sealed  trait WC

/** A Stub is the simplest case, where we have not seen any complete words yet. */
case class Stub(chars:String) extends WC

/**
 * Part keeps the number of complete words we have seen so far, in words.
 *
 * @param lStub: holds any partial word we have seen to the left of those words
 * @param words  the number of complete words
 * @param rStub: holds the ones on the right
 *
 * Example:  "lorem ipsum dolor sit amet, "
 * lorem ipsum dolor sit amet,
 * lorem ipsum do  lor sit amet,_
 * |------------|  |------------|
 *
 * Part("lorem", 1, "do") // only guaranteed 1 complete words(ipsum)
 * Part(lor,2," ") // only guaranteed "2" complete words(sit amet)
 */

case class Part(lStub: String, words: Int, rStub: String) extends WC

val wcMonoid: Monoid[WC] = new Monoid[WC]{
  // The empty result, where we haven't seen any characters yet.
  val unit = Stub("")
  def op(a: WC, b: WC) = (a, b) match{
    case(Stub(c), Stub(d)) => Stub(c+d)
    case(Stub(c), Part(l,w,r)) => Part(c+l, w, r)
    case(Part(l, w, r), Stub(c)) => Part(l,w,r+c)
    case(Part(l1, w1, r1), Part(l2, w2, r2)) =>
      Part(
        l1,
        w1 + (if((r1 + l2).isEmpty)0 else 1) + w2,
        r2
      )
  }
}

/** counting how many words in a string */
def count(s: String): Int = {
  def wc(c: Char): WC = if (c.isWhitespace) Part("", 0, "")
  else Stub(c.toString)
  def unstub(s: String) = s.length min 1
  foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
    case Stub(s) => unstub(s)
    case Part(l, w, r) => unstub(l) + w + unstub(r) }
}

// TODO -> parsing/processing file in parallel form

/** Testing our solution*/
count("we are making test for count words in a string")

/**
 *  For parallel processing Akka is my recommendation
 **/

val ints = List(100,22,33,4).toIndexedSeq

val orderedMon = new Monoid[Option[(Int, Int, Boolean)]] {
  def op (o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
    (o1, o2) match {
    // The ranges should not overlap if the sequence is ordered.
    case (Some((x1, y1, p)), Some((x2, y2, q))) => Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
    case (x, None) => x
    case (None, x) => x
  }
  val unit = None
}

/**
 * def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.unit)((a, b) => m.op(a, f(b)))
 *
 * foldMap(List(100,22,33,4), orderedMon)(i => Some((i, i, true)))
 * internal runtime
 * 100. (None, 100) => orderedMon.op(None, Some((100,100,true))) => Some((100,100,true))
 * 22.  (Some((100,100,true)), 22) => orderedMon.op(Some((100,100,true)), Some((22,22,true))) => Some((22,100,false))
 * .
 * .
 * 4.   ........ ......... .......... Some((4,100,false))
 * */

foldMapV(ints, orderedMon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
/* Better solution**/
foldMap(List(100,22,33,4), orderedMon)(i => Some((i, i, true))).forall(_._3)

val isSorted: List[Int] => Boolean = (a: List[Int]) => a == a.sorted
isSorted(List(100,22,33,4))

/** It does not correspond to any solution it is just an example of a very specific Monoid */
def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
  new Monoid[Map[K, V]] {
    def unit = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(unit) { (acc,k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.unit),
          b.getOrElse(k, V.unit)))
      }
  }