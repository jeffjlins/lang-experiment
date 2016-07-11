package fpis.ch5

import scala.collection.mutable.ListBuffer

trait Stream[+A] {

  import Stream._

  def toList1: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList1
  }

  def toList2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => go(t(), h() :: l)
    }
    go(this, Nil).reverse
  }

  def toList3: List[A] = {
    val lb = new ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => lb.toList
      case Cons(h, t) =>
        lb += h()
        go(t())
    }
    go(this)
  }

  def take1(amt: Int): Stream[A] = {
    def go(s: Stream[A], amt: Int): Stream[A] = {
      s match {
        case Empty => Empty
        case Cons(h, t) =>
          if (amt == 0)
            Empty
          else {
            cons(h(), go(t(), amt - 1))
          }
      }
    }
    go(this, amt)
  }

  def take2(amt: Int): Stream[A] = this match {
    case Cons(h, t) if amt > 1 => cons(h(), t().take2(amt - 1))
    case Cons(h, t) if amt == 1 => cons(h(), Empty)
    case _ => Empty
  }

  def drop1(amt: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], amt: Int): Stream[A] = {
      s match {
        case Empty => Empty
        case ss if amt == 0 =>
          ss
        case Cons(_, t) =>
          go(t(), amt - 1)
      }
    }
    go(this, amt)
  }

  @annotation.tailrec
  final def drop2(amt: Int): Stream[A] = this match {
    case Cons(_, t) if amt > 1 => t().drop2(amt - 1)
    case Cons(_, t) if amt == 1 => t()
    case _ => Empty
  }

  def takeWhile1(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile1(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A]) { (a, b) =>
    if (p(a)) cons(a, b)
    else empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => if (!p(a)) false else b)

  def headOption2: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A]) { (a, b) =>
    if (f(a)) cons(a, b)
    else b
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

//  def flatMap[B](f: A => Stream[B]): Stream[B] =

  //=====================

  def headOption1: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  //=========================

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  //==================================================

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}

object Example {
  def main(args: Array[String]) {
    ex56
  }

  def ex51() = {
    val s = Stream(1, 2, 3)
    println(s.toList1)
    println(s.toList2)
    println(s.toList3)
  }

  def ex52 = {
    val s = Stream(1, 2, 3, 4, 5, 6)
    println(s.take2(3).toList3)
    println(s.drop2(2).toList3)
  }

  def ex53 = {
    val s = Stream(1, 2, 3, 4, 5, 6)
    println(s.takeWhile1(x => {println("tw"); x < 5}).take2(2).toList3)
  }

  def ex54 = {
    val s = Stream(1, 2, 3, 4, 5, 6)
    println(s.forAll(_ < 7))
    println(s.forAll({x => println("fa"); x < 3}))
  }

  def ex55 = {
    val s = Stream(1, 2, 3, 4, 5, 6)
    println(s.takeWhile2(x => {println("tw"); x < 5}).take2(2).toList3)
  }

  def ex56 = {
    val s = Stream(1, 2)
    val s2 = Stream()
    println(s.headOption2)
    println(s2.headOption2)
  }
}
