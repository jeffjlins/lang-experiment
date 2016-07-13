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

  def take3(amt: Int): Stream[A] = unfold( (this, amt) ) {
    case (Empty, _) => None
    case (_, a) if a == 0 => None
    case (Cons(h, t), a) => Some( (h(), (t(), a - 1)) )
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

  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some( (h(), t()) )
    case default => None
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => if (!p(a)) false else b)

  def headOption2: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map1[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A]) { (a, b) =>
    if (f(a)) cons(a, b)
    else b
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  def zipWith[B, C](xb: Stream[B])(f: (A, B) => C): Stream[C] = unfold( (this, xb) ) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some( (f(h1(), h2()), (t1(), t2())) )
  }

  def zipAll[B](xb: Stream[B]): Stream[(Option[A], Option[B])] = unfold( (this, xb) ) {
    case (Empty, Empty) => None
    case (Empty, Cons(h, t)) => Some( ( (None, Some(h())), (Empty, t()) ) )
    case (Cons(h, t), Empty) => Some( ( (Some(h()), None), (t(), Empty) ) )
    case (Cons(h1, t1), Cons(h2, t2)) => Some( ( (Some(h1()), Some(h2())), (t1(), t2()) ) )
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    this.zipAll(s).foldRight(true) {
      case (_, false) => false
      case ((None, Some(_)), _) => false
      case ((Some(_), None), prev) => prev
      case ((Some(a), Some(b)), cond) => a == b
      case default => false
    }
  }

  def startsWith2[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile3(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }
  }

  def tails1: Stream[Stream[A]] = this match {
    case Cons(h, t) => cons(this, t().tails1)
    case x @ Empty => x
  }

  def tails2: Stream[Stream[A]] = unfold(this) {
    case x @ Cons(h, t) => Some((x, t()))
    case Empty => None
  } append Stream(empty)

//  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
//    this.foldRight((z, this)) {
//      case ((el, s @ Cons(h, t)), acc) => (f(el, acc), t)
//    }
//  }

  //TODO: did not get this one
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

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

  def constant1[A](a: A): Stream[A] = {
    lazy val r: Stream[A] = Stream.cons(a, r)
    r
  }

  def constant2[A](a: A): Stream[A] = {
    unfold(a) { x => Some(a, a) }
  }

  def from1(n: Int): Stream[Int] = cons(n, from1(n + 1))

  def from2(n: Int): Stream[Int] = unfold(n) { x => Some((x, x + 1))}

  def fibs1(): Stream[Int] = {
    def go(x: Int, y: Int): Stream[Int] = {
      cons(x, go(y, x + y))
    }
    go(0, 1)
  }

  def fibs2(): Stream[Int] = unfold((0, 1)) { case (x, y) => Some((x, (y, x + y)))}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty[A]
      case Some((nextVal, nextState)) => cons(nextVal, unfold(nextState)(f))
    }
  }

  //==================================================

  val ones: Stream[Int] = Stream.cons(1, ones)
}

object Example {
  def main(args: Array[String]) {
    ex516
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
    println(s.takeWhile1(x => { println("tw"); x < 5 }).take2(2).toList3)
  }

  def ex54 = {
    val s = Stream(1, 2, 3, 4, 5, 6)
    println(s.forAll(_ < 7))
    println(s.forAll({ x => println("fa"); x < 3 }))
  }

  def ex55 = {
    val s = Stream(1, 2, 3, 4, 5, 6)
    println(s.takeWhile2(x => { println("tw"); x < 5 }).take2(2).toList3)
  }

  def ex56 = {
    val s = Stream(1, 2)
    val s2 = Stream()
    println(s.headOption2)
    println(s2.headOption2)
  }

  def ex57 = {
    val s = Stream(1, 2, 3, 4, 5, 6)
    println(s.map1(x => { println("m"); x * 2 }).take2(3).toList3)
    println(s.filter(x => { println("f"); x % 2 == 0 }).take2(2).toList3)
    println(s.append(Stream(7, 8)).toList3)
    println(s.flatMap(x => { println("fm"); Stream(x, x) }).take2(8).toList3)
  }

  def ex58 = {
    println(Stream.constant1(5).take2(5).toList3)
  }

  def ex59 = {
    println(Stream.from1(5).take2(5).toList3)
  }

  def ex510 = {
    println(Stream.fibs1().take2(10).toList3)
  }

  def ex512 = {
    println(Stream.constant2(5).take2(5).toList3)
    println(Stream.from2(5).take2(5).toList3)
    println(Stream.fibs2().take2(10).toList3)
  }

  def ex513 = {
    val s = Stream(1, 2, 3, 4, 5, 6)
    println(s.map2(x => { println("m"); x * 2 }).take2(3).toList3)
    println(s.map2(x => { println("m"); x * 2 }).toList3)
    println(s.take3(3).toList3)
    println(s.take3(100).toList3)
    println(s.takeWhile3(x => { println("tw"); x < 5 }).take2(2).toList3)
    println(s.takeWhile3(x => { println("tw"); x < 5 }).toList3)
    println(Stream("a", "b", "c").zipWith(Stream.constant2("x"))(_ + _).toList3)
    println(Stream.constant2("x").zipWith(Stream("a", "b", "c"))(_ + _).toList3)
    println(Stream(1, 2, 3).zipAll(Stream("a", "b")).toList3)
  }

  def ex514 = {
    println(Stream(1, 2, 3, 4, 5, 6).startsWith(Stream(1, 2, 3)))
    println(Stream(1, 2, 3, 4, 5, 6).startsWith(Stream(1, 2, 4)))
  }

  def ex515 = {
    val s = Stream(1, 2, 3, 4, 5, 6)
    println(s.tails2.map2(_.toList3).toList3)
  }

  def ex516 = {
    val s = Stream(1, 2, 3, 4, 5, 6)
    println(s.scanRight(0)(_ + _).toList3)
  }

}
