import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
   def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
   }
   def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
   }
   def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
   }

   def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
   }

   def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
   }

   def sum2(ns: List[Int]) = {
      foldRight(ns, 0)((x, y) => x + y)
   }

   def product2(ns: List[Double]) = {
      foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
   }

   //# 3.2
   def tail[A](xs: List[A]): List[A] = xs match {
      case Cons(_, t) => t
      case Nil => Nil
   }
   //# 3.3
   def setHead[A](xs: List[A], h: A): List[A] = xs match {
      case Cons(_, t) => Cons(h, t)
      case Nil => Cons(h, Nil)
   }
   //# 3.4
   def drop[A](xs: List[A], amt: Int): List[A] = {
      if (amt <= 0) xs
      else xs match {
         case Nil => Nil
         case Cons(_, t) => drop(t, amt - 1)
      }
   }
//   def drop[A](xs: List[A], amt: Int): List[A] = (xs, amt) match {
//      case (Nil, _) => Nil
//      case (_, _) if amt <= 0 => xs
//      case (Cons(_, t), _) => drop(t, amt - 1)
//   }

   //# 3.5
   def dropWhile[A](xs: List[A], cond: A => Boolean): List[A] = xs match {
      case Cons(h, t) if cond(h) => dropWhile(t, cond)
      case _ => xs
   }
   def dropWhile2[A](xs: List[A])(cond: A => Boolean): List[A] = xs match {
      case Cons(h, t) if cond(h) => dropWhile2(t)(cond)
      case _ => xs
   }
//   def dropWhile[A](xs: List[A], cond: A => Boolean): List[A] = xs match {
//      case Cons(h, t) => if (cond(h)) dropWhile(t, cond) else xs
//   }

   //# 3.6
   // this will cause stack overflows in big lists
   def init[A](xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
   }
   // a mutable list has to be used here so that the last statement can be the recursive call, otherwise the recursive call in in a constructor and so is not the last statement
   // an inner method is used so that the mutable list is not exposed
   def init2[A](l: List[A]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]
      @annotation.tailrec
      def go(cur: List[A]): List[A] = cur match {
         case Nil => sys.error("init of empty list")
         case Cons(_,Nil) => List(buf.toList: _*)
         case Cons(h,t) => buf += h; go(t)
      }
      go(l)
   }

   //# 3.9
   def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, x) => x + 1)
   }

   //# 3.10
   @annotation.tailrec
   def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
   }

   //# 3.11
   def sum3(ns: List[Int]) = {
      foldLeft(ns, 0)(_ + _)
   }

   def product3(ns: List[Double]) = {
      foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
   }

   def length3[A](as: List[A]): Int = {
      foldLeft(as, 0)((x, _) => x + 1)
   }

   //# 3.12
   def reverse[A](xs: List[A]): List[A] = {
      foldLeft(xs, Nil:List[A])((acc, x) => Cons(x, acc))
   }

   //# 3.14
   def appendViaFold[A](a1: List[A], a2: List[A]): List[A] = {
      foldRight(a1, a2)(Cons(_, _))
   }

   //# 3.15
   def concat[A](xs: List[List[A]]) = {
//      foldRight(xs, Nil: List[A]) { (xsl, accl) =>
//         foldRight(xsl, accl)(Cons(_, _))
//      }
      foldRight(xs, Nil: List[A])(appendViaFold)
   }

   //# 3.16
   def addOne(xs: List[Int]) = {
      foldRight(xs, Nil: List[Int])((x, y) => Cons(x + 1, y))
   }

   //# 3.17
   def toStrings(xs: List[Int]) = {
      foldRight(xs, Nil: List[String])((x, y) => Cons(x.toString, y))
   }

   //# 3.18
   def map[A, B](xs: List[A])(f: A => B): List[B] = {
      foldRight(xs, Nil: List[B])((x, y) => Cons(f(x), y))
   }

   //# 3.19
   //@tailrec can't do this because the Cons surrounds the filter
   def filter[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
      case Nil => xs
      case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
   }
   def filter2[A](xs: List[A])(f: A => Boolean): List[A] = {
      val lb = new ListBuffer[A]()
      @tailrec
      def go(xs2: List[A]): List[A] = xs2 match {
         case Nil => xs
         case Cons(h, t) => if (f(h)) lb += h; go(t)
      }
      go(xs)
      List(lb: _*)
   }

   //# 3.20
   def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
      concat(map(as)(f))
   }

   //# 3.21
   def filterViaFlatMap[A](xs: List[A])(f: A => Boolean): List[A] = {
      flatMap(xs) { (x) =>
         if (f(x)) List(x)
         else Nil
      }
   }

   //# 3.22
   def addLists(xa: List[Int], xb: List[Int]): List[Int] = (xa, xb) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addLists(ta, tb))
   }

   //# 3.23
   def zipWith[A, B, C](xa: List[A], xb: List[B])(f: (A, B) => C): List[C] = (xa, xb) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
   }

//   @tailrec
//   def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
//      case (_,Nil) => true
//      case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
//      case _ => false
//   }

   def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
      val results = List.zipWith(sup, sub) { (supe, sube) =>
         supe == sube
      }
      List.foldLeft(results, true)(_ == _)
   }

   @tailrec
   def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => false
      case Cons(h, t) => if (startsWith(sup, sub)) true else hasSubsequence(sup, sub)
   }
}


//# 3.1
val r = List(1,2,3,4,5) match {
   case Cons(x, Cons(2, Cons(4, _))) => x
   case Nil => 42
   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
   case Cons(h, t) => h + List.sum(t)
   case _ => 101
}

//# 3.5ish
val xs: List[Int] = List(1,2,3,4,5)
val ex1 = List.dropWhile(xs, (x: Int) => x < 4)
// this does not compile because not all type information flows from one arg to the next in a single argument list at the call site
//val ex1 = List.dropWhile(xs, x => x < 4)
// but this does compile because they are separate argument lists
val ex3 = List.dropWhile2(xs)(x => x < 4)

//# 3.7
// no, product2 using foldRight cannot halt recursion it it encounters 0.0 in the middle of the list because the function provided is executed strictly so doesn't run until it's argument is evaluated and the recursion is in the argument so the function doesn't run until it reaches the last element in the list (then it runs on the last one, next to last and so on until the beginning)
// foldRight works the same as nesting the functions naturally the same way as cons and using the starting value as Nil
//    functions are right associative just like foldRight
//# 3.8
val l = List(1, 2, 3)
val l4 = List(1, 2, 3, 4)
val r2 = List.foldRight(l, Nil:List[Int])(Cons(_, _))

//# 3.9
List.length(l)

//# 3.10
//does not compile
//val r3 = List.foldLeft(l, Nil:List[Int])(Cons(_, _))
//# 3.12
List.reverse(l)
//# 3.14
val l4z = List(4, 5, 6)
List.append(l, l4z)
List.appendViaFold(l, l4z)

//# 3.15
val l7z = List(7, 8, 9)
val ll = List(l, l4z, l7z)
List.concat(ll)
//# 3.16
List.addOne(l)
//# 3.17
List.toStrings(l)

//# 3.18
List.map(l)(_ + 1)
List.map(l)(_.toString)

//# 3.19
List.filter(l)(_ % 2 != 0)
List.filter2(l)(_ % 2 != 0)

//# 3.20
List.flatMap(l)(i => List(i, i))

//# 3.21
List.filterViaFlatMap(l)(_ % 2 != 0)

//# 3.22
List.addLists(l, l)

//# 3.23
List.zipWith(l, l)(_ + _)

//# 3.24
List.hasSubsequence(l4, List(2, 3))
