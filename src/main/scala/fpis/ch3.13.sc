sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
   def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
   }

   def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
   }

   @annotation.tailrec
   def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
   }

   //# 3.12
   def reverse[A](xs: List[A]): List[A] = {
      foldLeft(xs, Nil:List[A])((acc, x) => Cons(x, acc))
   }

   //# 3.13
   def foldRightViaFoldLeft_1[A,B](xs: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(reverse(xs), z)((b, a) => f(a, b))
   }

   def foldRightViaFoldLeft_2[ELEM,ACC](l: List[ELEM], z: ACC)(f: (ELEM,ACC) => ACC): ACC = {
      def wrapped(zb: ACC => ACC, a: ELEM): ACC => ACC = {
         (b: ACC) => zb(f(a, b))
      }
      //foldLeft(l, (x: ACC) => x)((zb, a) => (b: ACC) => zb(f(a, b))(z)
      foldLeft(l, (x: ACC) => x)(wrapped)(z)
   }
   //val fa = (xa: Int) => xa
   // f(f(f(fa, 1), 2), 3)(0)

   // wrapped() creates: passes in real initial function
   //  first arg: grabs first element of list
   //  second arg: instead of passing initial accumulator value, use a pass-through function
   //  f("pass-through function", "first element of list = 1")
   // wrapped() creates: wraps previous function in new function
   //  first arg: grabs second element of list
   //  second arg:

   // WTF
   def foldRightViaFoldLeft_1a[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
      foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
   }
   // WTF
   def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = {
      foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
   }

}

val l = List(1, 2, 3)
val l4 = List(1, 2, 3, 4)
val r2 = List.foldRight(l, Nil:List[Int])(Cons(_, _))
val r3 = List.foldRight(l, Nil:List[Int])(Cons(_, _))

List.foldRight(l, 0)(_ + _)
List.foldRightViaFoldLeft_1(l, Nil:List[Int])(Cons(_, _))
List.foldRightViaFoldLeft_1(l4, Nil:List[Int])(Cons(_, _))
List.foldRightViaFoldLeft_1(l, 0)(_ + _)
val h: (List[Int]) => (List[Int]) = (a) => a
val l3 = List(1, 2, 3)
//List.foldRightViaFoldLeft_1(l3, 0)(_ + _)
List.foldLeft(l3, (xx: Int) => xx)((accFunc, a: Int) => ((b: Int) => accFunc(a + b)))(0)
// (0) => 0
// (1) => (((0) => 0) => 1 + 0)
// (2) => (((1) => ((0 => 0) => 1 + 0)) => 2 + 1)
// (3) => (((2) => (((1) => ((0 => 0) => 1 + 0)) => 2 + 1)) => 3 + 3)

val aaa = 3
def aa(a: Int) = {
   def bb(b: Int) = {
      def cc(c: Int) = {
         def dd(d: Int) = {
            d
         }
         c + dd(0)
      }
      b + cc(1)
   }
   a + bb(2)
}
aa(3)
//def foldLeftOuter[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
//   case Nil => z
//   case Cons(h, t) => foldLeftOuter(t, f(z, h))(f)
//}

val f = (xf: (Int => Int), xa: Int) => {
   val fbb = (xb: Int) => {
      xf(xa + xb)
   }
   fbb
}

val fa = (xa: Int) => xa
val fb = f(fa, 1) // == (x: Int) => fa(1 + x)
val fc = f(fb, 2)
val fd = f(fc, 3)
fd(0)
f(f(f(fa, 1), 2), 3)(0)
