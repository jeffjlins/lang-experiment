sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

   //# 3.25
   def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
   }

   //# 3.26
   def maximum(t: Tree[Int]): Int = t match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l) max maximum(r)
   }

   //# 3.27
   def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
   }

   //# 3.28
   def map[A,B](t: Tree[A])(f: (A) => B): Tree[B] = t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
   }

   //# 3.29
   def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
      case Leaf(x) => f(x)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
   }

   def sizeViaFold[A](t: Tree[A]): Int = {
      fold(t)((_) => 1)(_ + _)
   }

   def maximumViaFold(t: Tree[Int]): Int = {
      fold(t)((x) => x)((l: Int, r: Int) => l max r)
   }

   def depthViaFold[A](t: Tree[A]): Int = {
      fold(t)((_) => 1)((l, r) => (l + 1) max (r + 1))
   }

   def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
      fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
   }

}

//# 3.25
val sample: Tree[Int] = Branch(Leaf(3), Branch(Leaf(1), Leaf(6)))
Tree.size(sample)

//# 3.26
Tree.maximum(sample)

//# 3.27
Tree.depth(sample)

//# 3.28
Tree.map(sample)(_ * 2)

//# 3.29
Tree.fold(sample)(_ * 2)(_ + _)
Tree.size(sample)
Tree.maximum(sample)
Tree.depth(sample)
Tree.map(sample)(_ * 2)