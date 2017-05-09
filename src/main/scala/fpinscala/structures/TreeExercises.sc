sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]



def fold[A, B](t: Tree[A])(f1: A => B, f2: (B, B) => B): B = t match {
  case Leaf(a) => f1(a)
  case Branch(l: Tree[A], r: Tree[A]) => f2(fold(l)(f1, f2), fold(r)(f1, f2))
}




def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l, r) => 1 + size(l) + size(r)
}

def size2[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 1,  1 + _ + _)




def maximum(t: Tree[Int]): Int = t match {
  case Leaf(v) => v
  case Branch(l, r) => maximum(l) max maximum(r)
}

def maximum2(t: Tree[Int]): Int = fold[Int, Int](t)(identity, _ max _)




def depth[A](t: Tree[A]): Int = t match {
  case Leaf(v) => 0
  case Branch(l, r) => 1 + depth(l) max depth(r)
}

def depth2[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 0, 1 + _ max _)




def map[A, B](t: Tree[A])(implicit f: A => B): Tree[B] = t match {
  case Leaf(a) => Leaf(f(a))
  case Branch(l, r) => Branch(map(l), map(r))
}

def map2[A, B](t: Tree[A])(implicit f: A => B): Tree[B] = fold[A, Tree[B]](t)(a => Leaf(f(a)), Branch(_, _))




val aTree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

size(aTree)
size2(aTree)
size(aTree) == size2(aTree)

maximum(aTree)
maximum2(aTree)
maximum(aTree) == maximum2(aTree)

depth(aTree)
depth2(aTree)
depth(aTree) == depth2(aTree)

val x3: Int => Int = _ * 3
map(aTree)(x3)
map2(aTree)(x3)
map(aTree)(x3) == map2(aTree)(x3)

