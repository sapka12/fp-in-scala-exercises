import Stream._

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
  case None => Empty
  case Some((a, s)) => Stream.cons(a, unfold(s)(f))
}

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}

sealed trait Stream[+A] {

  def toList: List[A] = foldRight[List[A]](List.empty[A])(_ :: _)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def take(n: Int): Stream[A] = {

    def go(as: Stream[A], i: Int): Stream[A] =
      if (i < 1) Empty
      else as match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => go(t(), i - 1))
      }

    go(this, n)
  }

  def drop(n: Int): Stream[A] = {

    def go(as: Stream[A], i: Int): Stream[A] =
      if (i < 1) as
      else as match {
        case Empty => Empty
        case Cons(_, t) => go(t(), i - 1)
      }

    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileWithFold(p: A => Boolean): Stream[A] = this.foldRight[Stream[A]](empty)((a, b) =>
    if (p(a)) cons(a, b)
    else empty
  )

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = unfold[A, Stream[A]](this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)(p(_) && _)

  def headOption: Option[A] = this.foldRight[Option[A]](None)(
    (a, _) => Some(a)
  )

  def map[B](f: A => B): Stream[B] = this.foldRight[Stream[B]](Empty)(
    (a, b) => cons(f(a), b)
  )

  def mapWithUnfold[B](f: A => B): Stream[B] = unfold[B, Stream[A]](this) {
    case Cons(a, tail) => Some((f(a()), tail()))
    case _ => None
  }

  def takeWithUnfold(n: Int): Stream[A] = unfold[A, (Stream[A], Int)]((this, n)) {
    case (Cons(h, tail), takeN) if takeN > 0 => Some((h(), (tail(), takeN - 1)))
    case _ => None
  }

  def filter(f: A => Boolean): Stream[A] = this.foldRight[Stream[A]](Empty)(
    (a, b) => if (f(a)) cons(a, b) else b
  )

  def append[B >: A](s: => Stream[B]): Stream[B] = this.foldRight[Stream[B]](s)(
    (a, b) => cons(a, b)
  )

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight[Stream[B]](Empty)(
    (a, b) => f(a).append(b)
  )

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold[C, (Stream[A], Stream[B])](this, bs) {
      case (Cons(aH, aT), Cons(bH, bT)) => Some(f(aH(), bH()), (aT(), bT()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this, s2) {
    case (Cons(a, as), Cons(b, bs)) => Some(
      (Some(a()), Some(b())),
      (as(), bs())
    )
    case (Cons(a, as), Empty) => Some(
      (Some(a()), None),
      (as(), Empty)
    )
    case (Empty, Cons(b, bs)) => Some(
      (None, Some(b())),
      (Empty, bs())
    )
    case _ => None
  }

  def startsWith[A](s: Stream[A]): Boolean = this.zipAll(s).takeWhile {
    case (_, Some(_)) => true
    case _ => false
  }.forAll {
    case (a, b) => a == b
  }


  def tails: Stream[Stream[A]] = unfold(this) {
    case s @ Cons(_, t) => Some((s, t()))
    case Empty => None
  } append (Stream(empty[A]))

  //TODO make it more efficient
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails.map(_.foldRight(z)(f))
}

Stream(1, 2, 3).scanRight(0)(_ + _).toList

Stream(1, 2, 3, 4).toList

Stream().take(2).toList
Stream(1, 2).take(0).toList
Stream(1, 2, 3, 4).take(2).toList
Stream(1, 2, 3, 4).take(8).toList

Stream().takeWithUnfold(2).toList
Stream(1, 2).takeWithUnfold(0).toList
Stream(1, 2, 3, 4).takeWithUnfold(2).toList
Stream(1, 2, 3, 4).takeWithUnfold(8).toList

Stream().drop(2).toList
Stream(1, 2).drop(0).toList
Stream(1, 2, 3, 4).drop(2).toList
Stream(1, 2, 3, 4).drop(8).toList

Stream.empty[Int].takeWhile(_ < 3).toList
Stream(1, 2, 3, 4).takeWhile(_ < 3).toList
Stream(1, 2, 3, 4).takeWhile(_ > 3).toList

Stream.empty[Int].takeWhileWithFold(_ < 3).toList
Stream(1, 2, 3, 4).takeWhileWithFold(_ < 3).toList
Stream(1, 2, 3, 4).takeWhileWithFold(_ > 3).toList

Stream.empty[Int].takeWhileWithUnfold(_ < 3).toList
Stream(1, 2, 3, 4).takeWhileWithUnfold(_ < 3).toList
Stream(1, 2, 3, 4).takeWhileWithUnfold(_ > 3).toList

Stream(1, 2, 3, 4).forAll(_ < 3)
Stream(1, 2, 3, 4).forAll(_ < 5)
Stream.empty[Int].forAll(_ < 3)
Stream.empty[Int].forAll(_ > 3)

Stream(1, 2, 3).headOption
Stream().headOption

Stream(1, 2, 3, 4).map(_ * 3).toList
Stream(1, 2, 3, 4).mapWithUnfold(_ * 3).toList

Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList

Stream(1, 2, 3, 4).append(Stream(5, 6)).toList

Stream(Stream(1, 2), Stream(3), Stream(), Stream(4)).flatMap(s => s.map(_ * 2)).toList

val ones: Stream[Int] = Stream.cons(1, ones)
def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
def fibs(n: Int, next: Int): Stream[Int] = Stream.cons(n, fibs(next, n + next))

def onesWithUnfold: Stream[Int] = unfold[Int, Unit](())(_ => Some((1, ())))
def constantWithUnfold[A](a: A): Stream[A] = unfold[A, Unit](())(_ => Some((a, ())))
def fromWithUnfold(n: Int): Stream[Int] = unfold[Int, Int](n)(s => Some((s, s + 1)))

def fibWithUnfold: Stream[Int] = unfold[Int, (Int, Int)]((0, 1)) { n =>
  Some(
    n._1,
    (n._2, n._1 + n._2)
  )
}

constant(2).drop(3).take(2).toList
from(1).drop(3).take(2).toList
fibs(0, 1).take(10).toList

fibWithUnfold.take(10).toList
constantWithUnfold(2).drop(3).take(2).toList
fromWithUnfold(10).drop(3).take(5).toList
onesWithUnfold.drop(3).take(2).toList

def lengthSum(d: Double, str: String): Int = str.length + d.toString.length

Stream(1).zipWith(Stream(2))(_ + _).toList
Stream(1).zipWith(empty[Int])(_ + _).toList

Stream(1.3, 2.45).zipWith(Stream("123", "1234"))(lengthSum).toList
Stream(1.3).zipWith(Stream("123", "1234"))(lengthSum).toList
Stream(1.3, 2.45).zipWith(Stream("123"))(lengthSum).toList
Stream().zipWith(Stream("123", "1234"))(lengthSum).toList
Stream(1.3, 2.45).zipWith(Stream())(lengthSum).toList

Stream(1).zipWith(Stream(2))(_ + _).toList
Stream(1).zipWith(empty[Int])(_ + _).toList

Stream(1.3, 2.45).zipAll(Stream("123", "1234")).toList
Stream(1.3).zipAll(Stream("123", "1234")).toList
Stream(1.3, 2.45).zipAll(Stream("123")).toList
Stream().zipAll(Stream("123", "1234")).toList
Stream(1.3, 2.45).zipAll(Stream()).toList


Stream(1, 2, 3, 4).startsWith(Stream())
Stream(1, 2, 3, 4).startsWith(Stream(1))

Stream(1, 2, 3, 4).startsWith(Stream(1, 2))
Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4))
Stream(1, 2, 3, 4).startsWith(Stream(2))

empty[Int].startsWith(Stream(1))
Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4, 5))

ones.startsWith(Stream(1, 1, 1))

Stream(1, 2, 3, 4).tails.toList.map(_.toList)
empty[Int].tails.toList.map(_.toList)