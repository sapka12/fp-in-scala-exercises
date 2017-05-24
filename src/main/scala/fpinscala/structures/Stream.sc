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

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)(p(_) && _)

  def headOption: Option[A] = this.foldRight[Option[A]](None)(
    (a, _) => Some(a)
  )

  def map[B](f: A => B): Stream[B] = this.foldRight[Stream[B]](Empty)(
    (a, b) => cons(f(a), b)
  )

  def mapWithUnfold[B](f: A => B): Stream[B] = unfold[B, Stream[A]](this)(_ match {
    case Cons(a, tail) => Some((f(a()), tail()))
    case _ => None
  })

  def filter(f: A => Boolean): Stream[A] = this.foldRight[Stream[A]](Empty)(
    (a, b) => if (f(a)) cons(a, b) else b
  )

  def append[B >: A](s: => Stream[B]): Stream[B] = this.foldRight[Stream[B]](s)(
    (a, b) => cons(a, b)
  )

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight[Stream[B]](Empty)(
    (a, b) => f(a).append(b)
  )

}

Stream(1, 2, 3, 4).toList

Stream().take(2).toList
Stream(1, 2).take(0).toList
Stream(1, 2, 3, 4).take(2).toList
Stream(1, 2, 3, 4).take(8).toList

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