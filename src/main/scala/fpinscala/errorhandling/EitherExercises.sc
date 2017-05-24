case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatten[EE >: E, B](either: Either[EE, Either[EE, B]]): Either[EE, B] =
    either match {
      case Right(b) => b
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    flatten(this.map(f))
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => Right(a)
      case Left(e) => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)

  // def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]]

}

val toStr: Int => String = _.toString

Right(1).map(_.toString)
Left("ERR404").map((b: Boolean) => if (b) 1.0 else 0)

val bt0: Int => Either[String, Double] = i =>
  if (i < 1) Left("Must be bigger than 0")
  else Right(1.0 / i)

Right(1).flatMap(bt0)
Right(-1).flatMap(bt0)
Left("ERR").flatMap(bt0)

Right(1).orElse(Right(2))
Left(1).orElse(Right(2))
Left(1).orElse(Left(2))

Right(1).map2(Right("one"))(_.toString + ".." + _)
Left("Err").map2(Right("one"))(_.toString + ".." + _)
Right(1).map2(Left(":("))(_.toString + ".." + _)

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
  type AA = Either[E, A]
  type BB = Either[E, List[A]]

  val z: BB = Right(List.empty[A])
  val f: (BB, AA) => BB = _.map2(_)(_ ::: List(_))

  es.foldLeft[BB](z)(f)
}

def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  es.foldLeft[Either[E, List[A]]](Right(List.empty[A]))(
    _.map2(_)(_ ::: List(_)))

sequence[String, Int](List(Right(1), Right(2), Right(3)))
sequence[String, Int](List(Right(1), Left("ERR"), Right(3)))

sequence2[String, Int](List(Right(1), Right(2), Right(3)))
sequence2[String, Int](List(Right(1), Left("ERR"), Right(3)))

def traverse[E, A, B](as: List[A])(implicit f: A => Either[E, B]): Either[E, List[B]] =
  as.foldLeft[Either[E, List[B]]](Right(List.empty[B])) {
    (either, a) => f(a).map2(either)(_ :: _)
  }

def reciprocal(i: Int): Either[String, Double] =
  if (i == 0) Left("ERR")
  else Right(1.0 / i)

def traverseReciprocal(is: List[Int]): Either[String, List[Double]] =
  traverse[String, Int, Double](is)(reciprocal)

traverseReciprocal(List(3, 2, 1, 0))
traverseReciprocal(List(3, 2, 1))
traverseReciprocal(List())