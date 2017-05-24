case class Some_[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some_(a) => Some_(f(a))
  }

  def flatMap_[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some_(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some_(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f(_)).getOrElse(None)

}

val one = Some_(1)
val world = Some_("world")

val onePerLength: (String => Option[Double]) = str =>
  if (str.isEmpty) None
  else Some_(1.0 / str.size)

one.map(_ + 2)
world.map("Hello, " + _)
val noneInt: Option[Int] = None
noneInt.map(_ + 2)

one.flatMap(i => Some_("string: " + (i * 2)))
world.flatMap(onePerLength)
Some_("").flatMap(onePerLength)
val noneString: Option[String] = None
noneString.flatMap(s => Some_(s.length))

world.getOrElse("...")
None.getOrElse("...")

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some_(xs.sum / xs.length)

mean(List(1, 2, 3))
mean(List())

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

variance(List(1, 2, 3))
variance(List())

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(aa => b.map(f(aa, _)))

def aMethod(i: Int, s: String): Double = s.length * i
map2(Some_(1), Some_("None"))(aMethod)
map2(None, Some_("None"))(aMethod)
map2(Some_(1), None)(aMethod)
map2(None, None)(aMethod)

def sequence[A](l: List[Option[A]]): Option[List[A]] =
  l.foldLeft[Option[List[A]]](Some_(List.empty[A]))(map2(_, _)((t, h) =>
    h :: t))

sequence(List())
sequence(List(None))
sequence(List(Some_(1)))
sequence(List(Some_(1), Some_(1), Some_(1)))
sequence(List(Some_(1), None, Some_(1)))

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as match {
    case List() => Some_(List())
    case a :: tail =>
      f(a) match {
        case None => None
        case Some_(b) =>
          map2[B, List[B], List[B]](Some_(b), traverse(tail)(f))(_ :: _)
      }
  }

def Try[A](a: => A): Option[A] =
  try Some_(a)
  catch {
    case e: Exception => None
  }

def parse(str: String): Option[Int] = Try(str.toInt)
traverse(List("1", "a"))(parse)
traverse(List("1", "2"))(parse)

def sequenceWithTraverse[A](l: List[Option[A]]): Option[List[A]] =
  traverse[Option[A], A](l)(identity)

sequenceWithTraverse(List())
sequenceWithTraverse(List(None))
sequenceWithTraverse(List(Some_(1)))
sequenceWithTraverse(List(Some_(1), Some_(1), Some_(1)))
sequenceWithTraverse(List(Some_(1), None, Some_(1)))
