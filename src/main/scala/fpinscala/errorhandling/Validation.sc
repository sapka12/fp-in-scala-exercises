sealed trait Validator[+E, +A] {

  def map[B](f: A => B): Validator[E, B] = this match {
    case Invalid(e) => Invalid(e)
    case Valid(a) => Valid(f(a))
  }

  def orElse[EE >: E, B >: A](b: => Validator[EE, B]): Validator[EE, B] =
    this match {
      case Valid(a) => Valid(a)
      case Invalid(e) => b match {
        case Invalid(ee) => Invalid(e ::: ee)
        case _ => Invalid(e)
      }
    }

  def map2[EE >: E, B, C](b: Validator[EE, B])(f: (A, B) => C): Validator[EE, C] = {
    this match {
      case Invalid(aErrors) => b match {
        case Invalid(bErrors) => Invalid(aErrors ::: bErrors)
        case _ => Invalid(aErrors)
      }
      case Valid(aa) => b match {
        case Valid(bb) => Valid[C](f(aa, bb))
        case Invalid(e) => Invalid[EE](e)
      }
    }
  }
}

case class Valid[+A](val a: A) extends Validator[Nothing, A]

case class Invalid[+E](e: List[E]) extends Validator[E, Nothing]

def traverse[E, A, B](as: List[A])(implicit f: A => Validator[E, B]): Validator[E, List[B]] =
  as.foldLeft[Validator[E, List[B]]](Valid(List.empty[B])) {
    (v, a) => v.map2(f(a))(_ ::: List(_))
  }

def sequence[E, A](es: List[Validator[E, A]]): Validator[E, List[A]] =
  es.foldLeft[Validator[E, List[A]]](Valid(List.empty[A]))(
    _.map2(_)(_ ::: List(_)))

def add(a: Int, b: Int): Int = a + b
def reciprocal(i: Int): Validator[String, Double] =
  if (i == 0) Invalid(List("Err */0"))
  else Valid(1.0 / i)

Valid(1).map(_ + 1)
Invalid(List(1)).map(add(_, 1))

Valid(1).orElse(Valid(2))
Invalid(List("err1")).orElse(Valid(2))
Invalid(List("err1")).orElse(Invalid(List("err2")))
Valid(1).orElse(Invalid(List("err1")))

Valid(1).map2(Valid(2))(add)
Valid(1).map2(Invalid(List("err2")))(add)
Invalid(List("err1")).map2(Valid(2))(add)
Invalid(List("err1")).map2(Invalid(List("err2")))(add)

traverse(List(1, 2, 3))(reciprocal)
traverse(List(0, 2, 0))(reciprocal)

sequence(List(Valid(0), Valid(1), Valid(2)))
sequence(List(Valid(0), Invalid(List("1-1", "1-2")), Invalid(List("2"))))