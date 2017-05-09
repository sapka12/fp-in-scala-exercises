import scala.util.{Success, Try}

case class Name(val value: String)
case class Age(val value: Int)
case class Person(val name: Name, val age: Age)

sealed trait Validator[+E, +A]
case class Valid[E, A](val a: A) extends Validator[E, A]
case class Invalid[E, A](es: List[E]) extends Validator[List[E], A]

def map2[E, A, B, C](a: Validator[List[E], A], b: Validator[List[E], B])(f: (A, B) => C): Validator[List[E], C] = {
  a match  {
    case Invalid(aErrors) => b match {
      case Invalid(bErrors) => Invalid(aErrors ::: bErrors)
      case _ => Invalid(aErrors)
    }
    case Valid(aa) => b match {
      case Valid(bb) => Valid[List[E], C](f(aa, bb))
      case Invalid(e) => Invalid[E, C](e)
    }
  }
}

def mkName(name: String): Validator[List[String], Name] =
  if (name.isEmpty) Invalid(List("name should be nonEmpty"))
  else Valid[List[String], Name](new Name(name))

def mkAge(age: String): Validator[List[String], Age] = Try{
  age.toInt
} match {
  case Success(i) => Valid(new Age(i))
  case _ => Invalid(List("age should be parsed to Int"))
}

mkAge("1")
mkAge("")
mkAge("a")

def mkPerson(name: String, age: String): Validator[List[String], Person] =
  map2(mkName(name), mkAge(age))(Person(_, _))

mkPerson("", "")
mkPerson("a", "")
mkPerson("", "1")
mkPerson("a", "1")