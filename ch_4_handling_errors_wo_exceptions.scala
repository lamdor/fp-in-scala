object ch4_option {
  def failingFn(i: Int): Int = {
    val x: Int = throw new Exception("fail!")
    try {
      val y = 42 + 5
      y + x
    } catch { case e: Exception => 43 }
  }

  sealed trait Option[+A] {
    // exercise 1
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None    => None
    }
    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f) getOrElse None
    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None    => default
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(_ => this) getOrElse ob
    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if(f(a)) this else None)
    def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // exercise 2
  def variance(xs: Seq[Double]): Option[Double] = {
    val maybeMean: Option[Double] = mean(xs)
    val maybeDifferences: Option[Seq[Double]] =
      maybeMean.map(mu => xs.map(x => math.pow(x - mu, 2)))
    maybeDifferences.flatMap(mean)
  }

  import java.util.regex._
  def pattern(s: String): Option[Pattern] =
    try Some(Pattern.compile(s))
    catch { case e: PatternSyntaxException => None }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches )

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat).flatMap { f =>
      mkMatcher(pat2) map { g =>
        f(s) && g(s)
      }
    }

  object Option {
    // exercise 3
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
      for {
        a_ <- a
        b_ <- b
      } yield f(a_,b_)

    // exercise 5
    def sequence[A](as: List[Option[A]]): Option[List[A]] = {
      @annotation.tailrec
      def go(as: List[Option[A]], soFar: List[A] = Nil): Option[List[A]] = as match {
        case head :: tail => head match {
          case None => None
          case Some(a) => go(tail, soFar :+ a)
        }
        case Nil => Some(soFar)
      }
      go(as)
    }

    // exercise 6
    def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
      @annotation.tailrec
      def go(as: List[A], soFar: List[B] = Nil): Option[List[B]] = as match {
        case head :: tail => f(head) match {
          case None => None
          case Some(b) => go(tail, soFar :+ b)
        }
        case Nil => Some(soFar)
      }
      go(as)
    }
    def sequence_2[A](as: List[Option[A]]): Option[List[A]] =
      traverse(as)(identity)
  }

  // exercise 4
  def bothMatch_4(pat: String, pat2: String, s: String): Option[Boolean] =
    Option.map2(mkMatcher(pat), mkMatcher(pat2)) { (f,g) =>
      f(s) && g(s)
    }
}

object ch4_either {
  sealed trait Either[+E,+A] {
    // exercise 7
    def map[B](f: A => B): Either[E,B] = this match {
      case l: Left[_] => l
      case Right(a) => Right(f(a))
    }
    def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] = this match {
      case l: Left[_] => l
      case Right(a) => f(a)
    }
    def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE,B] = this match {
      case l: Left[_] => b
      case r: Right[_] => r
    }
    def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
      for {
        a  <- this
        b_ <- b
      } yield f(a,b_)
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try
      Right(x/y)
    catch {
      case e: Exception => Left(e)
    }

  object Either {
    // exercise 8
    def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
      @annotation.tailrec
      def go(as: List[A], soFar: List[B] = Nil): Either[E, List[B]] = as match {
        case head :: tail => f(head) match {
          case l: Left[_] => l
          case Right(b) => go(tail, soFar :+ b)
        }
        case Nil => Right(soFar)
      }
      go(as)
    }
    def sequence[E,A](as: List[Either[E,A]]): Either[E,List[A]] =
      traverse(as)(identity)
  }

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person.apply)

  // exercise 9
  // really is like the scalaz Validation (but we're going to just cut corners)
  sealed trait Validation[+E, +A] {
    def map[B](f: A => B): Validation[E,B] = this match {
      case f: Failure[_] => f
      case Success(a)    => Success(f(a))
    }
    def flatMap[EE >: E, B](f: A => Validation[EE,B]): Validation[EE,B] = this match {
      case f: Failure[_] => f
      case Success(a)    => f(a)
    }
    def orElse[EE >: E, B >: A](b: => Validation[EE,B]): Validation[EE,B] = this match {
      case f: Failure[_] => f
      case r: Success[_] => r
    }
    def map2[EE >: E, B, C](b: Validation[EE,B])(f: (A,B) => C): Validation[EE,C] = (this, b) match {
      case (Failure(e1), Failure(e2))       => Failure(e1 ++ e2)
      case (f1: Failure[_], s2: Success[_]) => f1
      case (s1: Success[_], f2: Failure[_]) => f2
      case (Success(a), Success(b))         => Success(f(a,b))
    }
  }
  case class Failure[E](failures: List[E]) extends Validation[E, Nothing]
  case class Success[A](value: A) extends Validation[Nothing, A]
  object Validation {
    def traverse[E,A,B](as: List[A])(f: A => Validation[E,B]): Validation[E, List[B]] = {
      @annotation.tailrec
      def go(as: List[A], soFar: Validation[E, List[B]] = Success(Nil)): Validation[E, List[B]] =
        as match {
          case head :: tail => f(head) match {
            case f1@Failure(fs1) => soFar match {
              case Failure(fs2) => go(tail, Failure(fs1 ++ fs2))
              case _            => go(tail, f1)
            }
            case Success(a) => soFar match {
              case f: Failure[_] => go(tail, f)
              case Success(as)   => go(tail, Success(as :+ a))
            }
          }
          case Nil => soFar
        }
      go(as)
    }
    def sequence[E,A](as: List[Validation[E,A]]): Validation[E,List[A]] =
      traverse(as)(identity)
  }
  def mkName_1(name: String): Validation[String, Name] =
    if (name == "" || name == null) Failure(List("Name is empty."))
    else Success(new Name(name))
  def mkAge_1(age: Int): Validation[String, Age] =
    if (age < 0) Failure(List("Age is out of range."))
    else Success(new Age(age))
  def mkPerson_1(name: String, age: Int): Validation[String, Person] =
    mkName_1(name).map2(mkAge_1(age))(Person.apply)
}

