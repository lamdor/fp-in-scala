import scala.util.matching.Regex

object ch9 {
  case class Location(input: String, offset: Int = 0) {
    lazy val upto = input.slice(0, offset + 1)
    lazy val line = upto.count(_ == '\n') + 1
    lazy val col  = upto.reverse.indexOf('\n')

    def advanceBy(chars: Int) = copy(input.drop(chars), offset + chars)
    def toError(msg: String) =
      ParseError(List((this, msg)))
  }
  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError =
      copy(stack = (loc, msg) :: stack)
    def label(msg: String) =
      ParseError(latestLoc.map((_, msg)).toList)
    def latestLoc: Option[Location] = latest map (_._1)
    def latest: Option[(Location,String)] = stack.lastOption
  }

  trait Parsers[Parser[+_]] { self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def slice[A](p: Parser[A]): Parser[String]

    def succeed[A](a: A): Parser[A] //= string("") map (_ => a)
    def fail: Parser[Nothing]

    def attempt[A](p: Parser[A]): Parser[A]

    def label[A](msg: String)(p: Parser[A]): Parser[A]
    def scope[A](msg: String)(p: Parser[A]): Parser[A]
    def errorLocation(e: ParseError): Location
    def errorMessage(e: ParseError): String

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
      for { a <- p1; b <- p2 } yield (a,b)
    
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n > 0) {
        map2(p, listOfN(n - 1, p))(_ :: _)
      } else {
        succeed(Nil)
      }

    def many[A](p: Parser[A]): Parser[List[A]] =
      or(map2(p, many(p))(_ :: _), succeed(Nil))
      // map2(p, or(many(p), succeed(Nil)))(_ :: _) // my impl

    def many1[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _)

    def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    def map[A,B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(f andThen succeed)
    def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      flatMap(p1)(a => map(p2)(b => f(a,b)))
    
    implicit def int(i: Int): Parser[Int] = string(i.toString).map(_.toInt)
    implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
    implicit def string(s: String): Parser[String]
    implicit def regex(r: Regex): Parser[String]
    implicit def intToParserOps(a: Int)(implicit f: Int => Parser[Int]) = 
        ParserOps(f(a))
    implicit def charToParserOps(a: Char)(implicit f: Char => Parser[Char]) = 
        ParserOps(f(a))
    implicit def stringToParserOps(a: String)(implicit f: String => Parser[String]) = 
        ParserOps(f(a))
    implicit def regexToParserOps(a: Regex)(implicit f: Regex => Parser[String]) = 
        ParserOps(f(a))

    implicit class ParserOps[A](p: Parser[A]) {
      def run(str: String) = self.run(p)(str)

      def attempt = self.attempt(p)
      
      def slice = self.slice(p)
      
      def label(msg: String) = self.label(msg)(p)
      def scope(msg: String) = self.scope(msg)(p)

      def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

      def **[B](p2: => Parser[B]) = self.product(p, p2)
      def product[B](p2: => Parser[B]) = self.product(p, p2)
      
      def many = self.many(p)
      def many1 = self.many1(p)

      def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
      def map[B](f: A => B) = self.map(p)(f)

      def map2[B,C](p2: => Parser[B])(f: (A,B) => C) = self.map2(p, p2)(f)
    }

    object Laws {
      import ch8._

      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

      def attemptLaw[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]) =
        equal(
          attempt(p1.flatMap(_ => fail)) or p2,
          p2
        )(in)

      def succeedLaw[A](a: A)(in: Gen[String]) =
        Prop.forAll(in)(s => run(succeed(a))(s) == Right(a))

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.map(identity))(in)

      def flatMapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.flatMap(succeed))(in)

      def charLaw(chP: Parser[Char])(in: Gen[String]) = {
        val strP = chP.map(_.toString)
        val strPToChar = strP.map(_.charAt(0))
        equal(chP, strPToChar)(in)
      }

      def productLaw_identity[A](p1: Parser[A])(in: Gen[String]): Prop = {
        val productL = succeed(()) ** p1
        val expectedL = p1.map(r => ((), r))
        val productR = p1 ** succeed(())
        val expectedR = p1.map(r => (r, ()))
        equal(productR, expectedR)(in)  && equal(productR, expectedR)(in)
      }
        
      def productLaw_swapped[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop = {
        val productL = p1 ** p2
        val productR = p2 ** p1
        equal(
          productL,
          productR.map(_.swap)
        )(in)
      }

      def labelLaw[A](p: Parser[A], inputs: Gen[String]): Prop =
        Prop.forAll(inputs ** Gen.string) { case (input, msg) =>
          run(label(msg)(p))(input) match {
            case Left(e) => errorMessage(e) == msg
            case _       => true
          }
        }
    }
  }

  sealed trait JSON
  object JSON {
    case object JSNull extends JSON
    case class JNumber(get: BigDecimal) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }

  import JSON._

  lazy val testJSON: JSON =
    JObject(Map("test" ->
                  JArray(Vector(JNumber(1),
                                JString("two"),
                                JObject(Map("key" -> JBool(false)))))))

  def show(js: JSON): String = js match {
    case JSNull        => "null"
    case JNumber(num) => num.toString
    case JString(str) => "\"" + str + "\""
    case JBool(bool)   => bool.toString
    case JArray(jss)   => "[" + jss.map(show).mkString(",") + "]"
    case JObject(kvs) =>
      def showKV(kv: Pair[String, JSON]) = {
        val (key, value) = kv
        "\"" + key + "\"" + ":" + show(value)
      }
      "{" + kvs.map(showKV).mkString(",") + "}"
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    val jNumber = regex("[0-9\\.]+".r).map(num => JNumber(BigDecimal(num)))
    val str = char('"') ** regex("[^\"]+".r) ** char('"') map {
      case ((_, str), _) => str
    }
    val jString = str map JString
    val jBool = (string("true") | string("false")) map { b => 
      JBool(b.toBoolean)
    }
    lazy val jArray = char('[') ** (json.many | spaces) ** char(']') map {
      case ((_, jss: List[JSON]), _) => JArray(jss.toIndexedSeq)
      case ((_, _), _)               => JArray(Vector.empty)
    }
    lazy val jObjectPair = spaces ** str ** spaces ** char(':') ** spaces ** json ** spaces map {
      case (((((_, key), _), _), json), _) => key -> json
    }
    lazy val jObject = char('{') ** jObjectPair.many ** char('}') map { 
      case ((_, pairs), _) => JObject(pairs.toMap)
    }
    lazy val json: Parser[JSON] = jNumber | jString | jBool | jArray

    json
  }

  object JSONParsingProperties {
    import JSONGen._
    import ch8._

    lazy val reversableLaw = Prop.forAll(jsonGen) { js =>
      val stringParsers: Parsers[StringParser] = StringParsers
      val parserForJson = jsonParser(stringParsers)
      stringParsers.run(parserForJson)(show(js)) == Right(js)
    }
  }

  object JSONGen {
    import JSON._
    import ch8._

    lazy val jNumberGen: Gen[JNumber] = Gen.bigDecimal.map(JNumber)
    lazy val jStringGen: Gen[JString] = Gen.string.map(JString)
    lazy val jBoolGen: Gen[JBool]     = Gen.boolean.map(JBool)

    lazy val jArrayGen: Gen[JArray]   =
      for {
        i <- Gen.choose(0,10)
        jss <- Gen.listOfN(i, jsonGen)
      } yield JArray(jss.toIndexedSeq)

    lazy val jObjectGen: Gen[JObject] =
      for {
        i <- Gen.choose(0,10)
        kvs <- Gen.listOfN(i, Gen.string ** jsonGen)
      } yield JObject(kvs.toMap)

    lazy val jsonGen: Gen[JSON] =
      Gen.weighted(
        jNumberGen -> 1.0/5,
        Gen.weighted(
          jStringGen -> 1.0/4,
          Gen.weighted(
            jBoolGen -> 2.0/3,
            Gen.union(
              jArrayGen,
              jObjectGen
            ) -> 1.0/3
          ) -> 3.0/4
        ) -> 4.0/5
      )
  }

  type StringParser[+A] = Location => Result[A]

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(error, commited) => Failure(f(error), commited)
      case s => s
    }
    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _                => this
    }
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, false) if isCommitted => Failure(e, true)
      case _                                => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError,
                     isCommitted: Boolean = true) extends Result[Nothing]

  object StringParsers extends Parsers[StringParser] {
    def run[A](p: StringParser[A])(input: String): Either[ParseError,A] =
      p(Location(input)) match {
        case Success(a, _)  => Right(a)
        case Failure(error, _) => Left(error)
      }

    implicit def string(s: String): StringParser[String] = { (l: Location) =>
      if (l.input.startsWith(s))
        Success(s, s.length)
      else
        Failure(l.toError("Expected: " + s))
    }
    implicit def regex(r: Regex): StringParser[String] = { (l: Location) =>
      r.findPrefixOf(l.input) match {
        case Some(prefix) => Success(prefix, prefix.length)
        case None         => Failure(l.toError(s"Expected: '${r.toString}'"))
      }
    }
    def slice[A](p: StringParser[A]): StringParser[String] = { (l: Location) =>
      p(l) match {
        case Success(_, charsConsumed) =>
          Success(l.input.take(charsConsumed), charsConsumed)
        case f: Failure => f
      }
    }

    def flatMap[A, B](p: StringParser[A])(f: A => StringParser[B]): StringParser[B] = {
      (l: Location) =>
      p(l) match {
        case Success(a, charsConsumed) =>
          val newL = l.advanceBy(charsConsumed)
          f(a).apply(newL).addCommit(charsConsumed == 0)
        case f: Failure => f
      }
    }

    def succeed[A](a: A): StringParser[A] = { (l: Location) =>
      Success(a, 0)
    }
    def fail: StringParser[Nothing] = { (l: Location) =>
      Failure(l.toError("Failure"))
    }

    def attempt[A](p: StringParser[A]): StringParser[A] = { l =>
      p(l).uncommit
    }
    def or[A](p1: StringParser[A], p2: => StringParser[A]): StringParser[A] = { l =>
      p1(l) match {
        case r@Failure(e, committed) if committed => p2(l)
        case r                                    => r
      }
    }

    def errorLocation(e: ParseError): Location = ???
    def errorMessage(e: ParseError): String = ???

    def scope[A](msg: String)(p: StringParser[A]): StringParser[A] =
      l => p(l).mapError(_.push(l, msg))
    def label[A](msg: String)(p: StringParser[A]): StringParser[A] =
      l => p(l).mapError(_.label(msg))
  }
}
