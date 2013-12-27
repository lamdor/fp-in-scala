object ch10 {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(s1: String, s2: String) = s1 + s2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(l1: List[A], l2: List[A]) = l1 ++ l2
    val zero = Nil
  }

  // exercise 1
  val intAddition = new Monoid[Int] {
    def op(i1: Int, i2: Int) = i1 + i2
    val zero = 0
  }
  val intMultiplication = new Monoid[Int] {
    def op(i1: Int, i2: Int) = i1 * i2
    val zero = 1
  }
  val booleanOr = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean) = b1 || b2
    val zero = false
  }
  val booleanAnd = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean) = b1 && b2
    val zero = true
  }

  // exercise 2
  def optionMonoid[A : Monoid] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]) =
      o1 map { a1 =>
        o2 map { a2 =>
          implicitly[Monoid[A]].op(a1, a2)
        } getOrElse a1
      } orElse o2

    val zero = None
  }

  // exercise 3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A) =
      f1 andThen f2
    def zero = identity
  }

  // exercise 4
  import ch8._
  import Gen._
  import SGen._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]) : Prop = {
    forAll(gen ** gen ** gen) { // binary associativy
      case ((a,b), c) =>
        m.op(a, m.op(b,c)) == m.op(m.op(a,b), c)
    } &&
    forAll(gen) { a => // identity
      m.op(a, m.zero) == a &&
      m.op(m.zero, a) == a
    }
  }
  val stringMonoidLaws = monoidLaws(stringMonoid, Gen.string)
  val listMonoidLaws = monoidLaws(listMonoid[Int], SGen.listOf(Gen.int))
  val intAdditionMonoidLaws = monoidLaws(intAddition, Gen.int)
  val intMultiplicationMonoidLaws = monoidLaws(intMultiplication, Gen.int)
  val booleanOrMonoidLaws = monoidLaws(booleanOr, Gen.boolean)
  val booleanAndMonoidLaws = monoidLaws(booleanAnd, Gen.boolean)
  val optionMonoidLaws = monoidLaws(optionMonoid(intAddition), Gen.option(Gen.int))
  // how to check equality of functions... we have to call them
  // val endoFunctionMonoidLaws = monoidLaws(endoMonoid[Int], Gen.endoIntFunction)
  val endoFunctionMonoidLaws = {
    val genI = Gen.int
    val gen = Gen.endoIntFunction
    val m = endoMonoid[Int]
    forAll(gen ** gen ** gen ** genI) { // binary associativy
      case (((a,b), c), i) =>
        m.op(a, m.op(b,c))(i) == m.op(m.op(a,b), c)(i)
    } &&
    forAll(gen ** genI) { case (a,i) => // identity
      m.op(a, m.zero)(i) == a(i) &&
      m.op(m.zero, a)(i) == a(i)
    }
  }
  def runAllMonoidLaws: Unit = {
    List(
      stringMonoidLaws,
      listMonoidLaws,
      intAdditionMonoidLaws,
      intMultiplicationMonoidLaws,
      booleanOrMonoidLaws,
      booleanAndMonoidLaws,
      optionMonoidLaws,
      endoFunctionMonoidLaws
    ).foreach(Prop.run(_))
  }

  val words = List("Hic", "Est", "index")
  val s = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
  val t = words.foldRight(stringMonoid.zero)(stringMonoid.op)

  // exercise  5
  val spacingTrimMonoid = new Monoid[String] {
    def op(s1: String, s2: String): String = {
      val s1Space =
        if (!s1.endsWith(" "))
          s1 + " "
        else
          s1
      s1Space + s2.trim
    }
      

    val zero = ""
  }
  assert(
    spacingTrimMonoid.op("Hic", spacingTrimMonoid.op("est ", "chorda ")) ==
      "Hic est chorda"
  )

  // exercise 6
  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // exercise 7
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((rb, a) => m.op(rb, f(a)))

  // exercise 8
  assert(foldLeftUsingFoldMap(List(1,2,3), 0)(_ + _) == 6)
  def foldLeftUsingFoldMap[A, B](as: List[A], zero: B)(f: (B,A) => B): B =
    foldMap(as, endoMonoid[B])(a => (b => f(b, a)))(zero)
  def foldRightUsingFoldMap[A, B](as: List[A], zero: B)(f: (A,B) => B): B =
    foldMap(as, endoMonoid[B])(a => (b => f(a, b)))(zero)

  // exercise 9
  assert(foldMapV(IndexedSeq(0,1,2), intAddition)(_ + 1) == 6)
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    v.splitAt(v.size / 2) match {
      case (IndexedSeq(), IndexedSeq())  => m.zero
      case (IndexedSeq(), IndexedSeq(a)) => f(a)
      case (left, right)                 =>
        m.op(
          foldMapV(left, m)(f),
          foldMapV(right, m)(f)
        )
    }
  }
  import ch7._
  def parMonoid[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(pa1: Par[A], pa2: Par[A]) =
      Par.flatMap(pa1) { a1 =>
        Par.map(pa2) { a2 =>
          m.op(a1, a2)
        }
      }

    val zero = Par.unit(m.zero)
  }
  def parFoldMap[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(as, parMonoid(m))(Par.asyncF(f))

  // exercise 10
  sealed trait Sorted
  case object Sorted extends Sorted
  case object Unsorted extends Sorted
  def minAll(is: Int*) = is.min
  def maxAll(is: Int*) = is.max
  type MinMax = Option[(Int, Int)]
  val minMaxSortedMonoid = new Monoid[(MinMax, Sorted)] {
    def op(is1: (MinMax, Sorted), is2: (MinMax, Sorted)) = (is1, is2) match {
      case ((None, s1), (None, s2)) => (None, sor(s1, s2))

      case ((None, s1), (Some((min2, max2)), s2)) => (Some(min2, max2), sor(s1, s2))
      case ((Some((min1, max1)), s1), (None, s2)) => (Some(min1, max1), sor(s1, s2))

      case ((Some((min1, max1)), s1), (Some((min2, max2)), s2)) =>
        if (s1 == Unsorted || s2 == Unsorted || max1 > min2) 
          (Some(minAll(min1, min2, max1, max2), maxAll(min1, min2, max1, max2)),
           Unsorted)
        else
          (Some(min1, max2), Sorted)
    }
    val zero = (None, Sorted)

    private[this] def sor(s1: Sorted, s2: Sorted) =
      if (s1 == Unsorted) Unsorted else s2
  }
  val rangeGen = (Gen.int ** Gen.int) map {
    case (a,b) => (math.min(a,b), math.max(a,b))
  }
  val minMaxGen = Gen.option(rangeGen)
  val sortedGen = Gen.boolean map (b => if (b) Sorted else Unsorted)
  val minMaxSortedGen = minMaxGen ** sortedGen
  val minMaxSortedMonoidLaws = monoidLaws(minMaxSortedMonoid, minMaxSortedGen)
  def isSortedUsingFoldMap(is: IndexedSeq[Int]): Boolean = 
    foldMapV(is, minMaxSortedMonoid)(i => (Some(i, i), Sorted))._2 == Sorted

  sealed trait WC {
    def totalCount: Int
  }
  case class Stub(chars: String) extends WC {
    def totalCount = 0
  }
  case class Part(lStub: String, words: Int, rStub: String) extends WC {
    def nonEmpty = lStub.nonEmpty || words > 0 || rStub.nonEmpty
    def totalCount =
      (if (lStub.nonEmpty) 1 else 0) + words + (if (rStub.nonEmpty) 1 else 0)
  }

  // exercise 11
  def mkPart(str: String): Part = {
    val words = " *\\w+ *".r.findAllIn(str).toSeq
    if (words.size >= 2) 
      Part(words.headOption.getOrElse(""),
           words.size - 2,
           words.lastOption.getOrElse(""))
    else
      Part(words.headOption.getOrElse(""),
           math.max(0, words.size - 2),
           "")
  }
  val wcMonoid = new Monoid[WC] {
    def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
      case (p1: Part, p2: Part) if p1.nonEmpty && p2.nonEmpty=>
        val joiningCount = 
          if (wordContinuation(p1.rStub, p2.lStub))
            1
          else
            (if (p1.rStub.nonEmpty) 1 else 0) + (if (p2.lStub.nonEmpty) 1 else 0)
        Part(p1.lStub, p1.words + p2.words + joiningCount, p2.rStub)
      case (p1: Part, p2: Part) if p1.nonEmpty => p1
      case (p1: Part, p2: Part) if p2.nonEmpty => p2
      case (p1: Part, s2: Stub) => op(p1, mkPart(s2.chars))
      case (s1: Stub, p2: Part) => op(mkPart(s1.chars), p2)
      case (s1: Stub, s2: Stub) => Stub(s1.chars + s2.chars)
    }
    def zero: WC = Stub("")

    private[this] def wordContinuation(str1: String, str2: String) =
      endsWithWordChar(str1) && startsWithWordChar(str2)

    private[this] val wordCharStart = "^\\w".r
    private[this] def startsWithWordChar(str: String) =
      wordCharStart.findFirstIn(str).isDefined

    private[this] val wordCharEnd = "\\w$".r
    private[this] def endsWithWordChar(str: String) =
      wordCharEnd.findFirstIn(str).isDefined
  }
  val stubGen = Gen.string map Stub.apply
  val partGen = (Gen.string ** Gen.int ** Gen.string) map {
    case ((l,w),r) => Part(l,w,r)
  }
  val wcGen: Gen[WC] = Gen.union(stubGen, partGen)
  val wcMonoidLaws = monoidLaws(wcMonoid, wcGen)

  // exercise 12
  @annotation.tailrec
  def makeSplits(str: String, splitSize: Int, soFar: IndexedSeq[String] = Vector.empty): IndexedSeq[String] = {
    if (str.isEmpty)
      soFar
    else
      makeSplits(str.drop(splitSize), splitSize, soFar :+ str.take(splitSize))
  }
  def wordCount(str: String, splitSize: Int = 10): Int = {
    val splits = makeSplits(str, splitSize)

    foldMapV(splits, wcMonoid)(mkPart).totalCount
  }
  def parWordCount(str: String, splitSize: Int = 10): Par[Int] = {
    val splits = makeSplits(str, splitSize)

    val parPart: Par[WC] = parFoldMap(splits, wcMonoid)(mkPart)
    Par.map(parPart)(_.totalCount)
  }

  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(zero: B)(f: (A,B) => B): B
    def foldLeft[A,B](as: F[A])(zero: B)(f: (B,A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
    // exercise 16
    def toList[A](as: F[A]): List[A] = foldRight(as)(List.empty[A])(_ :: _)
  }

  // exercise 13
  val listFoldable = new Foldable[List] {
    def foldRight[A,B](as: List[A])(zero: B)(f: (A,B) => B): B = as match {
      case Nil => zero
      case a :: as => f(a, foldRight(as)(zero)(f))
    }
    def foldLeft[A,B](as: List[A])(zero: B)(f: (B,A) => B): B = {
      @annotation.tailrec
      def go(as: List[A], b: B): B = as match {
        case Nil => b
        case a :: as => go(as, f(b,a))
      }
      go(as, zero)
    }
  }
  val indexedSeqFoldable = new Foldable[IndexedSeq] {
    def foldRight[A,B](as: IndexedSeq[A])(zero: B)(f: (A,B) => B): B = as match {
      case IndexedSeq() => zero
      case as => f(as.head, foldRight(as.tail)(zero)(f))
    }
    def foldLeft[A,B](as: IndexedSeq[A])(zero: B)(f: (B,A) => B): B = {
      @annotation.tailrec
      def go(as: IndexedSeq[A], b: B): B = as match {
        case IndexedSeq() => b
        case as => go(as.tail, f(b,as.head))
      }
      go(as, zero)
    }
  }
  import ch5._
  val streamFoldable = new Foldable[Stream] {
    def foldRight[A,B](as: Stream[A])(zero: B)(f: (A,B) => B): B =
      as.foldRight(zero)((a,b) => f(a,b))
    def foldLeft[A,B](as: Stream[A])(zero: B)(f: (B,A) => B): B = {
      @annotation.tailrec
      def go(as: Stream[A], b: B): B = as.uncons match {
        case None       => b
        case Some(cons) => go(cons.tail, f(b, cons.head))
      }
      go(as, zero)
    }
  }

  // exercise 14
  import ch3._
  val treeFoldable = new Foldable[Tree] {
    def foldRight[A,B](as: Tree[A])(zero: B)(f: (A,B) => B): B = as match {
      case Leaf(a) => f(a,zero)
      case Branch(left, right) =>
        foldRight(left)(foldRight(right)(zero)(f))(f)
    }
    def foldLeft[A,B](as: Tree[A])(zero: B)(f: (B,A) => B): B = ???
  }

  // exercise 15
  val optionFoldable = new Foldable[Option] {
    def foldRight[A,B](oa: Option[A])(zero: B)(f: (A,B) => B): B =
      oa.map(a => f(a,zero)).getOrElse(zero)
    def foldLeft[A,B](oa: Option[A])(zero: B)(f: (B,A) => B): B =
      foldRight(oa)(zero)((b,a) => f(a,b))
  }

  // exercise 17
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def op(ab1: (A,B), ab2: (A,B)): (A,B) =
      (
        A.op(ab1._1, ab2._1),
        B.op(ab1._2, ab2._2)
      )

    def zero = (A.zero, B.zero)
  }
  val productMonoidLaws = monoidLaws(productMonoid(intAddition, stringMonoid),
                                     Gen.int ** Gen.string)

  // exercise 18
  // if right biased, it can be a monoid
  def eitherMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[Either[A,B]] = new Monoid[Either[A,B]] {
    def op(ab1: Either[A,B], ab2: Either[A,B]) = (ab1, ab2) match {
      case (Right(b1), Right(b2)) => Right(B.op(b1, b2))
      case (Right(b1), Left(a2))  => Left(a2)
      case (Left(a1),  Right(b2)) => Left(a1)
      case (Left(a1),  Left(a2))  => Left(A.op(a1, a2))
    }
    def zero = Right(B.zero)
  }
  val eitherMonoidLaws = monoidLaws(eitherMonoid(stringMonoid, intAddition),
                                    Gen.union(
                                      Gen.string.map(Left.apply),
                                      Gen.int.map(Right.apply)
                                    ))

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K,V]] =
    new Monoid[Map[K,V]] {
      def zero = Map.empty
      def op(a: Map[K,V], b: Map[K,V]) = {
        val allKeys = a.keySet ++ b.keySet
        allKeys.map { k =>
          (k, V.op(a.get(k) getOrElse V.zero,
                   b.get(k) getOrElse V.zero))
        } toMap
      }
    }
  val mapMergeMonoidLaws = monoidLaws(mapMergeMonoid[Int, Int](intAddition),
                                      SGen.listOf(Gen.int ** Gen.int).map(_.toMap))

  // exercise 19
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def zero = a => B.zero
      def op(f1: A => B, f2: A => B) =
        a => B.op(f1(a), f2(a))
    }
  val functionMonoidLaws = monoidLaws(functionMonoid[Int,Int](intAddition),
                                      Gen.endoIntFunction)

  // exercise 20
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val asMaps = as.map(a => Map(a -> 1)).toList
    concatenate(asMaps, mapMergeMonoid[A,Int](intAddition))
  }

  val m = productMonoid(intAddition, intAddition)
  val (total, size) = listFoldable.foldMap(scala.collection.immutable.List(1,2,3,4))(a => (1,a))(m)
  val mean = total / size.toDouble
}
