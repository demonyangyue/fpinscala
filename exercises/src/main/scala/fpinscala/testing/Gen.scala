package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import Status._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

sealed trait Status {}

object Status {
  case object Exhausted extends Status
  case object Proven extends Status
  case object Unfalsified extends Status
}

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop) = Prop {
    (m, t, rng) => run(m, t, rng) match {
      case Right((a,n)) => p.run(m,n,rng).right.map {case (s, m) => (s, n+m)}
      case l => l
    }
  }

  def ||(p: Prop) = Prop {
    (m, t, rng) => run(m, t, rng) match {
      case Left(s) => p.run(m,n,rng).right.map {case (s, m) => (s, n+m)}
      case r => r
    }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type Result = Either[FailedCase,(Status,SuccessCount)]

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => {
      def go(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result): Result =
        if (i == j) Right((Unfalsified, i))
        else s match {
          case Cons(h,t) => h() match {
            case Some(h) =>
              try {
                if (f(h)) go(i+1,j,t(),onEnd)
                else Left(h.toString) }
              catch { case e: Exception => Left(buildMsg(h, e)) }
            case None => Right((Unfalsified,i))
          }
          case _ => onEnd(i)
        }
      go(0, n/3, a.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified,_)) =>
          val rands = randomStream(a)(rng).map(Some(_))
          go(n/3, n, rands, i => Right((Unfalsified, i)))
        case s => s // If proven or failed, stop immediately
      }
    }
  }
}

case class Gen[+A](sample: State[RNG,A], exhaustive: Stream[Option[A]]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(_.map(f)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = 
    //notice thant here the return type of f is Gen[B], not State[RNG, B]
  Gen(sample.flatMap(a => f(a).sample), exhaustive.flatMap { 
    case Some(x) => f(x).exhaustive
    case None => unbounded
  })

}

object Gen {

  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))
  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a), bounded(Stream(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean), bounded(Stream(true, false)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)), bounded(Stream.from(start).take(stopExclusive-start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)),
        cartesian(Stream.constant(g.exhaustive).take(n)).
        map(l => sequenceOption(l.toList)))

  /* `cartesian` generates all possible combinations of a `Stream[Stream[A]]`. For instance:
   *
   *    cartesian(Stream(Stream(1,2), Stream(3), Stream(4,5))) ==
   *    Stream(Stream(1,3,4), Stream(1,3,5), Stream(2,3,4), Stream(2,3,5))
  */
  def cartesian[A](s: Stream[Stream[A]]): Stream[Stream[A]] =
    s.foldRight(Stream(Stream[A]()))((hs,ts) => map2Stream(hs,ts)(Stream.cons(_,_)))

  def uniform: Gen[Double] = Gen(State(RNG.double), unbounded)
  
  def choose(i: Double, j: Double): Gen[Double] = 
    Gen(State(RNG.double).map(d => i + d*(j-i)), unbounded)
    
  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 == 0) stopExclusive - 1 else stopExclusive).
    map (n => if (n%2 != 0) n+1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 != 0) stopExclusive - 1 else stopExclusive).
    map (n => if (n%2 == 0) n+1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int,Int)] = for {
    i <- choose(from,to)
    j <- if (i%2 == 0) even(from,to) else odd(from,to)
  } yield (i,j)

}


trait SGen[+A] {

}

