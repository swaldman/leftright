package com.mchange.leftright;

import org.scalacheck.{Arbitrary,Prop,Properties,Gen};
import Arbitrary.arbitrary;
import Prop._;

// modified from scalacheck test for scala.util.Either
object BiasedEitherProperties extends Properties("BiasedEither") {
  implicit def arbitraryEither[X, Y](implicit xa: Arbitrary[X], ya: Arbitrary[Y]): Arbitrary[Either[X, Y]] =
    Arbitrary[Either[X, Y]](Gen.oneOf(arbitrary[X].map(Left(_)), arbitrary[Y].map(Right(_))))

  object CheckLeftBiased {
    import BiasedEither.LeftBias._

    val prop_value = forAll((n: Int) => Left(n).get == n)

    val prop_getOrElse = forAll((e: Either[Int, Int], or: Int) => e.getOrElse(or) == (e match {
      case Left(a) => a
      case Right(_) => or
    }))

    val prop_forall = forAll((e: Either[Int, Int]) =>
      e.forall(_ % 2 == 0) == (e.isRight || e.get % 2 == 0))

    val prop_exists = forAll((e: Either[Int, Int]) =>
      e.exists(_ % 2 == 0) == (e.isLeft && e.get % 2 == 0))

    val prop_flatMapLeftIdentity = forAll((e: Either[Int, Int], n: Int, s: String) => {
      def f(x: Int) = if(x % 2 == 0) Left(s) else Right(s)
      Left(n).flatMap(f(_)) == f(n)})

    val prop_flatMapRightIdentity = forAll((e: Either[Int, Int]) => e.flatMap(Left(_)) == e)

    val prop_flatMapComposition = forAll((e: Either[Int, Int]) => {
      def f(x: Int) = if(x % 2 == 0) Left(x) else Right(x)
      def g(x: Int) = if(x % 7 == 0) Right(x) else Left(x)
      e.flatMap(f(_)).flatMap(g(_)) == e.flatMap(f(_).flatMap(g(_)))})

    val prop_mapIdentity = forAll((e: Either[Int, Int]) => e.map(x => x) == e)

    val prop_mapComposition = forAll((e: Either[String, Int]) => {
      def f(s: String) = s.toLowerCase
      def g(s: String) = s.reverse
      e.map(x => f(g(x))) == e.map(x => g(x)).map(f(_))})

    val prop_seq = forAll((e: Either[Int, Int]) => e.toSeq == (e match {
      case Left(a) => Seq(a)
      case Right(_) => Seq.empty
    }))

    val prop_option = forAll((e: Either[Int, Int]) => e.toOption == (e match {
      case Left(a) => Some(a)
      case Right(_) => None
    }))

    val prop_withFilter = forAll((e: Either[Int, Int] ) => {
      if ( e.isLeft ) {
        if (e.get % 2 == 0) e.withFilter( _ % 2 == 0 ) == e;
        else {
          try { e.withFilter( _ % 2 == 0 ); false }
          catch { case _ : NoSuchElementException => true }
        }
      } else {
        e.withFilter(_ % 2 == 0) == e // right should be unchanged
      }
    })

    val prop_extractTuple = forAll((e: Either[(Int,Int,Int),Int]) => {
      if ( e.isLeft ) {
      e.get._1 == (for ( ( a, b, c ) <- e ) yield a).get
      } else {
        e == (for ( ( a, b, c ) <- e ) yield a) // right should be unchanged
      }
    })

    val prop_assignVariable = forAll((e: Either[(Int,Int,Int),Int]) => {
      if ( e.isLeft ) {
        e.get._2 == (for ( tup <- e; b = tup._2 ) yield b).get
      } else {
        e == (for ( tup <- e; b = tup._2 ) yield b) // right should be unchanged
      }
    })

    val prop_filterInFor = forAll((e: Either[Int,Int], mul : Int, passThru: Boolean) => {
      if ( e.isLeft && passThru) {
        e.map(_ * mul) == (for ( x <- e if passThru ) yield (mul * x))
      } else if ( e.isLeft && !passThru ) {
        try { for ( x <- e if passThru ) yield x; false }
        catch { case nse : NoSuchElementException => true; }
      } else {
        e == (for ( x <- e ) yield x) // right should be unchanged
      }
    })
  }

  object CheckLeftBiasedWithEmptyToken {
    val Bias = BiasedEither.LeftBias.withEmptyToken(-1);
    import Bias._;

    val prop_withFilter = forAll((e: Either[Int, Int] ) => {
      if ( e.isLeft ) {
        if (e.get % 2 == 0) e.withFilter( _ % 2 == 0 ) == e;
        else e.withFilter( _ % 2 == 0 ) == Right[Int,Int](-1)
      } else {
        e.withFilter(_ % 2 == 0) == e // right should be unchanged
      }
    })

    val prop_filterInFor = forAll((e: Either[Int,Int], mul : Int, passThru: Boolean) => {
      if ( e.isLeft && passThru) {
        e.map(_ * mul) == (for ( x <- e if passThru ) yield (mul * x))
      } else if ( e.isLeft && !passThru ) {
        (for ( x <- e if passThru ) yield x) == Right[Int,Int](-1)
      } else {
        e == (for ( x <- e ) yield x) // right should be unchanged
      }
    })
  }

  object CheckRightBiased {
    import BiasedEither.RightBias._

    val prop_value = forAll((n: Int) => Right(n).get == n)

    val prop_getOrElse = forAll((e: Either[Int, Int], or: Int) => e.getOrElse(or) == (e match {
      case Left(_) => or
      case Right(b) => b
    }))

    val prop_forall = forAll((e: Either[Int, Int]) =>
      e.forall(_ % 2 == 0) == (e.isLeft || e.get % 2 == 0))

    val prop_exists = forAll((e: Either[Int, Int]) =>
      e.exists(_ % 2 == 0) == (e.isRight && e.get % 2 == 0))

    val prop_flatMapLeftIdentity = forAll((e: Either[Int, Int], n: Int, s: String) => {
      def f(x: Int) = if(x % 2 == 0) Left(s) else Right(s)
      Right(n).flatMap(f(_)) == f(n)})

    val prop_flatMapRightIdentity = forAll((e: Either[Int, Int]) => e.flatMap(Right(_)) == e)

    val prop_flatMapComposition = forAll((e: Either[Int, Int]) => {
      def f(x: Int) = if(x % 2 == 0) Left(x) else Right(x)
      def g(x: Int) = if(x % 7 == 0) Right(x) else Left(x)
      e.flatMap(f(_)).flatMap(g(_)) == e.flatMap(f(_).flatMap(g(_)))})

    val prop_mapIdentity = forAll((e: Either[Int, Int]) => e.map(x => x) == e)

    val prop_mapComposition = forAll((e: Either[Int, String]) => {
      def f(s: String) = s.toLowerCase
      def g(s: String) = s.reverse
      e.map(x => f(g(x))) == e.map(x => g(x)).map(f(_))})

    val prop_seq = forAll((e: Either[Int, Int]) => e.toSeq == (e match {
      case Left(_) => Seq.empty
      case Right(b) => Seq(b)
    }))

    val prop_option = forAll((e: Either[Int, Int]) => e.toOption == (e match {
      case Left(_) => None
      case Right(b) => Some(b)
    }))

    val prop_withFilter = forAll((e: Either[Int, Int] ) => {
      if ( e.isRight ) {
        if (e.get % 2 == 0) e.withFilter( _ % 2 == 0 ) == e;
        else {
          try { e.withFilter( _ % 2 == 0 ); false }
          catch { case _ : NoSuchElementException => true }
        }
      } else {
        e.withFilter(_ % 2 == 0) == e // left should be unchanged
      }
    })

    val prop_extractTuple = forAll((e: Either[Int,(Int,Int,Int)]) => {
      if ( e.isRight ) {
        e.get._1 == (for ( ( a, b, c ) <- e ) yield a).get
      } else {
        e == (for ( ( a, b, c ) <- e ) yield a) // left should be unchanged
      }
    })

    val prop_assignVariable = forAll((e: Either[Int,(Int,Int,Int)]) => {
      if ( e.isRight ) {
        e.get._2 == (for ( tup <- e; b = tup._2 ) yield b).get
      } else {
        e == (for ( tup <- e; b = tup._2 ) yield b) // left should be unchanged
      }
    })

    val prop_filterInFor = forAll((e: Either[Int,Int], mul : Int, passThru: Boolean) => {
      if ( e.isRight && passThru) {
        e.map(_ * mul) == (for ( x <- e if passThru ) yield (mul * x))
      } else if ( e.isRight && !passThru ) {
        try { for ( x <- e if passThru ) yield x; false }
        catch { case nse : NoSuchElementException => true; }
      } else {
        e == (for ( x <- e ) yield x) // left should be unchanged
      }
    })
  }

  object CheckRightBiasedWithEmptyToken {
    val Bias = BiasedEither.RightBias.withEmptyToken(-1);
    import Bias._;

    val prop_withFilter = forAll((e: Either[Int, Int] ) => {
      if ( e.isRight ) {
        if (e.get % 2 == 0) e.withFilter( _ % 2 == 0 ) == e;
        else e.withFilter( _ % 2 == 0 ) == Left[Int,Int](-1)
      } else {
        e.withFilter(_ % 2 == 0) == e // left should be unchanged
      }
    })

    val prop_filterInFor = forAll((e: Either[Int,Int], mul : Int, passThru: Boolean) => {
      if ( e.isRight && passThru) {
        e.map(_ * mul) == (for ( x <- e if passThru ) yield (mul * x))
      } else if ( e.isRight && !passThru ) {
        (for ( x <- e if passThru ) yield x) == Left[Int,Int](-1)
      } else {
        e == (for ( x <- e ) yield x) // left should be unchanged
      }
    })
  }

  val tests = List(
      ("LeftBiased.prop_value", CheckLeftBiased.prop_value),
      ("LeftBiased.prop_getOrElse", CheckLeftBiased.prop_getOrElse),
      ("LeftBiased.prop_forall", CheckLeftBiased.prop_forall),
      ("LeftBiased.prop_exists", CheckLeftBiased.prop_exists),
      ("LeftBiased.prop_flatMapLeftIdentity", CheckLeftBiased.prop_flatMapLeftIdentity),
      ("LeftBiased.prop_flatMapRightIdentity", CheckLeftBiased.prop_flatMapRightIdentity),
      ("LeftBiased.prop_flatMapComposition", CheckLeftBiased.prop_flatMapComposition),
      ("LeftBiased.prop_mapIdentity", CheckLeftBiased.prop_mapIdentity),
      ("LeftBiased.prop_mapComposition", CheckLeftBiased.prop_mapComposition),
      ("LeftBiased.prop_seq", CheckLeftBiased.prop_seq),
      ("LeftBiased.prop_option", CheckLeftBiased.prop_option),
      ("LeftBiased.prop_withFilter", CheckLeftBiased.prop_withFilter),
      ("LeftBiased.prop_extractTuple", CheckLeftBiased.prop_extractTuple),
      ("LeftBiased.prop_assignVariable", CheckLeftBiased.prop_assignVariable),
      ("LeftBiased.prop_filterInFor", CheckLeftBiased.prop_filterInFor),

      ("LeftBiasedWithEmptyToken.prop_withFilter", CheckLeftBiasedWithEmptyToken.prop_withFilter),
      ("LeftBiasedWithEmptyToken.prop_filterInFor", CheckLeftBiasedWithEmptyToken.prop_filterInFor),

      ("RightBiased.prop_value", CheckRightBiased.prop_value),
      ("RightBiased.prop_getOrElse", CheckRightBiased.prop_getOrElse),
      ("RightBiased.prop_forall", CheckRightBiased.prop_forall),
      ("RightBiased.prop_exists", CheckRightBiased.prop_exists),
      ("RightBiased.prop_flatMapLeftIdentity", CheckRightBiased.prop_flatMapLeftIdentity),
      ("RightBiased.prop_flatMapRightIdentity", CheckRightBiased.prop_flatMapRightIdentity),
      ("RightBiased.prop_flatMapComposition", CheckRightBiased.prop_flatMapComposition),
      ("RightBiased.prop_mapIdentity", CheckRightBiased.prop_mapIdentity),
      ("RightBiased.prop_mapComposition", CheckRightBiased.prop_mapComposition),
      ("RightBiased.prop_seq", CheckRightBiased.prop_seq),
      ("RightBiased.prop_option", CheckRightBiased.prop_option),
      ("RightBiased.prop_withFilter", CheckRightBiased.prop_withFilter),
      ("RightBiased.prop_extractTuple", CheckRightBiased.prop_extractTuple),
      ("RightBiased.prop_assignVariable", CheckRightBiased.prop_assignVariable),
      ("RightBiased.prop_filterInFor", CheckRightBiased.prop_filterInFor),

      ("RightBiasedWithEmptyToken.prop_withFilter", CheckRightBiasedWithEmptyToken.prop_withFilter),
      ("RightBiasedWithEmptyToken.prop_filterInFor", CheckRightBiasedWithEmptyToken.prop_filterInFor)
    )

  for ((label, prop) <- tests) {
    property(label) = prop
  }
}
