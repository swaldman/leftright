package com.mchange.leftright;

import scala.language.implicitConversions;

import scala.util.{Either,Left,Right}

/**
  * For now, this is maintained in a scala language fork, proposed for
  * inclusion within Either. Modifications/improvements should be made there, 
  * and then pulled back here.
  * 
  * This library lacks detailed documentation, to keep the code concise and 
  * easy to review. For full documentation, please see the 
  * [[http://www.mchange.com/work/enrich-bias-either/enrich-bias-either-2015-09-19/index.html#scala.util.Either forked version of Either]].
  * The biasing in this library is identical to that documented for the scala 
  * fork, except that `Either.LeftBias` and Either.RightBias` map to 
  * `BiasedEither.LeftBias` and `BiasedEither.RightBias`.
  */ 
object BiasedEither {
  trait Bias[+E] {
    def empty : E;

    def isLeftBias  : Boolean;
    def isRightBias : Boolean = !isLeftBias;
    def conformsToBias[A,B]( target : Either[A,B] ) : Boolean = {
      target match {
        case Left( _ )  => isLeftBias;
        case Right( _ ) => isRightBias;
      }
    }
  }
  final object RightBias {

    private[BiasedEither] final val DefaultThrowingOps = withEmptyToken.Throwing( throw new NoSuchElementException( noSuchElementMessage( true ) ) );

    implicit class Ops[A,B]( val target : Either[A,B] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[AA >: A, Z]( f : B => Either[AA,Z] ) : Either[AA,Z] = DefaultThrowingOps.flatMap[A,AA,B,Z]( target )( f );
      def map[Z]( f : B => Z )                         : Either[A,Z]  = DefaultThrowingOps.map( target )( f );
      def withFilter( p : B => Boolean )               : Either[A,B]  = DefaultThrowingOps.withFilter( target )( p );

      // extra ops
      def exists( f : B => Boolean )                  : Boolean           = DefaultThrowingOps.exists( target )( f );
      def forall( f : B => Boolean )                  : Boolean           = DefaultThrowingOps.forall( target )( f );
      def foreach[U]( f : B => U )                    : Any               = DefaultThrowingOps.foreach( target )( f );
      def get                                         : B                 = DefaultThrowingOps.get( target );
      def getOrElse[ BB >: B ]( or : =>BB )           : BB                = DefaultThrowingOps.getOrElse[A,B,BB]( target )( or );
      def toOption                                    : Option[B]         = DefaultThrowingOps.toOption( target );
      def toSeq                                       : collection.Seq[B] = DefaultThrowingOps.toSeq( target );
      def xget                                        : A                 = DefaultThrowingOps.xget( target );
      def xgetOrElse[AA>:A]( or : =>AA )              : AA                = DefaultThrowingOps.xgetOrElse[A,AA,B]( target )( or );
      def xmap[Z]( f : A => Z )                       : Either[Z,B]       = DefaultThrowingOps.xmap( target )( f );
      def replaceIfEmpty[AA>:A]( replacement : =>AA ) : Either[AA,B]      = DefaultThrowingOps.replaceIfEmpty[A,AA,B]( target )( replacement );
      def isEmpty                                     : Boolean           = DefaultThrowingOps.isEmpty( target );
      def isLeftBiased                                : Boolean           = DefaultThrowingOps.isLeftBias;
      def isRightBiased                               : Boolean           = DefaultThrowingOps.isRightBias;
      def conformsToBias                              : Boolean           = DefaultThrowingOps.conformsToBias( target );
    }

    object withEmptyToken {
      abstract class AbstractOps[A,B]( target : Either[A,B] )( opsTypeClass : BiasedEither.RightBias.withEmptyToken.Generic[A] ) {

        // monad ops
        def flatMap[AA >: A, Z]( f : B => Either[AA,Z] ) : Either[AA,Z] = opsTypeClass.flatMap[A,AA,B,Z]( target )( f );
        def map[Z]( f : B => Z )                         : Either[A,Z]  = opsTypeClass.map( target )( f );
        def withFilter( p : B => Boolean )               : Either[A,B]  = opsTypeClass.withFilter( target )( p );

        // extra ops
        def exists( f : B => Boolean )                  : Boolean           = opsTypeClass.exists( target )( f );
        def forall( f : B => Boolean )                  : Boolean           = opsTypeClass.forall( target )( f );
        def foreach[U]( f : B => U )                    : Any               = opsTypeClass.foreach( target )( f );
        def get                                         : B                 = opsTypeClass.get( target );
        def getOrElse[BB>:B]( or : =>BB )               : BB                = opsTypeClass.getOrElse[A,B,BB]( target )( or );
        def toOption                                    : Option[B]         = opsTypeClass.toOption( target );
        def toSeq                                       : collection.Seq[B] = opsTypeClass.toSeq( target );
        def isEmpty                                     : Boolean           = opsTypeClass.isEmpty( target );
        def xget                                        : A                 = opsTypeClass.xget( target );
        def xgetOrElse[AA>:A]( or : =>AA )              : AA                = opsTypeClass.xgetOrElse[A,AA,B]( target )( or );
        def xmap[Z]( f : A => Z )                       : Either[Z,B]       = opsTypeClass.xmap( target )( f );
        def replaceIfEmpty[AA>:A]( replacement : =>AA ) : Either[AA,B]      = opsTypeClass.replaceIfEmpty[A,AA,B]( target )( replacement );
        def isLeftBiased                                : Boolean           = opsTypeClass.isLeftBias;
        def isRightBiased                               : Boolean           = opsTypeClass.isRightBias;
        def conformsToBias                              : Boolean           = opsTypeClass.conformsToBias( target );
      }

      implicit final class Ops[A,B]( target : Either[A,B] )( implicit opsTypeClass : BiasedEither.RightBias.withEmptyToken.Generic[A] ) extends AbstractOps( target )( opsTypeClass );

      trait Generic[+E] extends BiasedEither.Bias[E] {
        /*
         * In order to meet the contract of withFilter(...) [from which this method is called],
         * no object allocation should occur on each non-Exception-raising call of this method.
         * Raising an exception is "fine" (in that it represents a hackish violation of the contract
         * anyway), as is overriding this with a val. But no new Left should be created on each
         * invocation.
         */ 
        protected def leftEmpty : Left[E,Nothing];

        def isEmpty[A>:E,B]( target : Either[A,B] ) : Boolean;

        // monad ops
        def flatMap[A>:E,AA>:A,B,Z]( target : Either[A,B] )( f : B => Either[AA,Z] ) : Either[AA,Z] = {
          target match {
            case Left( _ )  => target.asInstanceOf[Left[A,Z]]
            case Right( b ) => f( b )
          }
        }
        def map[A>:E,B,Z]( target : Either[A,B] )( f : B => Z ) : Either[A,Z] = {
          target match {
            case Left( _ )  => target.asInstanceOf[Left[A,Z]]
            case Right( b ) => Right( f( b ) )
          }
        }
        def withFilter[A>:E,B]( target : Either[A,B] )( p : B => Boolean ) : Either[A,B] = {
          target match {
            case      Left( _ ) => target;
            case r @ Right( b ) => if ( p(b) ) r else leftEmpty;
          }
        }

        // extra ops
        def exists[A>:E,B]( target : Either[A,B] )( f : B => Boolean ) : Boolean = {
          target match {
            case Left( _ )  => false;
            case Right( b ) => f( b );
          }
        }
        def forall[A>:E,B]( target : Either[A,B] )( f : B => Boolean ) : Boolean = {
          target match {
            case Left( _ )  => true;
            case Right( b ) => f( b );
          }
        }
        def foreach[A>:E,B,U]( target : Either[A,B] )( f : B => U ) : Any = {
          target match {
            case Left( _ )  => ();
            case Right( b ) => f( b );
          }
        }
        def get[A>:E,B]( target : Either[A,B] ) : B = {
          target match {
            case Left( _ )  => throw new NoSuchElementException( NoSuchRightMessage );
            case Right( b ) => b;
          }
        }
        def getOrElse[A>:E,B,BB>:B]( target : Either[A,B] )( or : =>BB ) : BB = {
          target match {
            case Left( _ )  => or;
            case Right( b ) => b;
          }
        }
        def toOption[A>:E,B]( target : Either[A,B] ) : Option[B] = {
          target match {
            case Left( _ )  => None;
            case Right( b ) => Some( b );
          }
        }
        def toSeq[A>:E,B]( target : Either[A,B] ) : collection.Seq[B] = {
          target match {
            case Left( _ )  => collection.Seq.empty[B];
            case Right( b ) => collection.Seq( b );
          }
        }
        def xget[A>:E,B]( target : Either[A,B] ) : A = {
          target match {
            case Left( a )  => a;
            case Right( _ ) => throw new NoSuchElementException( NoSuchXLeftMessage );
          }
        }
        def xgetOrElse[A>:E,AA>:A,B]( target : Either[A,B] )( or : =>AA ) : AA = {
          target match {
            case Left( a )  => a;
            case Right( _ ) => or;
          }
        }
        def xmap[A>:E,B,Z]( target : Either[A,B] )( f : A => Z ) : Either[Z,B] = {
          target match {
            case Left( a )  => Left( f( a ) )
            case Right( _ ) => target.asInstanceOf[Right[Z,B]]
          }
        }
        def replaceIfEmpty[A>:E,AA>:A,B]( target : Either[A,B] )( replacement : =>AA ) : Either[AA,B] = {
          if (isEmpty( target )) Left( replacement ) else target;
        }
        def isLeftBias  : Boolean = false;

        implicit def toOps[A>:E,B]( target : Either[A,B] ) : RightBias.withEmptyToken.Ops[A,B] = new RightBias.withEmptyToken.Ops[A,B]( target )( this )
      }
      def apply[E]( token : E ) : withEmptyToken[E] = new withEmptyToken( token );

      object Throwing {
        def apply( throwableBuilder : =>java.lang.Throwable ) : Throwing = new Throwing( throwableBuilder );
      }
      final class Throwing private( throwableBuilder : =>java.lang.Throwable ) extends withEmptyToken.Generic[Nothing] {
        override protected def leftEmpty : Nothing = empty;

        override def empty : Nothing = throw throwableBuilder;

        override def isEmpty[A,B]( target : Either[A,B] ) : Boolean = false; // no state represents empty, empties cannot be formed as an Exception is thrown when it is tried
      }
    }

    final class withEmptyToken[+E] private( override val empty : E ) extends withEmptyToken.Generic[E] {
      override protected val leftEmpty : Left[E,Nothing] = Left(empty);

      override def isEmpty[A>:E,B]( target : Either[A,B] ) : Boolean = (target == leftEmpty);
    }
    abstract class Base[L]( emptyToken : L ) extends RightBias[L] {
      override val EmptyTokenDefinition = RightBias.withEmptyToken[L]( emptyToken )
    }
  }
  trait RightBias[L] {
    val EmptyTokenDefinition : BiasedEither.RightBias.withEmptyToken.Generic[L] = RightBias.DefaultThrowingOps;

    implicit def toRightBiasEtherOps[R]( target : Either[L,R] ) : RightBias.withEmptyToken.AbstractOps[L,R] = new RightBias.withEmptyToken.Ops[L,R]( target )( EmptyTokenDefinition );
  }

  final object LeftBias {

    private[BiasedEither] final val DefaultThrowingOps = withEmptyToken.Throwing( throw new NoSuchElementException( noSuchElementMessage( false ) ) );

    implicit class Ops[A,B]( val target : Either[A,B] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[BB >: B, Z]( f : A => Either[Z,BB] ) : Either[Z,BB] = DefaultThrowingOps.flatMap[A,B,BB,Z]( target )( f );
      def map[Z]( f : A => Z )                         : Either[Z,B]  = DefaultThrowingOps.map( target )( f );
      def withFilter( p : A => Boolean )               : Either[A,B]  = DefaultThrowingOps.withFilter( target )( p );

      // extra ops
      def exists( f : A => Boolean )                  : Boolean           = DefaultThrowingOps.exists( target )( f );
      def forall( f : A => Boolean )                  : Boolean           = DefaultThrowingOps.forall( target )( f );
      def foreach[U]( f : A => U )                    : Any               = DefaultThrowingOps.foreach( target )( f );
      def get                                         : A                 = DefaultThrowingOps.get( target );
      def getOrElse[AA >: A ]( or : =>AA )            : AA                = DefaultThrowingOps.getOrElse[A,AA,B]( target )( or );
      def toOption                                    : Option[A]         = DefaultThrowingOps.toOption( target );
      def toSeq                                       : collection.Seq[A] = DefaultThrowingOps.toSeq( target );
      def isEmpty                                     : Boolean           = DefaultThrowingOps.isEmpty( target );
      def xget                                        : B                 = DefaultThrowingOps.xget( target );
      def xgetOrElse[BB>:B]( or : =>BB )              : BB                = DefaultThrowingOps.xgetOrElse[A,B,BB]( target )( or );
      def xmap[Z]( f : B => Z )                       : Either[A,Z]       = DefaultThrowingOps.xmap( target )( f );
      def replaceIfEmpty[BB>:B]( replacement : =>BB ) : Either[A,BB]      = DefaultThrowingOps.replaceIfEmpty[A,B,BB]( target )( replacement )
      def isLeftBiased                                : Boolean           = DefaultThrowingOps.isLeftBias;
      def isRightBiased                               : Boolean           = DefaultThrowingOps.isRightBias;
      def conformsToBias                              : Boolean           = DefaultThrowingOps.conformsToBias( target );
    }

    object withEmptyToken {
      abstract class AbstractOps[A,B]( target : Either[A,B] )( opsTypeClass : BiasedEither.LeftBias.withEmptyToken.Generic[B] ) {

        // monad ops
        def flatMap[BB >: B, Z]( f : A => Either[Z,BB] ) : Either[Z,BB] = opsTypeClass.flatMap[A,B,BB,Z]( target )( f );
        def map[Z]( f : A => Z ) : Either[Z,B] = opsTypeClass.map( target )( f );
        def withFilter( p : A => Boolean ) : Either[A,B] = opsTypeClass.withFilter( target )( p );

        // extra ops
        def exists( f : A => Boolean ) : Boolean = opsTypeClass.exists( target )( f );
        def forall( f : A => Boolean ) : Boolean = opsTypeClass.forall( target )( f );
        def foreach[U]( f : A => U ) : Any = opsTypeClass.foreach( target )( f );
        def get : A = opsTypeClass.get( target );
        def getOrElse[AA >: A ]( or : =>AA ) : AA = opsTypeClass.getOrElse[A,AA,B]( target )( or );
        def toOption : Option[A] = opsTypeClass.toOption( target );
        def toSeq : collection.Seq[A] = opsTypeClass.toSeq( target );
        def isEmpty : Boolean = opsTypeClass.isEmpty( target );
        def xget : B = opsTypeClass.xget( target );
        def xgetOrElse[BB>:B]( or : =>BB ) : BB = opsTypeClass.xgetOrElse[A,B,BB]( target )( or );
        def xmap[Z]( f : B => Z ) : Either[A,Z] = opsTypeClass.xmap( target )( f );
        def replaceIfEmpty[BB>:B]( replacement : =>BB ) : Either[A,BB] = opsTypeClass.replaceIfEmpty[A,B,BB]( target )( replacement )
        def isLeftBiased : Boolean = opsTypeClass.isLeftBias;
        def isRightBiased : Boolean = opsTypeClass.isRightBias;
        def conformsToBias : Boolean = opsTypeClass.conformsToBias( target );
      }

      implicit final class Ops[A,B]( target : Either[A,B] )( implicit opsTypeClass : BiasedEither.LeftBias.withEmptyToken.Generic[B] ) extends AbstractOps( target )( opsTypeClass );

      trait Generic[+E] extends BiasedEither.Bias[E] {
        /*
         * In order to meet the contract of withFilter(...) [from which this method is called],
         * no object allocation should occur on each non-Exception-raising call of this method.
         * Raising an exception is "fine" (in that it represents a hackish violation of the contract
         * anyway), as is overriding this with a val. But no new Right should be created on each
         * invocation.
         */ 
        protected def rightEmpty : Right[Nothing,E]; 

        def isEmpty[A>:E,B]( target : Either[A,B] ) : Boolean;

        // monad ops
        def flatMap[A, B>:E, BB>:B ,Z]( target : Either[A,B] )( f : A => Either[Z,BB] ) : Either[Z,BB] = {
          target match {
            case Left( a )  => f( a )
            case Right( _ ) => target.asInstanceOf[Right[Z,B]]
          }
        }
        def map[A, B>:E, Z]( target : Either[A,B] )( f : A => Z ) : Either[Z,B] = {
          target match {
            case Left( a )  => Left( f( a ) )
            case Right( _ ) => target.asInstanceOf[Right[Z,B]]
          }
        }
        def withFilter[A,B>:E]( target : Either[A,B] )( p : A => Boolean ) : Either[A,B] = {
          target match {
            case l @  Left( a ) => if ( p(a) ) l else rightEmpty;
            case     Right( _ ) => target;
          }
        }

        // extra ops
        def exists[A,B>:E]( target : Either[A,B] )( f : A => Boolean ) : Boolean = {
          target match {
            case Left( a )  => f(a);
            case Right( _ ) => false;
          }
        }
        def forall[A,B>:E]( target : Either[A,B] )( f : A => Boolean ) : Boolean = {
          target match {
            case Left( a )  => f(a)
            case Right( _ ) => true;
          }
        }
        def foreach[A,B>:E,U]( target : Either[A,B] )( f : A => U ) : Any = {
          target match {
            case Left( a )  => f(a);
            case Right( _ ) => ();
          }
        }
        def get[A,B>:E]( target : Either[A,B] ) : A = {
          target match {
            case Left( a )  => a;
            case Right( _ ) => throw new NoSuchElementException( NoSuchLeftMessage );
          }
        }
        def getOrElse[A, AA>:A, B>:E]( target : Either[A,B] )( or : =>AA ) : AA = {
          target match {
            case Left( a )  => a;
            case Right( _ ) => or;
          }
        }
        def toOption[A,B>:E]( target : Either[A,B] ) : Option[A] = {
          target match {
            case Left( a )  => Some( a );
            case Right( _ ) => None; 
          }
        }
        def toSeq[A,B>:E]( target : Either[A,B] ) : collection.Seq[A] = {
          target match {
            case Left( a )  => collection.Seq( a );
            case Right( _ ) => collection.Seq.empty[A];
          }
        }
        def xget[A,B>:E]( target : Either[A,B] ) : B = {
          target match {
            case Left( _ )  => throw new NoSuchElementException( NoSuchXRightMessage );
            case Right( b ) => b;
          }
        }
        def xgetOrElse[A,B>:E,BB>:B]( target : Either[A,B] )( or : =>BB ) : BB = {
          target match {
            case Left( _ )  => or;
            case Right( b ) => b;
          }
        }
        def xmap[A,B>:E,Z]( target : Either[A,B] )( f : B => Z ) : Either[A,Z] = {
          target match {
            case Left( _ )  => target.asInstanceOf[Left[A,Z]]
            case Right( b ) => Right( f(b) )
          }
        }
        def replaceIfEmpty[A,B>:E,BB>:B]( target : Either[A,B] )( replacement : =>BB ) : Either[A,BB] = {
          if (isEmpty( target )) Right( replacement ) else target;
        }
        def isLeftBias  : Boolean = true;

        implicit def toOps[A,B>:E]( target : Either[A,B] ) : LeftBias.withEmptyToken.Ops[A,B] = new LeftBias.withEmptyToken.Ops[A,B]( target )( this )
      }
      def apply[E]( token : E ) : withEmptyToken[E] = new withEmptyToken( token );

      object Throwing {
        def apply( throwableBuilder : =>java.lang.Throwable ) : Throwing = new Throwing( throwableBuilder );
      }
      final class Throwing private( throwableBuilder : =>java.lang.Throwable ) extends withEmptyToken.Generic[Nothing] {
        override protected def rightEmpty : Nothing = empty;

        override def empty : Nothing = throw throwableBuilder;

        override def isEmpty[A,B]( target : Either[A,B] ) : Boolean = false; // no state represents empty, empties cannot be formed as an Exception is thrown when it is tried
      }
    }
    final class withEmptyToken[+E] private( override val empty : E ) extends withEmptyToken.Generic[E] {
      override protected val rightEmpty : Right[Nothing,E] = Right(empty);

      override def isEmpty[A>:E,B]( target : Either[A,B] ) : Boolean = (target == rightEmpty);
    }
    abstract class Base[R]( emptyToken : R ) extends LeftBias[R] {
      override val EmptyTokenDefinition = LeftBias.withEmptyToken[R]( emptyToken )
    }
  }
  trait LeftBias[R] { // we use R rather than B here to emphasize to users that it is the right-side type that must be specified here
    val EmptyTokenDefinition : BiasedEither.LeftBias.withEmptyToken.Generic[R] = LeftBias.DefaultThrowingOps;

    implicit def toLeftBiasEtherOps[L]( target : Either[L,R] ) : LeftBias.withEmptyToken.AbstractOps[L,R] = new LeftBias.withEmptyToken.Ops[L,R]( target )( EmptyTokenDefinition );
  }

  private def noSuchElementMessage[A,B]( rightBias : Boolean, mbEither : Option[Either[A,B]] = None ) = {
    val bias = if ( rightBias ) "Right-biased" else "Left-biased";
    val withToken = if ( rightBias ) "RightBias.withEmptyToken" else "LeftBias.withEmptyToken";
    val eitherRep = mbEither.fold(" ")( either => s" '${either}' " );
    s"${bias} Either${eitherRep}filtered to empty or failed to match a pattern. Consider using ${withToken}"
  }

  private val NoSuchLeftMessage = "Can't get a value from a left-biased Either which is in fact a Right.";
  private val NoSuchRightMessage = "Can't get a value from a right-biased Either which is in fact a Left.";
  private val NoSuchXLeftMessage = "This right-biased either is in fact a Right. xget requires a value against its bias."
  private val NoSuchXRightMessage = "This left-biased either is in fact a Left. xget requires a value against its bias."
}
