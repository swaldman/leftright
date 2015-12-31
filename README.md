# leftright &mdash; monadicize `Either`

This library tries to address shortcomings ([4],[5]) of `scala.util.Either` in its core use-case as a 
monadic container for failable operations that carry information about failures 
through a sequence of operations. The existing projections API does not support
`for` loops that make use of pattern matching, conditional guards, or viariable 
assignment, rendering projections crippled scala monads.

The code in this library was originally proposed to be included within `scala.util.Either`.
It is fully documented in a fork of that class, 
[here](http://www.mchange.com/work/enrich-bias-either/enrich-bias-either-2015-09-19/index.html#scala.util.Either).

However, while it remains separate, you will need to include a dependency, currently

    "com.mchange" %% "leftright" % "0.0.1"

and

```scala
import com.mchange.leftright.BiasedEither
```

and instead of referring to `Either.LeftBias`, you must refer to `BiasedEither.LeftBias`.

## Example

Here is a simple example:

```scala
import com.mchange.leftright.BiasedEither

val RightBias = BiasedEither.RightBias.withEmptyToken[String]("EMPTY")
import RightBias._

val a : Either[String,Int] = Right(1)
val b : Either[String,Int] = Right(99)

for( v <- a; w <- b ) yield v+w          // Right(100)
for( v <- a; w <- b if v > 10) yield v+w // Left(EMPTY)
```

For more complete examples, see the [documentation in the scala fork](http://www.mchange.com/work/enrich-bias-either/enrich-bias-either-2015-09-19/index.html#scala.util.Either)

## Related documents

1. [Pull Request: Implicit enrichment as alternative to broken Either projection APIs][1]
2. [SLIP Proposal - Implicit enrichment of `scala.util.Either` to support monadic bias][2]
3. [Extensively updated Either API documentation][3]
4. [SI-7222: Pattern match typing fail in for comprehension][4]
5. [SI-5589: For-comprehension on Either.RightProjection with Tuple2 extractor in generator fails to compile][5]
6. [Proposed Either/Or/Xor/Validation SLIP expert group][6]

[1]: https://github.com/scala/scala/pull/4547 "Pull Request"
[2]: https://github.com/swaldman/slip/blob/enrich-bias-either/text/0000-enrich-bias-either.md "SLIP Proposal"
[3]: http://www.mchange.com/work/enrich-bias-either/enrich-bias-either-2015-09-19/index.html#scala.util.Either "API Documentation"
[4]: https://issues.scala-lang.org/browse/SI-7222 "SI-7222"
[5]: https://issues.scala-lang.org/browse/SI-5589 "SI-5589"
[6]: https://github.com/scala/slip/issues/5 "Either/Or/Xor/Validation SLIP expert group"






