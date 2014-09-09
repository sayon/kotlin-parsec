package jet.util.parsing.combinator

import jet.util.parsing.input.Reader
import jet.util.parsing.combinator.Parsers.Success
import jet.util.parsing.combinator.Parsers.NoSuccess
import java.util.LinkedList


abstract class Parsers<in Elem> {

    abstract inner class ParseResult<in T> {
        abstract fun map<U>(f: (T) -> U): ParseResult<U>

        abstract fun flatMapWithNext<U>(f: (T) -> ((Reader<Elem>) -> ParseResult<U>)): ParseResult<U>

        abstract fun filterWithError(p: (T) -> Boolean, error: (T) -> String, position: Reader<Elem>): ParseResult<T>

        abstract fun append<U : T>(a: () -> ParseResult<U>): ParseResult<U>

        val isEmpty: Boolean get() = !successful

        abstract fun get(): T

        abstract val next: Reader<Elem>

        abstract val successful: Boolean
    }

    inner class Success<in T>(val result: T, override val next: Reader<Elem>) : ParseResult<T>()
    {
        override fun <U : T> append(a: () -> Parsers.ParseResult<U>): Parsers.ParseResult<U> = this
        override fun filterWithError(p: (T) -> Boolean, error: (T) -> String, position: Reader<Elem>): Parsers.ParseResult<T> =
                if (p(result)) this
                else Failure(error(result), position)
        override fun <U> flatMapWithNext(f: (T) -> (Reader<Elem>) -> Parsers.ParseResult<U>): Parsers.ParseResult<U> = f(result)(next)

        override fun map<U>(f: (T) -> U) = Success(f(result), next)

        override fun get(): T = result

        fun toString() = "[ ${next.pos}] parsed: $result"

        override val successful = true

    }

    inner abstract class NoSuccess(val msg: String, override val next: Reader<Elem>) : ParseResult<Any>() {
        override val successful = false
        override fun map<U>(f: (Any) -> U): Parsers.ParseResult<U> = this

        override fun flatMapWithNext<U>(f: (Any) -> ((Reader<Elem>) -> ParseResult<U>)): ParseResult<U> = this

        override fun filterWithError(p: (Any) -> Boolean, error: (Any) -> String, position: Reader<Elem>): ParseResult<Any> = this

        override fun get(): Any = error("No result when parsing failed")
    }

    inner class Failure(msg: String, next: Reader<Elem>) : NoSuccess(msg, next) {
        override fun <U : Any> append(a: () -> Parsers.ParseResult<U>): Parsers.ParseResult<U> {
            val alt = a.invoke();
            return when (alt) {
                is Success -> alt
                is NoSuccess -> if (alt.next.pos < next.pos) this else alt
                else -> throw IllegalStateException("Only Success or NoSuccess inheritors are implemented as ParseResult inheritors")
            }
        }

        fun toString() = "[${next.pos}] failure: $msg \n\n${next.pos.longString}"
    }

    inner class Error(msg: String, next: Reader<Elem>) : NoSuccess(msg, next) {
        override fun <U : Any> append(a: () -> Parsers.ParseResult<U>): Parsers.ParseResult<U> = this
    }


    //        def OnceParser[T](f: Input => ParseResult[T]): Parser[T] with OnceParser[T]
    //        = new Parser[T] with OnceParser[T] { def apply(in: Input) = f(in) }
    //

    //fixme: ThisType should be replaced by This in future when self types are supported
    abstract inner class Parser<In T> : (Reader<Elem>) -> ParseResult<T> {
        abstract fun apply(reader: Reader<Elem>): ParseResult<T>
        fun flatMap<U>(f: (T) -> Parser<U>): Parser<U> = Parser { apply(it) flatMapWithNext f }
        fun map<U>(f: (T) -> U): Parser<U> = Parser { apply(it) map f }
        fun filter(p: (T) -> Boolean): Parser<T> = withFilter(p)
        fun withFilter(p: (T) -> Boolean): Parser<T> = Parser { apply(it).filterWithError(p, { "Input doesn't match filter: " }, it) }
        fun append<U : T>(that: () -> Parser<U>): Parser<U> = Parser { r -> apply(r).append({ that().apply(r) }) }

        fun `~`<U>(that: () -> Parser<U>): Parser<And<T, U>> = flatMap<And<T, U>> { a -> that() map { b -> And(a, b) } }
        fun `~)` <U>(that: () -> Parser<U>): Parser<U> = flatMap { that() map { it } }
        fun `(~` <U>(that: () -> Parser<U>): Parser<T> = flatMap { a -> that().map { a } }

        //fixme when the lower type bounds will be supported, this one should be rewritten.
        fun `|`(that: () -> Parser<T>): Parser<T> = append(that)

        ///* not really useful: V cannot be inferred because Parser is covariant in first type parameter (V is always trivially Nothing)
        //fun ~~ [U, V](q: => Parser<U>)(implicit combine: (T, U) => V): Parser[V] = new Parser[V] {
        // fun apply(in: Reader<Elem>) = seq(Parser.this, q)((x, y) => combine(x,y))(in)
        //}  */
        //

        fun `^^`<U>(f: (T) -> U) = map(f)
        fun into<U>(fq: (T) -> Parser<U>): Parser<U> = flatMap(fq)
    }

    fun rep<T>(p: () -> Parser<T>): Parser<List<T>> = rep1(p) `|` { success(listOf<T>()) }

    fun success<T>(v: T) = Parser { Success(v, it) }

    fun rep1<T>(p: ()-> Parser<T>): Parser<List<T>> = rep1(p, p)
    fun rep1<T>(first: () -> Parser<T>, p0: () -> Parser<T>): Parser<List<T>> = Parser { rep1impl(it, first, p0) }

    private fun rep1impl<T>(input: Reader<Elem>, first: () -> Parser<T>, p0: () -> Parser<T>): ParseResult<List<T>> {

        val elems = LinkedList<T>()

        val parser = p0()
        fun cont(input: Reader<Elem>): ParseResult<List<T>> {


            fun applyp(in0: Reader<Elem>): ParseResult<List<T>> {
                val parseRes = parser(in0)
                return when (parseRes) {
                    is Success<T> -> {
                        elems.addLast(parseRes.result); applyp(parseRes.next)
                    }
                    is Error -> parseRes
                    else -> Success(elems, in0)
                }
            }
            return applyp(input)
        }

        val fst = first().apply(input)
        return when (fst) {
            is Success<T> -> {
                elems addLast fst.result; cont(fst.next)
            }
            is NoSuccess -> fst
            else -> {
                throw IllegalStateException()
            }
        }
    }


    fun Parser<T>(f: (Reader<Elem>) -> ParseResult<T>): Parser<T> = Parser<T>{ fun apply(reader: Reader<Elem>) = f(reader) }

    class And<in a, in b>(val _1: a, val  _2: b) {
        fun toString() = "(" + _1 + "~" + _2 + ")"
    }
    abstract class Or<in a, in b>
    class Left<a>(val get: a) : Or<a, Nothing>()
    class Right<b>(val get: b) : Or<Nothing, b>()

    //    class Or<in a, in b>(val _1: a, val  _2: b) {
    //        fun toString() = "(" + _1 + "~" + _2 + ")"
    //    }
}
//fun ~! <U>(p: => Parser<U>): Parser[~[T, U]]
//= OnceParser{ (for(a <- this; b <- commit(p)) yield new ~(a,b)).named("~!") }
//
///** A parser combinator for alternative with longest match composition.
//*
//*  `p ||| q` succeeds if `p` succeeds or `q` succeeds.
//*  If `p` and `q` both succeed, the parser that consumed the most characters accepts.
//*
//* @param q0 a parser that accepts if p consumes less characters. -- evaluated at most once, and only when necessary
//* @return a `Parser` that returns the result of the parser consuming the most characters (out of `p` and `q`).
//*/
//@migration("The call-by-name argument is evaluated at most once per constructed Parser object, instead of on every need that arises during parsing.", "2.9.0")
//fun ||| [U >: T](q0: => Parser<U>): Parser<U> = new Parser<U> {
//lazy val q = q0 // lazy argument
//fun apply(in: Reader<Elem>) = {
//val res1 = Parser.this(in)
//val res2 = q(in)
//
//(res1, res2) match {
//case (s1 @ Success(_, next1), s2 @ Success(_, next2)) => if (next2.pos < next1.pos) s1 else s2
//case (s1 @ Success(_, _), _) => s1
//case (_, s2 @ Success(_, _)) => s2
//case (e1 @ Error(_, _), _) => e1
//case (f1 @ Failure(_, next1), ns2 @ NoSuccess(_, next2)) => if (next2.pos < next1.pos) f1 else ns2
//}
//}
//override fun toString = "|||"
//}

//
///** A parser combinator that changes a successful result into the specified value.
// *
// *  `p ^^^ v` succeeds if `p` succeeds; discards its result, and returns `v` instead.
// *
// * @param v The new result for the parser, evaluated at most once (if `p` succeeds), not evaluated at all if `p` fails.
// * @return a parser that has the same behaviour as the current parser, but whose successful result is `v`
// */
//@migration("The call-by-name argument is evaluated at most once per constructed Parser object, instead of on every need that arises during parsing.", "2.9.0")
//fun ^^^ <U>(v: => U): Parser<U> =  new Parser<U> {
//lazy val v0 = v // lazy argument
//fun apply(in: Reader<Elem>) = Parser.this(in) map (x => v0)
//}.named(toString+"^^^")
//
///** A parser combinator for partial function application.
// *
// *  `p ^? (f, error)` succeeds if `p` succeeds AND `f` is defined at the result of `p`;
// *  in that case, it returns `f` applied to the result of `p`. If `f` is not applicable,
// *  error(the result of `p`) should explain why.
// *
// * @param f a partial function that will be applied to this parser's result
// *          (see `mapPartial` in `ParseResult`).
// * @param error a function that takes the same argument as `f` and produces an error message
// *        to explain why `f` wasn't applicable
// * @return a parser that succeeds if the current parser succeeds <i>and</i> `f` is applicable
// *         to the result. If so, the result will be transformed by `f`.
// */
//fun ^? <U>(f: PartialFunction[T, U], error: T => String): Parser<U> = Parser{ in =>
//this(in).mapPartial(f, error)}.named(toString+"^?")
//
///** A parser combinator for partial function application.
// *
// *  `p ^? f` succeeds if `p` succeeds AND `f` is defined at the result of `p`;
// *  in that case, it returns `f` applied to the result of `p`.
// *
// * @param f a partial function that will be applied to this parser's result
// *          (see `mapPartial` in `ParseResult`).
// * @return a parser that succeeds if the current parser succeeds <i>and</i> `f` is applicable
// *         to the result. If so, the result will be transformed by `f`.
// */
//fun ^? <U>(f: PartialFunction[T, U]): Parser<U> = ^?(f, r => "Constructor function not defined at "+r)
//
///** A parser combinator that parameterizes a subsequent parser with the
// *  result of this one.
// *
// *  Use this combinator when a parser depends on the result of a previous
// *  parser. `p` should be a function that takes the result from the first
// *  parser and returns the second parser.
// *
// *  `p into fq` (with `fq` typically `{x => q}`) first applies `p`, and
// *  then, if `p` successfully returned result `r`, applies `fq(r)` to the
// *  rest of the input.
// *
// *  ''From: G. Hutton. Higher-order functions for parsing. J. Funct. Program., 2(3):323--343, 1992.''
// *
// *  @example {{{
// *  fun perlRE = "m" ~> (".".r into (separator => """[^%s]*""".format(separator).r <~ separator))
// *  }}}
// *
// *  @param fq a function that, given the result from this parser, returns
// *         the second parser to be applied
// *  @return a parser that succeeds if this parser succeeds (with result `x`)
// *          and if then `fq(x)` succeeds
// */

//fun * = rep(this)
//
///** Returns a parser that repeatedly parses what this parser parses,
// *  interleaved with the `sep` parser. The `sep` parser specifies how
// *  the results parsed by this parser should be combined.
// *
// *  @return chainl1(this, sep)
// */
//fun *[U >: T](sep: => Parser[(U, U) => U]) = chainl1(this, sep)
//
//// TODO: improve precedence? a ~ b*(",") = a ~ (b*(","))  should be true
//
///** Returns a parser that repeatedly (at least once) parses what this parser parses.
// *
// *  @return rep1(this)
// */
//fun + = rep1(this)
//
///** Returns a parser that optionally parses what this parser parses.
// *
// *  @return opt(this)
// */
//fun ? = opt(this)
//
///** Changes the failure message produced by a parser.
// *
// *  This doesn't change the behavior of a parser on neither
// *  success nor error, just on failure. The semantics are
// *  slightly different than those obtained by doing `| failure(msg)`,
// *  in that the message produced by this method will always
// *  replace the message produced, which is not guaranteed
// *  by that idiom.
// *
// *  For example, parser `p` below will always produce the
// *  designated failure message, while `q` will not produce
// *  it if `sign` is parsed but `number` is not.
// *
// *  {{{
// *  fun p = sign.? ~ number withFailureMessage  "Number expected!"
// *  fun q = sign.? ~ number | failure("Number expected!")
// *  }}}
// *
// *  @param msg The message that will replace the default failure message.
// *  @return    A parser with the same properties and different failure message.
// */
//fun withFailureMessage(msg: String) = Parser{ in =>
//this(in) match {
//case Failure(_, next) => Failure(msg, next)
//case other            => other
//}
//}
//
///** Changes the error message produced by a parser.
// *
// *  This doesn't change the behavior of a parser on neither
// *  success nor failure, just on error. The semantics are
// *  slightly different than those obtained by doing `| error(msg)`,
// *  in that the message produced by this method will always
// *  replace the message produced, which is not guaranteed
// *  by that idiom.
// *
// *  For example, parser `p` below will always produce the
// *  designated error message, while `q` will not produce
// *  it if `sign` is parsed but `number` is not.
// *
// *  {{{
// *  fun p = sign.? ~ number withErrorMessage  "Number expected!"
// *  fun q = sign.? ~ number | error("Number expected!")
// *  }}}
// *
// *  @param msg The message that will replace the default error message.
// *  @return    A parser with the same properties and different error message.
// */
//fun withErrorMessage(msg: String) = Parser{ in =>
//this(in) match {
//case Error(_, next) => Error(msg, next)
//case other          => other
//}
//}
//}
//
///** Wrap a parser so that its failures become errors (the `|` combinator
// *  will give up as soon as it encounters an error, on failure it simply
// *  tries the next alternative).
// */
//fun commit<T>(p: => Parser<T>) = Parser{ in =>
//p(in) match{
//case s @ Success(_, _) => s
//case e @ Error(_, _) => e
//case f @ Failure(msg, next) => Error(msg, next)
//}
//}
//
///** A parser matching input elements that satisfy a given predicate.
// *
// *  `elem(kind, p)` succeeds if the input starts with an element `e` for which `p(e)` is true.
// *
// *  @param  kind   The element kind, used for error messages
// *  @param  p      A predicate that determines which elements match.
// *  @return
// */
//fun elem(kind: String, p: Elem => Boolean) = acceptIf(p)(inEl => kind+" expected")
//
///** A parser that matches only the given element `e`.
// *
// *  `elem(e)` succeeds if the input starts with an element `e`.
// *
// *  @param e the `Elem` that must be the next piece of input for the returned parser to succeed
// *  @return a `Parser` that succeeds if `e` is the next available input (and returns it).
// */
//fun elem(e: Elem): Parser[Elem] = accept(e)
//
///** A parser that matches only the given element `e`.
// *
// *  The method is implicit so that elements can automatically be lifted to their parsers.
// *  For example, when parsing `Token`s, `Identifier("new")` (which is a `Token`) can be used directly,
// *  instead of first creating a `Parser` using `accept(Identifier("new"))`.
// *
// *  @param e the `Elem` that must be the next piece of input for the returned parser to succeed
// *  @return a `tParser` that succeeds if `e` is the next available input.
// */
//
//implicit fun accept(e: Elem): Parser[Elem] = acceptIf(_ == e)("`"+e+"' expected but " + _ + " found")
//
///** A parser that matches only the given list of element `es`.
// *
// *  `accept(es)` succeeds if the input subsequently provides the elements in the list `es`.
// *
// *  @param  es the list of expected elements
// *  @return a Parser that recognizes a specified list of elements
// */
//fun accept[ES <% List[Elem]](es: ES): Parser[List[Elem]] = acceptSeq(es)
//
///** The parser that matches an element in the domain of the partial function `f`.
// *
// *  If `f` is defined on the first element in the input, `f` is applied
// *  to it to produce this parser's result.
// *
// *  Example: The parser `accept("name", {case Identifier(n) => Name(n)})`
// *          accepts an `Identifier(n)` and returns a `Name(n)`
// *
// *  @param expected a description of the kind of element this parser expects (for error messages)
// *  @param f a partial function that determines when this parser is successful and what its output is
// *  @return A parser that succeeds if `f` is applicable to the first element of the input,
// *          applying `f` to it to produce the result.
// */
//fun accept<U>(expected: String, f: PartialFunction[Elem, U]): Parser<U> = acceptMatch(expected, f)
//
///** A parser matching input elements that satisfy a given predicate.
// *
// *  `acceptIf(p)(el => "Unexpected "+el)` succeeds if the input starts with an element `e` for which `p(e)` is true.
// *
// *  @param  err    A function from the received element into an error message.
// *  @param  p      A predicate that determines which elements match.
// *  @return        A parser for elements satisfying p(e).
// */
//fun acceptIf(p: Elem => Boolean)(err: Elem => String): Parser[Elem] = Parser { in =>
//if (in.atEnd) Failure("end of input", in)
//else if (p(in.first)) Success(in.first, in.rest)
//else Failure(err(in.first), in)
//}
//
///** The parser that matches an element in the domain of the partial function `f`.
// *
// *  If `f` is defined on the first element in the input, `f` is applied
// *  to it to produce this parser's result.
// *
// *  Example: The parser `acceptMatch("name", {case Identifier(n) => Name(n)})`
// *          accepts an `Identifier(n)` and returns a `Name(n)`
// *
// *  @param expected a description of the kind of element this parser expects (for error messages)
// *  @param f a partial function that determines when this parser is successful and what its output is
// *  @return A parser that succeeds if `f` is applicable to the first element of the input,
// *          applying `f` to it to produce the result.
// */
//fun acceptMatch<U>(expected: String, f: PartialFunction[Elem, U]): Parser<U> = Parser{ in =>
//if (in.atEnd) Failure("end of input", in)
//else if (f.isDefinedAt(in.first)) Success(f(in.first), in.rest)
//else Failure(expected+" expected", in)
//}
//
///** A parser that matches only the given [[scala.collection.Iterable]] collection of elements `es`.
// *
// *  `acceptSeq(es)` succeeds if the input subsequently provides the elements in the iterable `es`.
// *
// *  @param  es the list of expected elements
// *  @return a Parser that recognizes a specified list of elements
// */
//fun acceptSeq[ES <% Iterable[Elem]](es: ES): Parser[List[Elem]] =
//es.foldRight[Parser[List[Elem]]](success(Nil)){(x, pxs) => accept(x) ~ pxs ^^ mkList}
//
///** A parser that always fails.
// *
// * @param msg The error message describing the failure.
// * @return A parser that always fails with the specified error message.
// */
//fun failure(msg: String) = Parser{ in => Failure(msg, in) }
//
///** A parser that results in an error.
// *
// * @param msg The error message describing the failure.
// * @return A parser that always fails with the specified error message.
// */
//fun err(msg: String) = Parser{ in => Error(msg, in) }
//
///** A parser that always succeeds.
// *
// * @param v The result for the parser
// * @return A parser that always succeeds, with the given result `v`
// */
//fun success<T>(v: T) = Parser{ in => Success(v, in) }
//
///** A helper method that turns a `Parser` into one that will
// *  print debugging information to stdout before and after
// *  being applied.
// */
//fun log<T>(p: => Parser<T>)(name: String): Parser<T> = Parser{ in =>
//println("trying "+ name +" at "+ in)
//val r = p(in)
//println(name +" --> "+ r)
//r
//}
//
///** A parser generator for repetitions.
// *
// *  `rep(p)` repeatedly uses `p` to parse the input until `p` fails
// *  (the result is a List of the consecutive results of `p`).
// *
// * @param p a `Parser` that is to be applied successively to the input
// * @return A parser that returns a list of results produced by repeatedly applying `p` to the input.
// */
//fun rep<T>(p: => Parser<T>): Parser[List<T>] = rep1(p) | success(List())
//
///** A parser generator for interleaved repetitions.
// *
// *  `repsep(p, q)` repeatedly uses `p` interleaved with `q` to parse the input, until `p` fails.
// *  (The result is a `List` of the results of `p`.)
// *
// *  Example: `repsep(term, ",")` parses a comma-separated list of term's, yielding a list of these terms.
// *
// * @param p a `Parser` that is to be applied successively to the input
// * @param q a `Parser` that parses the elements that separate the elements parsed by `p`
// * @return A parser that returns a list of results produced by repeatedly applying `p` (interleaved with `q`) to the input.
// *         The results of `p` are collected in a list. The results of `q` are discarded.
// */
//fun repsep<T>(p: => Parser<T>, q: => Parser[Any]): Parser[List<T>] =
//rep1sep(p, q) | success(List())
//
///** A parser generator for non-empty repetitions.
// *
// *  `rep1(p)` repeatedly uses `p` to parse the input until `p` fails -- `p` must succeed at least
// *             once (the result is a `List` of the consecutive results of `p`)
// *
// * @param p a `Parser` that is to be applied successively to the input
// * @return A parser that returns a list of results produced by repeatedly applying `p` to the input
// *        (and that only succeeds if `p` matches at least once).
// */
//fun rep1<T>(p: => Parser<T>): Parser[List<T>] = rep1(p, p)
//
///** A parser generator for non-empty repetitions.
// *
// *  `rep1(f, p)` first uses `f` (which must succeed) and then repeatedly
// *     uses `p` to parse the input until `p` fails
// *     (the result is a `List` of the consecutive results of `f` and `p`)
// *
// * @param first a `Parser` that parses the first piece of input
// * @param p0 a `Parser` that is to be applied successively to the rest of the input (if any) -- evaluated at most once, and only when necessary
// * @return A parser that returns a list of results produced by first applying `f` and then
// *         repeatedly `p` to the input (it only succeeds if `f` matches).
// */
//@migration("The `p0` call-by-name arguments is evaluated at most once per constructed Parser object, instead of on every need that arises during parsing.", "2.9.0")
//fun rep1<T>(first: => Parser<T>, p0: => Parser<T>): Parser[List<T>] = Parser { in =>
//lazy val p = p0 // lazy argument
//val elems = new ListBuffer<T>
//
//fun continue(in: Reader<Elem>): ParseResult[List<T>] = {
//val p0 = p    // avoid repeatedly re-evaluating by-name parser
//@tailrec fun applyp(in0: Reader<Elem>): ParseResult[List<T>] = p0(in0) match {
//case Success(x, rest) => elems += x ; applyp(rest)
//case e @ Error(_, _)  => e  // still have to propagate error
//case _                => Success(elems.toList, in0)
//}
//
//applyp(in)
//}
//
//first(in) match {
//case Success(x, rest) => elems += x ; continue(rest)
//case ns: NoSuccess    => ns
//}
//}
//
///** A parser generator for a specified number of repetitions.
// *
// *  `repN(n, p)` uses `p` exactly `n` time to parse the input
// *  (the result is a `List` of the `n` consecutive results of `p`).
// *
// * @param p   a `Parser` that is to be applied successively to the input
// * @param num the exact number of times `p` must succeed
// * @return    A parser that returns a list of results produced by repeatedly applying `p` to the input
// *        (and that only succeeds if `p` matches exactly `n` times).
// */
//fun repN<T>(num: Int, p: => Parser<T>): Parser[List<T>] =
//if (num == 0) success(Nil) else Parser { in =>
//val elems = new ListBuffer<T>
//val p0 = p    // avoid repeatedly re-evaluating by-name parser
//
//@tailrec fun applyp(in0: Reader<Elem>): ParseResult[List<T>] =
//if (elems.length == num) Success(elems.toList, in0)
//else p0(in0) match {
//case Success(x, rest) => elems += x ; applyp(rest)
//case ns: NoSuccess    => ns
//}
//
//applyp(in)
//}
//
//fun rep1sep<T>(p : => Parser<T>, q : => Parser[Any]): Parser[List<T>] =
//p ~ rep(q ~> p) ^^ {case x~y => x::y}
//
//fun chainl1<T>(p: => Parser<T>, q: => Parser[(T, T) => T]): Parser<T>
//= chainl1(p, p, q)
//
//fun chainl1[T, U](first: => Parser<T>, p: => Parser<U>, q: => Parser[(T, U) => T]): Parser<T>
//= first ~ rep(q ~ p) ^^ {
//case x ~ xs => xs.foldLeft(x: T){case (a, f ~ b) => f(a, b)} // x's type annotation is needed to deal with changed type inference due to SI-5189
//}
//
//fun chainr1[T, U](p: => Parser<T>, q: => Parser[(T, U) => U], combine: (T, U) => U, first: U): Parser<U>
//= p ~ rep(q ~ p) ^^ {
//case x ~ xs => (new ~(combine, x) :: xs).foldRight(first){case (f ~ a, b) => f(a, b)}
//}
//
//fun opt<T>(p: => Parser<T>): Parser[Option<T>] =
//p ^^ (x => Some(x)) | success(None)
//
//fun not<T>(p: => Parser<T>): Parser[Unit] = Parser { in =>
//p(in) match {
//case Success(_, _)  => Failure("Expected failure", in)
//case _              => Success((), in)
//}
//}
//
//fun guard<T>(p: => Parser<T>): Parser<T> = Parser { in =>
//p(in) match{
//case s@ Success(s1,_) => Success(s1, in)
//case e => e
//}
//}
//
//fun positioned[T <: Positional](p: => Parser<T>): Parser<T> = Parser { in =>
//p(in) match {
//case Success(t, in1) => Success(if (t.pos == NoPosition) t setPos in.pos else t, in1)
//case ns: NoSuccess => ns
//}
//}
//
//fun phrase<T>(p: Parser<T>) = new Parser<T> {
//fun apply(in: Reader<Elem>) = lastNoSuccessVar.withValue(None) {
//p(in) match {
//case s @ Success(out, in1) =>
//if (in1.atEnd)
//s
//else
//lastNoSuccessVar.value filterNot { _.next.pos < in1.pos } getOrElse Failure("end of input expected", in1)
//case ns => lastNoSuccessVar.value.getOrElse(ns)
//}
//}
//}
//
//fun mkList<T> = (_: ~[T, List<T>]) match { case x ~ xs => x :: xs }
//
//case class ~[+a, +b](_1: a, _2: b) {
//override fun toString = "("+ _1 +"~"+ _2 +")"
//}
//
//trait OnceParser[+T] extends Parser<T> {
//override fun ~ <U>(p: => Parser<U>): Parser[~[T, U]]
//= OnceParser{ (for(a <- this; b <- commit(p)) yield new ~(a,b)).named("~") }
//}
//}
