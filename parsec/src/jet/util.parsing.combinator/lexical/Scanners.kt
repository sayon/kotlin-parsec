package jet.util.parsing.combinator.lexical

import jet.util.parsing.combinator.token.Tokens.Token
import jet.util.parsing.combinator.Parsers.Parser
import jet.util.parsing.input.Reader
import jet.util.parsing.combinator.token.Tokens
import jet.util.parsing.input.Position
import jet.util.parsing.input.CharArrayReader
import jet.util.parsing.combinator.Parsers.Success
import jet.util.parsing.combinator.Parsers.NoSuccess


abstract class Scanner(val input: Reader<Char>) : Reader<Token>() {
    abstract fun errorToken(msg: String): Token

    /** A parser that produces a token (from a stream of characters). */
    abstract fun token(): Parser<Token>

    /** A parser for white-space -- its result will be discarded. */
    abstract fun whitespace(): Parser<Any>


    private var tok: Token? = null

    private var rest1: Reader<Token>? = null
    private var rest2: Reader<Token>? = null


    {
        val wspaceMatch = whitespace().apply(input)
        when (wspaceMatch) {
            is Success<Any> -> {
                val t = token().apply(wspaceMatch.next)
                when (t) {
                    is Success<Token> -> {
                        tok = t.result; rest1 = wspaceMatch.next; rest2 = t.next
                    }
                    is NoSuccess -> {
                        tok = errorToken(t.msg); rest1 = t.next; rest2 = skip(t.next)
                    }
                }

            }
            is NoSuccess -> {
                tok = errorToken(wspaceMatch.msg); rest1 = wspaceMatch.next; rest2 = skip(wspaceMatch.next)
            }

        }
    }


    override fun first(): Tokens.Token {
        throw UnsupportedOperationException()
    }
    override fun rest(): Reader<Tokens.Token> {
        throw UnsupportedOperationException()
    }
    override val pos: Position = throw UnsupportedOperationException()

    override fun atEnd(): Boolean {
        throw UnsupportedOperationException()
    }
}


private fun skip(input: Reader<Char>) = if (input.atEnd()) input else input.rest()


}

//private val (tok, rest1, rest2) = whitespace(in) match {
//case Success(_, in1) =>
//token(in1) match {
//case Success(tok, in2) => (tok, in1, in2)
//case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
//}
//case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
//}
//private def skip(in: Reader[Char]) = if (in.atEnd) in else in.rest
//
//override def source: java.lang.CharSequence = in.source
//override def offset: Int = in.offset
//def first = tok
//def rest = new Scanner(rest2)
//def pos = rest1.pos
//def atEnd = in.atEnd || (whitespace(in) match { case Success(_, in1) => in1.atEnd case _ => false })
//}
