package jet.util.parsing.combinator.token


trait Tokens {
    abstract class Token(val chars: String)

    class ErrorToken(msg: String) : Token("*** error: $msg")

    object EOF : Token("<eof>")

    fun errorToken(msg: String): Token = ErrorToken(msg)


}