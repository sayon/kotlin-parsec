package jet.util.parsing.combinator.token

import jet.util.parsing.combinator.token.Tokens.Token


class Keyword(str: String) : Token("`$str'")

class NumericLit(str: String) : Token(str)

class StringLit(str: String) : Token(str) {
    fun toString() = "\"$chars\""
}

class Identifier(str: String) : Token(str) {
    fun toString() = "identifier " + chars
}

