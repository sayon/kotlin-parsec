package jet.util.parsing.combinator.lexical

import jet.util.parsing.combinator.token.Tokens

/** This component complements the `Scanners` component with
 *  common operations for lexical parsers.
 *
 *  Refer to [[scala.util.parsing.combinator.lexical.StdLexical]]
 *  for a concrete implementation for a simple, Scala-like language.
 *
 * @author Martin Odersky, Adriaan Moors
 */
//abstract class Lexical : Scanners, Tokens {

/** A character-parser that matches a letter (and returns it).*/
//letter = elem("letter", _.isLetter)
//
///** A character-parser that matches a digit (and returns it).*/
//def digit = elem("digit", _.isDigit)
//
///** A character-parser that matches any character except the ones given in `cs` (and returns it).*/
//def chrExcept(cs: Char*) = elem("", ch => (cs forall (ch != _)))
//
///** A character-parser that matches a white-space character (and returns it).*/
//def whitespaceChar = elem("space char", ch => ch <= ' ' && ch != EofCh)
//}
