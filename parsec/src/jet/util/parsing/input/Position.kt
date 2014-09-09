package jet.util.parsing.input

import java.util.ArrayList

trait Position : Comparable<Position> {

    val line: Int get

    /** The column number referred to by the position; column numbers start at 1. */
    val column: Int get

    val lineContents: String get

    fun toString() = "$line.$column"

    val longString: String get() = lineContents + "\n" + lineContents.take(column - 1).map {
        if (it == '\t') it else ' '
    } + "^"

    override fun compareTo(other: Position): Int =
            if (line == other.line && column == other.column) 0
            else if ( line < other.line || (
            line == other.line && column < other.column
            )) -1 else 1
}


object NoPosition : Position {
    override val line: Int get() = 0
    override val column: Int get() = 0

    override val lineContents: String = "<undefined position>"

    override val longString = toString()
}

class OffsetPosition(val source: jet.CharSequence, val offset: Int) : Position {

    private val index: Array<Int> = {
        val lineStarts = ArrayList<Int>()
        lineStarts add 0
        for (i in 0..source.length()) if (source.charAt(i) == '\n')
            lineStarts add i + 1

        lineStarts add source.length
        Array<Int>(lineStarts.size(), { lineStarts get it })
    }()

    override val line: Int = {
        var lo = 0
        var hi = index.size - 1
        while (lo + 1 < hi) {
            val mid = (hi + lo) / 2
            if (offset < index[mid]) hi = mid
            else lo = mid
        }
        lo + 1
    }()

    override val column: Int get() = offset - index[line - 1] + 1

    override val lineContents: String = source.subSequence(index[line - 1], index[line]).toString()

    override fun toString() = "$line.$column"


    override fun compareTo(other: Position): Int {
        fun less(that: Position): Boolean = when (that) {
            is OffsetPosition -> this.offset < that.offset
            else -> {
                line < that.line ||
                (line == that.line && this.column < that.column)
            }
        }
        return if (line == other.line && column == other.column) 0
        else if (less(other)) -1 else 1
    }


}
