package jet.util.parsing.input


//Tested


class PagedCharSeq protected(
        private val more: (CharArray, Int, Int) -> Int,
        private val first1: CharPage = CharPage(0),
        private val start: Int = 0,
        private val end: Int = PagedCharSeq.UndeterminedEnd) : CharSequence
{
    class object {
        val UndeterminedEnd = Integer.MAX_VALUE
        fun fromReader(source: java.io.Reader): PagedCharSeq = PagedCharSeq({(charArray, y, z) -> source.read(charArray, y, z) })
    }

    private var current: CharPage = first1

    private val latest: CharPage get() = first1.latest

    private fun addMore() = latest.addMore(more)

    private fun page(absindex: Int): CharPage {
        if (absindex < current.start)
            current = first1
        while (absindex >= current.end && current.next != null)
            current = current.next!!
        while (absindex >= current.end && !current.isLast) {
            current = addMore()
        }
        return current
    }

    /** The length of the paged sequence
     *  @note Calling this method will force the entire sequence to be read.
     */
    override val length: Int  get() {
        while (!latest.isLast) addMore()
        return Math.min(latest.end, end) - start
    }

    /** The element at position `index`.
     */
    override fun get(index: Int): Char =
            if (isDefinedAt(index)) page(index + start)[index + start]
            else throw IndexOutOfBoundsException(index.toString())

    fun isDefinedAt(index: Int) =
            index >= 0 && index < end - start && {
                val p = page(index + start); index + start < p.end
            }()

    fun slice(_start: Int, _end: Int): PagedCharSeq {
        page(start)
        val s = start + _start
        val e = if (_end == UndeterminedEnd) _end else start + _end
        var f = first1
        while (f.end <= s && !f.isLast) f = f.next!!

        return PagedCharSeq(more, f, s, e)
    }
    fun slice(start: Int): PagedCharSeq = slice(start, UndeterminedEnd)

    override fun toString(): String {
        val sb = StringBuilder()
        for ( i in 0..length-1) sb.append(this[i])
        return sb.toString()
    }
}


private class CharPage(val num: Int) {

    private final val PageSize = 4096

    /** The next page in the sequence */
    var next: CharPage? = null

    /** A later page in the sequence, serves a cache for pointing to last page */
    var later: CharPage = this

    /** The number of elements read into this page */
    var filled: Int = 0

    /** Set true if the current page is the last in the sequence or if
     *   the `more` function returned -1 signalling end of input. */
    var isLast: Boolean = false

    /** The element array */
    final val data = CharArray(PageSize)

    /** The index of the first element in this page relative to the whole sequence */
    final val start = num * PageSize

    /** The index of the element following the last element in this page relative
     *  to the whole sequence */
    val end: Int get() = start + filled

    /** The last page as currently present in the sequence; This can change as more
     *  elements get appended to the sequence.  */
    final val latest: CharPage get()  {
        if (later.next != null) later = later.next!!.latest
        return later
    }

    fun get(index: Int): Char {
        if (index < start || index - start >= filled) throw IndexOutOfBoundsException(index.toString())
        return data[index - start]
    }

    /** Produces more elements by calling `more` and adds them on the current page,
     *  or fills a subsequent page if current page is full.
     *  @note If current page is full, it is the last one in the sequence.  */
    final fun addMore(more: (CharArray, Int, Int) -> Int): CharPage =
            if (filled == PageSize) {
                next = CharPage(num + 1)
                next!!.addMore(more)
            } else {
                val count = more(data, filled, PageSize - filled)
                if (count < 0) isLast = true
                else filled += count
                this
            }
}
