package jet.util.parsing.input


open class CharSequenceReader(val source: CharSequence,
                              val offset: Int) : Reader<Char>() {


    override fun first() = if (offset < source.length) source.charAt(offset) else EofCh

    override fun rest(): CharSequenceReader = if (offset < source.length) CharSequenceReader(source, offset + 1) else this

    override val pos: Position get() = OffsetPosition(source, offset)

    override fun atEnd() = offset >= source.length

    override fun drop(n: Int): CharSequenceReader =
            CharSequenceReader(source, offset + n)


}

private fun CharArray.toCharSequence(): CharSequence {
    val thiz = this;
    return object : CharSequence {
        override fun toString(): String = thiz.toString()

        public override fun get(index: Int): Char = thiz[index]

        public override val length: Int = thiz.size
    }
}

class CharArrayReader(chars: CharArray, index: Int = 0) : CharSequenceReader(chars.toCharSequence(), index)

