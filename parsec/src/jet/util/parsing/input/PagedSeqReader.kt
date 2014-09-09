package jet.util.parsing.input


class PagedSeqReader(val source: PagedCharSeq,
                     val offset: Int = 0) : Reader<Char>() {

    /** Returns the first element of the reader, or EofCh if reader is at its end
     */
    override fun first(): Char = if (source.isDefinedAt(offset)) source[offset] else EofCh

    /** Returns a PagedSeqReader consisting of all elements except the first
     *
     * @return If `atEnd` is `true`, the result will be `this`;
     *         otherwise, it's a `PagedSeqReader` containing the rest of input.
     */
    override fun  rest(): PagedSeqReader = if (source.isDefinedAt(offset)) PagedSeqReader(source, offset + 1) else this

    /** The position of the first element in the reader.
     */
    override val pos: Position get() = OffsetPosition(source, offset)

    /** true iff there are no more elements in this reader (except for trailing
     *  EofCh's).
     */
    override fun atEnd(): Boolean  = !source.isDefinedAt(offset)

    /** Returns an abstract reader consisting of all elements except the first
     *  `n` elements.
     */
    override fun drop(n: Int): PagedSeqReader = PagedSeqReader(source, offset + n)
}
