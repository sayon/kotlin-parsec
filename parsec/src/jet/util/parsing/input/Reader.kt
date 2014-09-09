package jet.util.parsing.input

abstract class Reader<in T> {

    fun source(): CharSequence = throw NoSuchMethodError("not a char sequence reader")

    fun offset(): Int = throw NoSuchMethodError("not a char sequence reader")

    abstract fun first(): T

    abstract fun rest(): Reader<T>

    open fun drop(n: Int): Reader<T> {
        var r: Reader<T> = this
        var cnt = n
        while (cnt > 0) {
            r = r.rest(); cnt -= 1
        }
        return r
    }

    abstract val pos: Position get

    abstract fun atEnd(): Boolean
}
