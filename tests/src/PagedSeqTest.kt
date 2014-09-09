package jet.util.parsing.combinator.tests

import jet.util.parsing.input.PagedCharSeq
import java.io.StringReader
import kotlin.test.assertEquals


public fun main(args: Array<String>) {

    val str = "qwertyuiopasdfghjklzxcvbnm"
    val ps = PagedCharSeq.fromReader(StringReader(str))
    println(str)
    println(ps.toString())

    assertEquals(ps.toString(), str)

}

