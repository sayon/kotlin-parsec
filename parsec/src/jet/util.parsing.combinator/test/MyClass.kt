//package jet.util.parsing.combinator.test
//
//
//
//abstract class A<AParam> {
//    inner abstract class AInner<IParam>() {
//        fun m(list : List<AParam>) = null
//    }
//}
//
//
//abstract class C : A<Char> () {
//    abstract fun cmethod() : A.AInner<Int>;
//
//    val a = cmethod().m(listOf('a'))
//
//}


//            class MyList extends ArrayList<List<? super MyList>> {}

//            List<? super MyList> x = new MyList();

package test

class MyC<T>() {
    var field: T;
    {
        field = null as T

    }

    fun get(): T {
        return field;
    }
}

fun main(args: Array<String>) {
    var c: MyC<Integer> = MyC();
    println( c.get() )
}