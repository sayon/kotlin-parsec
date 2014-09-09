package jet.util.parsing.combinator.test;

import java.util.Collections;
import java.util.List;


public class Tester {
    abstract class A<AParam> {
        abstract class AInner<AIParam> {
            Object m(List<AParam> list) {
                return null;
            }



//            class MyList extends ArrayList<List<? super MyList>> {}

//            List<? super MyList> x = new MyList();

        }

    }

    abstract class C extends A<Character> {
        abstract AInner<Integer> cmethod();

        public C() {
            cmethod().m(Collections.singletonList('a'));
        }
    }
}