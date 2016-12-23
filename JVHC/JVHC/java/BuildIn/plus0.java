package BuildIn;

import java.util.function.Function;


public class plus0 implements Function<Object,plus1> {

    @Override
    public plus1 apply(Object integer) {
        return new plus1(integer);
    }
}
