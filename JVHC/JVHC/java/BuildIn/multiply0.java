package BuildIn;

import java.util.function.Function;


public class multiply0 implements Function<Object,multiply1> {

    @Override
    public multiply1 apply(Object integer) {
        return new multiply1(integer);
    }
}

