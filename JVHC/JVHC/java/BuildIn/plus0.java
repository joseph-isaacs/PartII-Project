package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class plus0 implements Function<Supplier<Integer>,plus1>, Supplier<plus0> {

    @Override
    public plus1 apply(Supplier<Integer> integer) {
        return new plus1(integer);
    }

    @Override
    public plus0 get() {
        return new plus0();
    }
}
