package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class multiply0 implements Function<Supplier<Integer>,multiply1>, Supplier<multiply0> {

    @Override
    public multiply1 apply(Supplier<Integer> i1) {
        return new multiply1(i1);
    }

    @Override
    public multiply0 get() {
        return new multiply0();
    }
}

