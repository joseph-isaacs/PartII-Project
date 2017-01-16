package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class subtract0 implements Function<Supplier<Integer>, subtract1>, Supplier<subtract0> {

    @Override
    public subtract1 apply(Supplier<Integer> integer) {
        return new subtract1(integer);
    }

    @Override
    public subtract0 get() {
        return new subtract0();
    }
}

