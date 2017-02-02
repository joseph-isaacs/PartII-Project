package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

public class intLT0 implements Function<Supplier<Integer>,intLT1>, Supplier<intLT0> {

    @Override
    public intLT1 apply(Supplier<Integer> integer) {
        return new intLT1(integer);
    }

    @Override
    public intLT0 get() {
        return new intLT0();
    }
}
