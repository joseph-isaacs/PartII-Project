package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

public class intEq0 implements Function<Supplier<Integer>,intEq1>, Supplier<intEq0> {

    @Override
    public intEq1 apply(Supplier<Integer> integer) {
        return new intEq1(integer);
    }

    @Override
    public intEq0 get() {
        return new intEq0();
    }
}