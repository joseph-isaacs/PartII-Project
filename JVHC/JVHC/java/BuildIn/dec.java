package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class dec implements Function<Supplier<Integer>,Integer>, Supplier<dec> {

    @Override
    public Integer apply(Supplier<Integer> integer) {
        return (integer.get() - 1);
    }

    @Override
    public dec get() {
        return new dec();
    }
}
