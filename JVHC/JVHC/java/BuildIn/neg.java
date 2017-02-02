package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class neg implements Function<Supplier<Integer>,Integer>, Supplier<neg>{
    @Override
    public Integer apply(Supplier<Integer> i) {
        return -i.get();
    }

    @Override
    public neg get() {
        return new neg();
    }
}
