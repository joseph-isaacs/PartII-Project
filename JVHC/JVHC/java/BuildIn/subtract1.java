package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

public class subtract1 implements Function<Supplier<Integer>,Integer> {
    Supplier<Integer> i0;

    subtract1(Supplier<Integer> i0) {
        this.i0 = i0;
    }


    @Override
    public Integer apply(Supplier<Integer> i1) {

        return i0.get() - i1.get();
    }
}

