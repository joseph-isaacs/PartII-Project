package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 01/02/2017.
 */
public class mod0 implements Function<Supplier<Integer>,mod1>, Supplier<mod0> {
    @Override
    public mod1 apply(Supplier<Integer> integer) {
        return new mod1(integer);
    }

    @Override
    public mod0 get() {
        return new mod0();
    }
}