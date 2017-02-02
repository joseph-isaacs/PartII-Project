package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class Bind0 <T> implements Function<Supplier<Supplier<IO<T>>>,Bind1>, Supplier<Bind0> {
    Supplier<Supplier<IO<T>>> ma;

    @Override
    public Bind1 apply(Supplier<Supplier<IO<T>>> ma) {
        this.ma = ma;
        return new Bind1<>(this);
    }

    @Override
    public Bind0 get() {
        return new Bind0();
    }
}
