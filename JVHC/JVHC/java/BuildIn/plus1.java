package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 20/12/2016.
 */
public class plus1 implements Function<Supplier<Integer>,Supplier<Integer>>{
    Supplier<Integer> i0;

    plus1(Supplier<Integer> s) {
        this.i0 = s;
    }


    @Override
    public Supplier<Integer> apply(Supplier<Integer> integerSupplier) {
        return new IntThunk(integerSupplier.get() + i0.get());
    }
}
