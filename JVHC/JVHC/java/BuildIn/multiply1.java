package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.removeThunks;

/**
 * Created by joeisaacs on 23/12/2016.
 */
public class multiply1 implements Function<Supplier<Integer>,Integer>{
    Supplier<Integer> i0;

    public multiply1(Supplier<Integer> integer) {
        this.i0 = integer;
    }

    @Override
    public Integer apply(Supplier<Integer> i1) {

        return i0.get() * i1.get();
    }
}