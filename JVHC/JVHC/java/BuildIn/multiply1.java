package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.removeThunks;

/**
 * Created by joeisaacs on 23/12/2016.
 */
public class multiply1 implements Function<Object,Object>{
    Object i0;

    public multiply1(Object integer) {
        this.i0 = integer;
    }

    @Override
    public Supplier<Integer> apply(Object i1) {

        return new IntThunk((int) removeThunks(i0) * (int)removeThunks(i1));
    }
}