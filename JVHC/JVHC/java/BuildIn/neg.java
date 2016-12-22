package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.*;

/**
 * Created by joeisaacs on 22/12/2016.
 */
public class neg implements Function<Object,Supplier<Integer>> {
    @Override
    public Supplier<Integer> apply(Object integerSupplier) {
        return new IntThunk(-(Integer)removeThunks(integerSupplier));
    }
}
