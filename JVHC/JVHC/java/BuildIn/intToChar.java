package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.removeThunks;

/**
 * Created by joeisaacs on 22/12/2016.
 */
public class intToChar implements Function<Object,Supplier<Character>> {

    @Override
    public Supplier<Character> apply(Object integerSupplier) {
        return new CharThunk((char) ((Integer)removeThunks(integerSupplier)).shortValue());
    }
}
