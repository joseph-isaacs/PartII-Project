package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.*;

/**
 * Created by joeisaacs on 22/12/2016.
 */
public class neg implements Function<Supplier<Integer>,Integer> {
    @Override
    public Integer apply(Supplier<Integer> i) {
        return -i.get();
    }
}
