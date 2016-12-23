package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class Return <T> implements Function <Supplier<T>,IO<T>> {

    @Override
    public IO<T> apply(Supplier<T> objectSupplier) {

        return () -> objectSupplier.get();
    }
}
