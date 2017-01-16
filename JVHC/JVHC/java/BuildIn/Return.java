package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

public class Return <T> implements Function <Supplier<T>,IO<T>> {

    @Override
    public IO<T> apply(Supplier<T> objectSupplier) {

        return () -> objectSupplier.get();
    }
}
