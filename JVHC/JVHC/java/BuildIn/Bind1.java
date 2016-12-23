package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.removeThunks;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class Bind1 <A,B> implements Function<Supplier<Function<A,IO<B>>>,IO<B>> {

    private final Bind0<A> b0;

    Bind1 (Bind0<A> b0) {
        this.b0 = b0;
    }

    @Override
    public IO<B> apply(Supplier<Function<A,IO<B>>> o) {
        return () -> o.get().apply(b0.ma.get().unsafePerformIO()).unsafePerformIO();
    }
}
