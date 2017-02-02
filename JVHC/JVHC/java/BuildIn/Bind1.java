package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;



public class Bind1 <A,B> implements Function<Supplier<Function<A,Supplier<IO<B>>>>,Supplier<IO<B>>> {

    private final Bind0<A> b0;

    Bind1 (Bind0<A> b0) {
        this.b0 = b0;
    }

    @Override
    public Supplier<IO<B>> apply(Supplier<Function<A,Supplier<IO<B>>>> o) {
        return new ObjThunk<>(() -> o.get().apply(b0.ma.get().get().unsafePerformIO()).get().unsafePerformIO());
    }
}
