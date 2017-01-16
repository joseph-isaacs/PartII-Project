package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class Seq1<B> implements Function<Supplier<B>,B> {
    @Override
    public B apply(Supplier<B> o) {
        return o.get();
    }
}
