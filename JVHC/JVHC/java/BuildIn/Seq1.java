package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 23/12/2016.
 */
public class Seq1<B> implements Function<Supplier<B>,B> {
    @Override
    public B apply(Supplier<B> o) {
        return o.get();
    }
}
