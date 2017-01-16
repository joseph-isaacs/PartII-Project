package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class Seq0 <A> implements Function<Supplier<A>,Seq1>, Supplier<Seq0> {
    @Override
    public Seq1 apply(Supplier<A> o) {
        o.get();
        System.out.println(o.get());
        return new Seq1();
    }

    @Override
    public Seq0 get() {
        return new Seq0();
    }
}
