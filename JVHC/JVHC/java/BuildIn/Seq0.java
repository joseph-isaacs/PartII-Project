package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class Seq0 <A> implements Function<Supplier<A>,Seq1>, Supplier<Seq0> {
    Supplier<A> sa;

    @Override
    public Seq1 apply(Supplier<A> o) {
        sa = o;
        sa.get();
        System.out.println(sa.get());
        return new Seq1(this);
    }

    @Override
    public Seq0 get() {
        return new Seq0();
    }
}
