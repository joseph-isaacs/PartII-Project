package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class Seq1<B> implements Function<Supplier<B>,B> {
    Seq0 seq0;

    Seq1(Seq0 seq0) {
        this.seq0 = seq0;
    }

    @Override
    public B apply(Supplier<B> o) {
       // seq0.sa.get();
        System.out.println(seq0.sa.get());
        return o.get();
    }
}
