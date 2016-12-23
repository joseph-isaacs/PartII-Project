package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.removeThunks;

/**
 * Created by joeisaacs on 23/12/2016.
 */
public class Seq0 <A> implements Function<Supplier<A>,Seq1>, Supplier<Seq0> {
    @Override
    public Seq1 apply(Supplier<A> o) {
        o.get();
        System.out.println(removeThunks(o));
        return new Seq1();
    }

    @Override
    public Seq0 get() {
        return new Seq0();
    }
}
