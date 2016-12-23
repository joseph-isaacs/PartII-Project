package BuildIn;

import java.util.function.Function;

/**
 * Created by joeisaacs on 23/12/2016.
 */
public class Seq0 implements Function<Object,Seq1> {
    @Override
    public Seq1 apply(Object o) {
        thunkRemover.removeThunks(o);
        return new Seq1();
    }
}
