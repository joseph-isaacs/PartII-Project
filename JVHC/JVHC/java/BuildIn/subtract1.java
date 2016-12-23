package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.removeThunks;


public class subtract1 implements Function<Object,Object> {
    Object i0;

    subtract1(Object i0) {
        this.i0 = i0;
    }


    @Override
    public Supplier<Integer> apply(Object i1) {

        return new IntThunk((int) removeThunks(i0) - (int)removeThunks(i1));
    }
}

