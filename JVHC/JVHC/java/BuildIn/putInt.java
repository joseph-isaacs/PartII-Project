package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class putInt implements Function<Supplier<Integer>,IO>, Supplier<putInt> {
    @Override
    public IO apply(Supplier<Integer> intThunk) {
        return (IO) () -> {
            System.out.print((int) intThunk.get());
            return new ObjThunk(new Unit());
        };
    }

    @Override
    public putInt get() {
        return new putInt();
    }
}
