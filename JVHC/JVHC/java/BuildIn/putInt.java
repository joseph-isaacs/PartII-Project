package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class putInt implements Function<Supplier<Integer>,Supplier<IO>>, Supplier<putInt> {
    @Override
    public Supplier<IO> apply(Supplier<Integer> intThunk) {
        return new ObjThunk<>( () -> {
            System.out.print((int) intThunk.get());
            return new ObjThunk<>(new Unit());
        });
    }


    @Override
    public putInt get() {
        return new putInt();
    }
}
