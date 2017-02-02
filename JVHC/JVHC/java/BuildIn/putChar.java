package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;


public class putChar implements Function<Supplier<Character>,Supplier<IO>>, Supplier<putChar> {
    @Override
    public Supplier<IO> apply(Supplier<Character> charThunk) {
        return new ObjThunk<>( () -> {
            System.out.print(charThunk.get());
            return new ObjThunk<>(new Unit());
        });
    }


    @Override
    public putChar get() {
        return new putChar();
    }
}
