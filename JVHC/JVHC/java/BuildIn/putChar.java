package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.removeThunks;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class putChar implements Function<Supplier<Character>,IO>, Supplier<putChar> {
    @Override
    public IO apply(Supplier<Character> charThunk) {
        return
                (IO) () -> {
                    System.out.print(charThunk.get());
                    return new ObjThunk(new Unit());
                };
    }


    @Override
    public putChar get() {
        return new putChar();
    }
}
