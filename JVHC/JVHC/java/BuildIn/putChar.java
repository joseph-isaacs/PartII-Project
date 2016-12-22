package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class putChar implements Function<CharThunk,Supplier<IO>> {
    @Override
    public Supplier<IO> apply(CharThunk charThunk) {
        return (Supplier)
                new ObjThunk((IO) () -> {
                    System.out.print(charThunk.get());
                    return new ObjThunk(new Unit());
                });
    }
}
