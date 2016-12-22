package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.removeThunks;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class putChar implements Function<Object,Supplier<IO>> {
    @Override
    public Supplier<IO> apply(Object charThunk) {
        return (Supplier)
                new ObjThunk((IO) () -> {
                    System.out.print(removeThunks(charThunk));
                    return new ObjThunk(new Unit());
                });
    }


}
