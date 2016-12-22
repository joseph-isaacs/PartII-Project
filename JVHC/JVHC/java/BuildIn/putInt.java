package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class putInt implements Function<Object,Supplier<IO>> {
    @Override
    public Supplier<IO> apply(Object intThunk) {
        return (Supplier)new ObjThunk((IO) () -> {

            System.out.print(thunkRemover.removeThunks(intThunk));
            return new ObjThunk(new Unit());
        });
    }
}
