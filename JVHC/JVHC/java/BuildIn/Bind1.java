package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

import static BuildIn.thunkRemover.removeThunks;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class Bind1 implements Function<Object,Supplier<IO>> {

    private final Bind0 b0;

    Bind1 (Bind0 b0) {
        this.b0 = b0;
    }

    @Override
    public Supplier<IO> apply(Object amb) {
        return (Supplier)new ObjThunk(new IO() {
            @Override
            public Object unsafePerformIO() {
                IO uma = (IO) removeThunks(b0.ma);
                Function uamb = (Function) removeThunks(amb);
                return ((IO)(removeThunks(uamb.apply(uma.unsafePerformIO())))).unsafePerformIO();
            }
        });
    }
}
