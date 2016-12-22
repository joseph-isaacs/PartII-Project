package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class Bind1 implements Function<Function<Object,Supplier<IO>>,Supplier<IO>> {

    private final Bind0 b0;

    Bind1 (Bind0 b0) {
        this.b0 = b0;
    }

    @Override
    public Supplier<IO> apply(Function<Object,Supplier<IO>> amb) {
        return (Supplier)new ObjThunk(new IO() {
            @Override
            public Object unsafePerformIO() {
                return amb.apply(b0.ma.get().unsafePerformIO()).get().unsafePerformIO();
            }
        });
    }
}
