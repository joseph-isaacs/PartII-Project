package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class Bind0 implements Function<Supplier<IO>,Bind1> {
    Supplier<IO> ma;
    @Override
    public Bind1 apply(Supplier<IO> ma) {
        this.ma = ma;
        return new Bind1(this);
    }
}
