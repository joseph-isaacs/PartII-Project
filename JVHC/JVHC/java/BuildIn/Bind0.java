package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class Bind0 implements Function<Object,Bind1> {
    Object ma;
    @Override
    public Bind1 apply(Object ma) {
        this.ma = ma;
        return new Bind1(this);
    }
}
