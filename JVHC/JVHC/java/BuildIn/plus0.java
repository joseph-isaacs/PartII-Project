package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 20/12/2016.
 */
public class plus0 implements Function<Object,plus1> {

    @Override
    public plus1 apply(Object integer) {
        return new plus1(integer);
    }
}
