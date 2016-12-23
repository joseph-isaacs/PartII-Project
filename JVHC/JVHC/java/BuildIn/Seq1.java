package BuildIn;

import java.util.function.Function;

/**
 * Created by joeisaacs on 23/12/2016.
 */
public class Seq1 implements Function<Object,Object> {
    @Override
    public Object apply(Object o) {
        return o;
    }
}
