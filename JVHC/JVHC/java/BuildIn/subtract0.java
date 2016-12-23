package BuildIn;

import java.util.function.Function;

/**
 * Created by joeisaacs on 23/12/2016.
 */

public class subtract0 implements Function<Object, subtract1> {

    @Override
    public subtract1 apply(Object integer) {
        return new subtract1(integer);
    }
}

