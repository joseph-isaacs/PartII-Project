package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class Return implements Function <Object,IO> {

    @Override
    public IO apply(Object objectSupplier) {
        return () -> objectSupplier;
    }
}