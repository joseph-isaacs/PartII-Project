package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class Return implements Function <Object,Supplier<IO>> {

    @Override
    public Supplier<IO> apply(Object objectSupplier) {

        return (Supplier)new ObjThunk((IO) () -> objectSupplier);
    }
}
