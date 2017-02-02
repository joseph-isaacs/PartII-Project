package BuildIn;

import java.util.function.Supplier;

/**
 * Created by joeisaacs on 01/02/2017.
 */
public class True extends Bool implements Supplier<Bool> {
    @Override
    public Bool get() {
        return new True();
    }
}
