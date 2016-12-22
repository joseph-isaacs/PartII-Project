package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 22/12/2016.
 */
public class putNewLine implements Supplier<IO> {
    @Override
    public IO get() {
        return (IO) () -> {
            System.out.print('\n');
            return new ObjThunk(new Unit());
        };
    }
}
