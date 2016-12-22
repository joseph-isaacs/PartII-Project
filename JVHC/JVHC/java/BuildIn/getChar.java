package BuildIn;

import java.io.IOException;
import java.util.Scanner;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public class getChar implements Supplier<IO> {
    @Override
    public IO get() {
        return (IO) () -> {
            Scanner reader = new Scanner(System.in);
            char c = reader.next(".").charAt(0);
            return new CharThunk(c);
        };
    }
}
