package BuildIn;

import java.util.Scanner;
import java.util.function.Supplier;


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
