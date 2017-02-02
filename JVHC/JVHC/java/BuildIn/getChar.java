package BuildIn;

import java.util.Scanner;
import java.util.function.Supplier;


public class getChar implements Supplier<Supplier<IO>> {
    @Override
    public Supplier<IO> get() {
        return new ObjThunk<> (() -> {
            Scanner reader = new Scanner(System.in);
            char c = reader.next(".").charAt(0);
            return new CharThunk(c);
        });
    }
}
