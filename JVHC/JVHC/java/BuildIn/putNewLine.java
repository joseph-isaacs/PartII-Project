package BuildIn;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by joeisaacs on 22/12/2016.
 */
public class putNewLine implements Supplier<Supplier<IO>> {
    @Override
    public Supplier<IO> get() {
        return new ObjThunk<>( () -> {
            System.out.print('\n');
            return new Unit();
        });
    }
}


//    public Supplier<IO> apply(Supplier<Character> charThunk) {
//        return new ObjThunk<>( () -> {
//            System.out.print(charThunk.get());
//            return new Unit();
//        });
//    }