package BuildIn;

import java.util.function.Function;

/**
 * Created by joeisaacs on 21/12/2016.
 */
public interface IO<R> {
//    private S computation;
//    IO(Function computation) {
//        this.computation = computation;
//    }
    R unsafePerformIO();

}
