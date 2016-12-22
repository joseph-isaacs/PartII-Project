package BuildIn;

import java.util.function.Supplier;

/**
 * Created by joeisaacs on 22/12/2016.
 */
public class thunkRemover {
    public static Object removeThunks (Object t) {
        while (t instanceof Supplier) {
            t = ((Supplier) t).get();
        }
        return t;
    }
}
