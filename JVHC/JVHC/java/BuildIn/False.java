package BuildIn;

import java.util.function.Supplier;

public class False extends Bool implements Supplier<Bool> {
    @Override
    public Bool get() {
        return new False();
    }
}
