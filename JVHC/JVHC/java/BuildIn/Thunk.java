package BuildIn;

import java.util.function.Supplier;

public abstract class Thunk<T> implements Supplier<T> {
    private T value;
    protected abstract T force();
    @Override
    public T get() {
        if ( value == null ) {
            return value = force();
        }
        return value;
    }
}
