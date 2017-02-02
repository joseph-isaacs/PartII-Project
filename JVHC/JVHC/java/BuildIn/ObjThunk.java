package BuildIn;

public class ObjThunk<T> extends Thunk<T> {
    T value;

    public ObjThunk (T value) {
        this.value = value;
    }

    @Override
    protected T force() {
        return value;
    }
}
