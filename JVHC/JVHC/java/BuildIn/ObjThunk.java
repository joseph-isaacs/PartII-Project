package BuildIn;

public class ObjThunk extends Thunk<Object> {
    Object value;

    public ObjThunk (Object value) {
        this.value = value;
    }

    @Override
    protected Object force() {
        return value;
    }
}
