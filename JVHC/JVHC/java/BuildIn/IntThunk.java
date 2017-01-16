package BuildIn;


public class IntThunk extends Thunk<Integer> {
    int value;

    public IntThunk (int value) {
        this.value = value;
    }

    @Override
    protected Integer force() {
        return value;
    }
}
