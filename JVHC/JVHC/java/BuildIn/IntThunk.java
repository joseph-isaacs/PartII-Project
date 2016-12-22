package BuildIn;

/**
 * Created by joeisaacs on 20/12/2016.
 */
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
