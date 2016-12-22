package BuildIn;

public class CharThunk extends Thunk<Character> {
    char value;

    public CharThunk (char value) {
        this.value = value;
    }

    @Override
    protected Character force() {
        return value;
    }
}