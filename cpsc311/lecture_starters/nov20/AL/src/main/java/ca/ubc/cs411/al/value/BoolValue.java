package ca.ubc.cs411.al.value;

import java.util.Objects;

/**
 * Value wrapping a boolean literal
 *
 * TODO: You may want to modify this class when translating the given Racket code.
 */
public class BoolValue extends Value {
    public final boolean b;

    public BoolValue(boolean b) {
        this.b = b;
    }

    @Override
    public String toString() {
        return "(bool-value " + b + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof BoolValue)) return false;
        BoolValue that = (BoolValue) o;
        return b == that.b;
    }

    @Override
    public int hashCode() {
        return Objects.hash(b);
    }

    @Override
    public boolean toBoolean() {
        return b;
    }

    @Override
    public boolean notFalse() {
        return b;
    }
}
