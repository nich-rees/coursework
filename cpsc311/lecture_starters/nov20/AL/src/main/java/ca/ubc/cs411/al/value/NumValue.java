package ca.ubc.cs411.al.value;

import java.util.Objects;

/**
 * Value wrapping a number literal
 *
 * TODO: You may want to modify this class when translating the given Racket code.
 */
public class NumValue extends Value {
    public final int n;

    public NumValue(int n) {
        this.n = n;
    }

    @Override
    public String toString() {
        return "(num-value " + n + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof NumValue)) return false;
        NumValue that = (NumValue) o;
        return n == that.n;
    }

    @Override
    public int hashCode() {
        return Objects.hash(n);
    }

    @Override
    public int toNumber() {
        return n;
    }

    @Override
    public BoolValue isZero() {
        return new BoolValue(n == 0);
    }
}
