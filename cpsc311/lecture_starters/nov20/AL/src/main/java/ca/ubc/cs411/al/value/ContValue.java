package ca.ubc.cs411.al.value;

import ca.ubc.cs411.al.eval.Continuation;

import java.util.Objects;

/**
 * Value containing a continuation.
 *
 * TODO: You may want to modify this class when translating the given Racket code.
 */
public class ContValue extends Value {
    public final Continuation c;

    public ContValue(Continuation c) {
        this.c = c;
    }

    @Override
    public String toString() {
        return "#continuation";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ContValue)) return false;
        ContValue that = (ContValue) o;
        return c.equals(that.c);
    }

    @Override
    public int hashCode() {
        return Objects.hash(c);
    }

    @Override
    public Continuation toCont() {
        return c;
    }
}