package ca.ubc.cs411.al.value;

import java.util.Objects;

/**
 * Value wrapping a boxed value
 *
 * To avoid writing our own store model (and using a technique like store-passing style), we abuse
 * Java's native mutability for variables here and directly allow setting the content of the box.
 *
 * TODO: You may want to modify this class when translating the given Racket code.
 */
public class BoxValue extends Value {
    public Value v;

    public BoxValue(Value v) {
        this.v = v;
    }

    @Override
    public String toString() {
        return "(box-value " + v.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof BoxValue)) return false;
        BoxValue that = (BoxValue) o;
        return v.equals(that.v);
    }

    @Override
    public int hashCode() {
        return Objects.hash(v);
    }

    @Override
    public BoxValue toBox() {
        return this;
    }
}
