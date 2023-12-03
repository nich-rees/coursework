package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.BoolValue;

import java.util.Objects;

/**
 * AST Node wrapping a boolean literal
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Bool extends Expr {
    public final boolean b;

    public Bool(boolean b) {
        this.b = b;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return new Bounce(() -> k.apply(new BoolValue(b)));
    }

    @Override
    public String toString() {
        return String.valueOf(b);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Bool)) return false;
        Bool bool = (Bool) o;
        return b == bool.b;
    }

    @Override
    public int hashCode() {
        return Objects.hash(b);
    }
}