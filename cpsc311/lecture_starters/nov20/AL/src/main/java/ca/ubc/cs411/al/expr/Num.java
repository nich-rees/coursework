package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.NumValue;

import java.util.Objects;

/**
 * AST Node wrapping (integer) number literal
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Num extends Expr {
    public final int n;

    public Num(int n) {
        this.n = n;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return new Bounce(() -> k.apply(new NumValue(n)));
    }

    @Override
    public String toString() {
        return String.valueOf(n);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Num)) return false;
        Num num = (Num) o;
        return n == num.n;
    }

    @Override
    public int hashCode() {
        return Objects.hash(n);
    }
}