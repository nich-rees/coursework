package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;

import java.util.Objects;

/**
 * AST Node for checking if an expression is zero
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class IsZero extends Expr {
    public final Expr e;

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return
            new Bounce(() ->
                e.evalExpr(env,
                    (v) -> k.apply(v.isZero())));
    }

    public IsZero(Expr e) {
        this.e = e;
    }

    @Override
    public String toString() {
        return "(zero? " + e.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IsZero)) return false;
        IsZero isZero = (IsZero) o;
        return e.equals(isZero.e);
    }

    @Override
    public int hashCode() {
        return Objects.hash(e);
    }
}