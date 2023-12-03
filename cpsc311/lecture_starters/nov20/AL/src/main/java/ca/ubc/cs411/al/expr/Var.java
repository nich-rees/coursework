package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;

import java.util.Objects;

/**
 * AST Node for variable identifiers
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Var extends Expr {
    public final String x;

    public Var(String x) {
        this.x = x;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return new Bounce(() -> k.apply(env.lookup(this)));
    }

    @Override
    public String toString() {
        return x;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Var)) return false;
        Var that = (Var) o;
        return x.equals(that.x);
    }

    @Override
    public int hashCode() {
        return Objects.hash(x);
    }
}