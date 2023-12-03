package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.ProcValue;

import java.util.Objects;

/**
 * AST Node for creating a lambda function taking a single parameter and body
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Lambda extends Expr {
    public final Var x;
    public final Expr e;

    public Lambda(Var x, Expr e) {
        this.x = x;
        this.e = e;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return new Bounce(() -> k.apply(new ProcValue(x, e, env)));
    }

    @Override
    public String toString() {
        return "(lambda (" + x.toString() + ") " + e.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Lambda)) return false;
        Lambda lambda = (Lambda) o;
        return x.equals(lambda.x) &&
                e.equals(lambda.e);
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, e);
    }
}