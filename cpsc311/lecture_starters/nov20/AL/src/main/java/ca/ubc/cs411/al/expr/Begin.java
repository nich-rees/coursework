package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;

import java.util.Objects;

/**
 * AST Node for evaluating two expressions sequentially (for their side effects)
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Begin extends Expr {
    public final Expr e1, e2;

    public Begin(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return
            new Bounce(() ->
                e1.evalExpr(env,
                    (v1) ->
                         e2.evalExpr(env, k)));
    }

    @Override
    public String toString() {
        return "(begin " + e1.toString() + " " + e2.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Begin)) return false;
        Begin begin = (Begin) o;
        return e1.equals(begin.e1) &&
                e2.equals(begin.e2);
    }

    @Override
    public int hashCode() {
        return Objects.hash(e1, e2);
    }
}