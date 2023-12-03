package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;

import java.util.Objects;

/**
 * AST Node for throwing an expression (effectively calling the given e1 with this expression as parameter)
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Throw extends Expr {
    public final Expr e1, e2;

    public Throw(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return
            new Bounce(() ->
                e1.evalExpr(env,
                    (v1) ->
                        e2.evalExpr(env,
                            (v2) -> {
                                Continuation c = v1.toCont();
                                return new Bounce(() -> c.apply(v2));
                             })));
    }

    @Override
    public String toString() {
        return "(throw " + e1.toString() + " " + e2.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Throw)) return false;
        Throw aThrow = (Throw) o;
        return e1.equals(aThrow.e1) &&
                e2.equals(aThrow.e2);
    }

    @Override
    public int hashCode() {
        return Objects.hash(e1, e2);
    }
}