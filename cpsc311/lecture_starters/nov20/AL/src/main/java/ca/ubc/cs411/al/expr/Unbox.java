package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.BoxValue;

import java.util.Objects;

/**
 * AST Node for unwrapping a box's content
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Unbox extends Expr {
    public final Expr e;

    public Unbox(Expr e) {
        this.e = e;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return
            new Bounce(() ->
                e.evalExpr(env,
                        (v) ->  {
                            BoxValue b = v.toBox();
                            return new Bounce(() -> k.apply(b.v));
                        }));
    }

    @Override
    public String toString() {
        return "(unbox " + e.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Unbox)) return false;
        Unbox unbox = (Unbox) o;
        return e.equals(unbox.e);
    }

    @Override
    public int hashCode() {
        return Objects.hash(e);
    }
}