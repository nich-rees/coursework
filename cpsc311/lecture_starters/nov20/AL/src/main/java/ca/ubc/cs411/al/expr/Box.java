package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.BoxValue;

import java.util.Objects;

/**
 * AST Node for creating a boxed value that can be overwritten using "set-box!".
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Box extends Expr {
    public final Expr e;

    public Box(Expr e) {
        this.e = e;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return
            new Bounce(() ->
                e.evalExpr(env, (v) ->
                    k.apply(new BoxValue(v))));
    }

    @Override
    public String toString() {
        return "(box " + e.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Box)) return false;
        Box box = (Box) o;
        return e.equals(box.e);
    }

    @Override
    public int hashCode() {
        return Objects.hash(e);
    }
}