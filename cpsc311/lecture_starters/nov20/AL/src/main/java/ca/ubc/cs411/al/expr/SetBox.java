package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.BoxValue;
import ca.ubc.cs411.al.value.Value;

import java.util.Objects;

/**
 * AST Node for overwriting a e1's content with a new expression
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class SetBox extends Expr {
    public final Expr e1, e2;

    public SetBox(Expr e1, Expr newContent) {
        this.e1 = e1;
        this.e2 = newContent;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return
            new Bounce(() ->
                e1.evalExpr(env,
                    (v1) ->
                        new Bounce(() ->
                            e2.evalExpr(env, (v2) -> {
                                BoxValue box = v1.toBox();
                                Value oldVal = box.v;
                                box.v = v2;
                                return new Bounce(() -> k.apply(oldVal));
                            }
                        ))));
    }

    @Override
    public String toString() {
        return "(set-box! " + e1.toString() + " " + e2.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof SetBox)) return false;
        SetBox setBox = (SetBox) o;
        return e1.equals(setBox.e1) &&
                e2.equals(setBox.e2);
    }

    @Override
    public int hashCode() {
        return Objects.hash(e1, e2);
    }
}