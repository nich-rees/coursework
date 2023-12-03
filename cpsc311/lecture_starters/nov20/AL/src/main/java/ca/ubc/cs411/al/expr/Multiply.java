package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.NumValue;

import java.util.Objects;

/**
 * AST Node for multiplication of two expressions
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Multiply extends Expr {
    public final Expr e1, e2;

    public Multiply(Expr e1, Expr e2) {
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
                                (v2) -> k.apply(new NumValue(v1.toNumber() * v2.toNumber())))));
    }

    @Override
    public String toString() {
        return "(* " + e1.toString() + " " + e2.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Multiply)) return false;
        Multiply multiply = (Multiply) o;
        return e1.equals(multiply.e1) &&
                e2.equals(multiply.e2);
    }

    @Override
    public int hashCode() {
        return Objects.hash(e1, e2);
    }
}