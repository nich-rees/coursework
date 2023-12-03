package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.NumValue;

import java.util.Objects;

/**
 * AST Node for subtraction of 1 from an expression
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Sub1 extends Expr {
    public final Expr e;

    public Sub1(Expr e) {
        this.e = e;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return
            new Bounce(() ->
                e.evalExpr(env,
                    (v) ->
                        k.apply(new NumValue(v.toNumber() - 1))));
    }

    @Override
    public String toString() {
        return "(sub1 " + e.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Sub1)) return false;
        Sub1 subtract1 = (Sub1) o;
        return e.equals(subtract1.e);
    }

    @Override
    public int hashCode() {
        return Objects.hash(e);
    }
}