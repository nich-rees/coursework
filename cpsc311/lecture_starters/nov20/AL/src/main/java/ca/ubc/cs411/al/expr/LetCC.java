package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.ContValue;

import java.util.Objects;

/**
 * AST Node for binding the current continuation to a variable name in a body expression
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class LetCC extends Expr {
    public final Var x;
    public final Expr e;

    public LetCC(Var x, Expr e) {
        this.x = x;
        this.e = e;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return new Bounce(() -> e.evalExpr(env.extend(x, new ContValue(k)), k));
    }

    @Override
    public String toString() {
        return "(letcc " + x.toString() + " " + e.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LetCC)) return false;
        LetCC letCC = (LetCC) o;
        return x.equals(letCC.x) &&
                e.equals(letCC.e);
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, e);
    }
}