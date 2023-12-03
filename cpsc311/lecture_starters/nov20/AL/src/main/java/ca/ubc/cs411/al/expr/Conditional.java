package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;

import java.util.Objects;

/**
 * AST Node for conditional expressions (if-then-else)
 *
 * TODO: You need to modify this class (at minimum by implementing evalExpr) when
 *       translating the given Racket code.
 */
public class Conditional extends Expr {
    public final Expr p, c, a;

    public Conditional(Expr p, Expr c, Expr a) {
        this.p = p;
        this.c = c;
        this.a = a;
    }

    @Override
    public Trampoline evalExpr(Environment env, Continuation k) {
        return
            new Bounce(() ->
                p.evalExpr(env,
                    (vp) -> {
                        if (vp.notFalse()) {
                            return c.evalExpr(env, k);
                        } else {
                            return a.evalExpr(env, k);
                        }
                    }));
    }

    @Override
    public String toString() {
        return "(if " + p.toString() + " " + c.toString() + " " + a.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Conditional)) return false;
        Conditional that = (Conditional) o;
        return p.equals(that.p) &&
                c.equals(that.c) &&
                a.equals(that.a);
    }

    @Override
    public int hashCode() {
        return Objects.hash(p, c, a);
    }
}