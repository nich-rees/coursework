package ca.ubc.cs411.al.value;

import ca.ubc.cs411.al.expr.Expr;
import ca.ubc.cs411.al.expr.Var;
import ca.ubc.cs411.al.eval.Environment;

import java.util.Objects;

/**
 * Value containing a procedure value
 *
 * TODO: You may want to modify this class when translating the given Racket code.
 */
public class ProcValue extends Value {
    public final Var x;
    public final Expr e;
    public final Environment env;

    @Override
    public ProcValue toProcedure() {
        return this;
    }

    public ProcValue(Var x, Expr e, Environment env) {
        this.x = x;
        this.e = e;
        this.env = env;
    }

    @Override
    public String toString() {
        // Imitating Racket's style of printing procedures as black box here
        return "#procedure";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ProcValue)) return false;
        ProcValue that = (ProcValue) o;
        return x.equals(that.x) &&
                e.equals(that.e) &&
                env.equals(that.env);
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, e, env);
    }
}
