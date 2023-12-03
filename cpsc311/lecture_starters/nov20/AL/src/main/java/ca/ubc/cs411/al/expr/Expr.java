package ca.ubc.cs411.al.expr;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.eval.trampoline.Bounce;
import ca.ubc.cs411.al.eval.trampoline.Dismount;
import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.Value;

/**
 * Abstract base class for Expr AST nodes
 *
 * You do not need to modify this file!
 */
public abstract class Expr {
    /**
     * Actual evaluation method implemented in CPS
     *
     * @param env Environment to use for evaluation
     * @param k Continuation that continues evaluation
     * @return Trampoline that contains the further computation
     */
    public abstract Trampoline evalExpr(Environment env, Continuation k);

    /**
     * Evaluates this expression to a value
     *
     * @return Result value of evaluating the expression
     * @throws ca.ubc.cs411.al.eval.EvalError if any evaluation error is encountered
     */
    public Value eval() {
        return Trampoline.loop(
                new Bounce(() ->
                        evalExpr(new Environment(), Dismount::new)));
    }
}
