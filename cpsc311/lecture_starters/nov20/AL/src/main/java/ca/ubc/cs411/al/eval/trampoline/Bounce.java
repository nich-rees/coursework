package ca.ubc.cs411.al.eval.trampoline;

import java.util.function.Supplier;

/**
 * Class used adding a new trampoline call (see evalExpr examples to see how it is used)
 *
 * You do not need to modify this file!
 */
public class Bounce extends Trampoline {
    public final Thunk p;

    public Bounce(Thunk p) {
        this.p = p;
    }
}
