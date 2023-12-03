package ca.ubc.cs411.al.eval.trampoline;

import ca.ubc.cs411.al.value.Value;

/**
 * Class used for exiting the trampoline call chain (see evalExpr examples to see how it is used)
 *
 * You do not need to modify this file!
 */
public class Dismount extends Trampoline {
    public final Value v;

    public Dismount(Value v) {
        this.v = v;
    }
}
