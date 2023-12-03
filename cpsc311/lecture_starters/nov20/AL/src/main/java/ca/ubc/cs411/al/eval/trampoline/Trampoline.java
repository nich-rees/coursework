package ca.ubc.cs411.al.eval.trampoline;

import ca.ubc.cs411.al.value.Value;

/**
 * Base interface for the trampoline call classes (see evalExpr examples to see how it is used)
 *
 * You do not need to modify this file!
 */
public abstract class Trampoline {
    /**
     * Starts the trampolining (see evalExpr examples to see how it is used)
     *
     * @param t Initial trampoline
     * @return Return value of the final dismount call
     */
    public static Value loop(Trampoline t) {
        while (t instanceof Bounce) {
            Bounce c = (Bounce)t;
            t = c.p.get();
        }
        assert t instanceof Dismount;
        Dismount d = (Dismount)t;
        return d.v;
    }
}
