package ca.ubc.cs411.al.eval;

/**
 * Error to be used when evaluation encounters an error
 * Note that this is an unchecked error, so it does not need to be declared via "throws ..."
 *
 * You do not need to modify this file!
 */
public class EvalError extends Error {
    public EvalError(String message) {
        super(message);
    }
}
