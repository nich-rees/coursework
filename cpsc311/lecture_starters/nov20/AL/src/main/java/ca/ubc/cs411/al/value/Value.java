package ca.ubc.cs411.al.value;

import ca.ubc.cs411.al.eval.Continuation;
import ca.ubc.cs411.al.eval.EvalError;

/**
 * Abstract base class for Expr values returned by the interpreter
 *
 * TODO: You may want to modify this class (and its subclasses)
 *       when translating the given Racket code.
 */
public abstract class Value {
    public int toNumber() {
        throw new EvalError("Expected a number, but received " + this.toString());
    }
    public boolean toBoolean() {
        throw new EvalError("Expected a boolean, but received " + this.toString());
    }
    public Continuation toCont() {
        throw new EvalError("Expected a continuation, but received " + this.toString());
    }
    public ProcValue toProcedure() {
        throw new EvalError("Expected a procedure, but received " + this.toString());
    }
    public BoxValue toBox() {
        throw new EvalError("Expected a box, but received " + this.toString());
    }
    public boolean notFalse() { return true; }
    public BoolValue isZero() { return new BoolValue(false); }
}
