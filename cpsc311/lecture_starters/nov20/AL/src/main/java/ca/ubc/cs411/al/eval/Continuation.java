package ca.ubc.cs411.al.eval;

import ca.ubc.cs411.al.eval.trampoline.Trampoline;
import ca.ubc.cs411.al.value.Value;

import java.util.function.Function;

/**
 * Interface used to simulate continuations in Java
 *
 * You do not need to modify this file!
 */
public interface Continuation extends Function<Value, Trampoline> {
}
