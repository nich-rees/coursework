package ca.ubc.cs411.al.eval;

import ca.ubc.cs411.al.expr.Var;
import ca.ubc.cs411.al.value.Value;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Environment that stores assignments of identifiers; used for evaluation
 *
 * Note that environments are read-only to match the Racket implementation style.
 * We need to create an extended copy using "extend(...)" to get a modified version.
 */
public class Environment {
    private final Map<Var, Value> environment;

    /**
     * Constructs a fresh and empty environment
     */
    public Environment() {
        this.environment = Collections.unmodifiableMap(new HashMap<>());
    }

    private Environment(Environment existingEnvironment, Var newIdent, Value newVal) {
        // We only extend the new hash-map here once and only store an unmodifiable link to prevent us from
        // accidentally modifying the environment
        HashMap<Var, Value> newEnvironment = new HashMap<>(existingEnvironment.environment);
        newEnvironment.put(newIdent, newVal);
        environment = Collections.unmodifiableMap(newEnvironment);
    }

    /**
     * Returns a copy of this environment with a new variable binding added,
     * while leaving this environment object untouched
     *
     * @param newIdent New identifier to bind in the environment
     * @param newVal New binding value for the identifier
     * @return Extended environment
     */
    public Environment extend(Var newIdent, Value newVal) {
        return new Environment(this, newIdent, newVal);
    }

    /**
     * Looks up a variable binding in the environment
     *
     * @param ident Identifier to look up in the environment
     * @return Binding for the identifier
     * @throws EvalError if identifier was not found in the environment
     */
    public Value lookup(Var ident) {
        if (environment.containsKey(ident)) {
            return environment.get(ident);
        }
        else {
            throw new EvalError("Encountered unbound variable: " + ident);
        }
    }

    @Override
    public String toString() {
        return "Environment: {" + environment + "}";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Environment)) return false;
        Environment that = (Environment) o;
        return environment.equals(that.environment);
    }

    @Override
    public int hashCode() {
        return Objects.hash(environment);
    }
}
