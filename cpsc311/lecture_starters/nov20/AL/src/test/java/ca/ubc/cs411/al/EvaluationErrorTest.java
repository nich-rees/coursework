package ca.ubc.cs411.al;

import ca.ubc.cs411.al.eval.EvalError;
import ca.ubc.cs411.al.expr.Expr;
import ca.ubc.cs411.al.parse.ALParser;
import ca.ubc.cs411.al.parse.ParseException;
import ca.ubc.cs411.al.value.NumValue;
import ca.ubc.cs411.al.value.Value;
import org.junit.jupiter.api.Test;

import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Tests for cases where the evaluation should fail
 *
 * TODO: Add more tests as needed to check your solution!
 */
class EvaluationErrorTest {
    @Test
    void errorMultiplyTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(* 7 #t)"));
        Expr parsed = parser.Program();
        try {
            parsed.eval();
            fail();
        } catch (EvalError e) {
            // OK
        }
    }

    @Test
    void errorSub1Test() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(sub1 #f)"));
        Expr parsed = parser.Program();
        try {
            parsed.eval();
            fail();
        } catch (EvalError e) {
            // OK
        }
    }

    @Test
    void errorUnboxTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(unbox 5)"));
        Expr parsed = parser.Program();
        try {
            parsed.eval();
            fail();
        } catch (EvalError e) {
            // OK
        }
    }

    @Test
    void errorApplyTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(5 #f)"));
        Expr parsed = parser.Program();
        try {
            parsed.eval();
            fail();
        } catch (EvalError e) {
            // OK
        }
    }

    @Test
    void errorThrowTest1() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(throw #t 1)"));
        Expr parsed = parser.Program();
        try {
            parsed.eval();
            fail();
        } catch (EvalError e) {
            // OK
        }
    }

    @Test
    void errorThrowTest2() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(throw (lambda (x) x) 1)"));
        Expr parsed = parser.Program();
        try {
            parsed.eval();
            fail();
        } catch (EvalError e) {
            // OK
        }
    }

    @Test
    void errorApplyContinuationTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(letcc k (* 7 (k 80)))"));
        Expr parsed = parser.Program();
        try {
            parsed.eval();
            fail();
        } catch (EvalError e) {
            // OK
        }
    }
}
