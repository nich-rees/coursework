package ca.ubc.cs411.al;

import ca.ubc.cs411.al.expr.Expr;
import ca.ubc.cs411.al.parse.ALParser;
import ca.ubc.cs411.al.parse.ParseException;
import ca.ubc.cs411.al.value.*;
import org.junit.jupiter.api.Test;

import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests for more complex features like functions, continuations and boxes
 *
 * TODO: Add more tests as needed to check your solution!
 */
class EvaluationComplexTest {
    @Test
    void functionTest1() throws ParseException {
        ALParser parser = new ALParser(new StringReader("((lambda (x) x) 7)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(7), result);
    }

    @Test
    void functionTest2() throws ParseException {
        ALParser parser = new ALParser(new StringReader("((lambda (x) (sub1 x)) 7)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(6), result);
    }

    @Test
    void nestedFunctionTest1() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(((lambda (x) (lambda (y) x)) 6) 9)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(6), result);
    }

    @Test
    void nestedFunctonTest2() throws ParseException {
        ALParser parser = new ALParser(new StringReader("((lambda (p) (p 70)) (lambda (x) (sub1 x)))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(69), result);
    }


    @Test
    void continuationTest1() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(letcc k (* 7 (throw k 80)))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(80), result);
    }

    @Test
    void continuationTest2() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(letcc k (* (throw k 7) (throw k 80)))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(7), result);
    }

    @Test
    void continuationTest3() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(sub1 (letcc k (zero? (throw k 7))))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(6), result);
    }

    @Test
    void continuationTest4() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(sub1 (call/cc\n" +
                "                            (lambda (k) (zero? (k 7)))))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(6), result);
    }

    @Test
    void boxTest1() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(let ([b (box 5)])\n" +
                "                       (set-box! b #f))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(5), result);
    }

    @Test
    void boxTest2() throws ParseException {
        ALParser parser = new ALParser(new StringReader("((lambda (b)\n" +
                "                        (begin (set-box! b #f)\n" +
                "                               (unbox b)))\n" +
                "                      (box 77))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new BoolValue(false), result);
    }

    @Test
    void boxTest3() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(let ([b (box 5)])\n" +
                "                       (let ([a (box b)])\n" +
                "                         (begin (set-box! b a)\n" +
                "                                (set-box! a 5)\n" +
                "                                (unbox (unbox b)))))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(5), result);
    }
}
