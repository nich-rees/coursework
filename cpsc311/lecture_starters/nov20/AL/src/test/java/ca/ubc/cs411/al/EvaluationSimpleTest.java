package ca.ubc.cs411.al;

import ca.ubc.cs411.al.eval.Environment;
import ca.ubc.cs411.al.expr.Expr;
import ca.ubc.cs411.al.expr.Num;
import ca.ubc.cs411.al.expr.Var;
import ca.ubc.cs411.al.parse.ALParser;
import ca.ubc.cs411.al.parse.ParseException;
import ca.ubc.cs411.al.value.*;
import org.junit.jupiter.api.Test;

import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests for the most simple functionality and base cases of evaluation
 *
 * TODO: Add more tests as needed to check your solution!
 */
class EvaluationSimpleTest {
    @Test
    void simpleNumTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("7"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(7), result);
    }

    @Test
    void simpleBoolTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("#f"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new BoolValue(false), result);
    }

    @Test
    void simpleLambdaTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(lambda (x) 5)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new ProcValue(new Var("x"), new Num(5), new Environment()), result);
    }

    @Test
    void simpleRecTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(rec f (n) n)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(result.toString(), "#procedure");
    }

    @Test
    void simpleBoxTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(box 5)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new BoxValue(new NumValue(5)), result);
    }

    @Test
    void simpleContinuationTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(letcc k k)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(result.toString(), "#continuation");
    }

    @Test
    void simpleMultiplyTest() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(* 7 8)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(56), result);
    }

    @Test
    void simpleSub1Test() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(sub1 23)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(22), result);
    }

    @Test
    void simpleIsZeroTest1() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(zero? #t)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new BoolValue(false), result);
    }

    @Test
    void simpleIsZeroTest2() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(zero? 7)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new BoolValue(false), result);
    }

    @Test
    void simpleIsZeroTest3() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(zero? 0)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new BoolValue(true), result);
    }

    @Test
    void simpleBeginTest1() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(begin 1 2)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(2), result);
    }

    @Test
    void simpleBeginTest2() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(begin 1 2 #f)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new BoolValue(false), result);
    }

    @Test
    void simpleBeginTest3() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(begin 1 2 #f 4 5 #t 7 8)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(8), result);
    }

    @Test
    void simpleConditionalTest1() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(if #t 1 2)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(1), result);
    }

    @Test
    void simpleConditionalTest2() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(if #f 1 2)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(2), result);
    }

    @Test
    void simpleConditionalTest3() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(if (box 1) 1 2)"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(1), result);
    }

    @Test
    void simpleLetTest3() throws ParseException {
        ALParser parser = new ALParser(new StringReader("(let ([q 9]) (* q q))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(81), result);
    }
}
