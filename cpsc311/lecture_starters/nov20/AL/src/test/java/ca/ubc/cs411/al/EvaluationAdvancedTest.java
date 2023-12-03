package ca.ubc.cs411.al;

import ca.ubc.cs411.al.expr.Expr;
import ca.ubc.cs411.al.parse.ALParser;
import ca.ubc.cs411.al.parse.ParseException;
import ca.ubc.cs411.al.value.NumValue;
import ca.ubc.cs411.al.value.Value;
import org.junit.jupiter.api.Test;

import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Advanced tests that combine multiple complex features and use recursion
 *
 * TODO: Add more tests as needed to check your solution!
 */
class EvaluationAdvancedTest {
    @Test
    void CallCCMeetsBoxTest() throws ParseException {
        ALParser parser =
            new ALParser(
                new StringReader(
                    "(let ([b (box #f)])\n" +
                "         (let ([q (* (* 9\n" +
                "                        (call/cc (lambda (k)\n" +
                "                                    (begin\n" +
                "                                      (set-box! b k)\n" +
                "                                      10))))\n" +
                "            11)])\n" +
                "          (let ([v (unbox b)])\n" +
                "            (if v\n" +
                "                (begin (set-box! b #f)\n" +
                "                       (v 17))\n" +
                "                q))))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(1683), result);
    }

    @Test
    void FactTest() throws ParseException {
        ALParser parser =
            new ALParser(
                new StringReader("(let ([fact (rec f (n)\n" +
                "                                  (if (zero? n)\n" +
                "                                      1\n" +
                "                                      (* n (f (sub1 n)))))])\n" +
                "                      (fact 5))"));
        Expr parsed = parser.Program();
        Value result = parsed.eval();
        assertEquals(new NumValue(120), result);
    }
}
