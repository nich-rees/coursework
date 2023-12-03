package ca.ubc.cs411.al;

import ca.ubc.cs411.al.expr.Expr;
import ca.ubc.cs411.al.parse.ALParser;
import ca.ubc.cs411.al.parse.ParseException;
import org.junit.jupiter.api.Test;

import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Basic tests for the parser
 *
 * You do not need to modify this file!
 */
class ParserTest {

  String parse(String exp) throws ParseException {
    ALParser parser = new ALParser(new StringReader(exp));
    Expr parsed = parser.Program();
    return parsed.toString();
  }

  void assertParsed(String exp) throws ParseException {
    assertEquals(exp, parse(exp));
  }

  @Test
  void parseMultiplyTest() throws ParseException {
    assertParsed("(* 3 (* 2 1))");
  }

  @Test
  void parseSub1Test() throws ParseException {
    assertParsed("(sub1 (sub1 2))");
  }

  @Test
  void parseIsZeroTest() throws ParseException {
    assertParsed("(zero? 2)");
  }

  @Test
  void parseLetCCTest() throws ParseException {
    assertParsed("(letcc x (* 1 2))");
  }

  @Test
  void parseThrowTest() throws ParseException {
    assertParsed("(throw x 9)");
  }

  @Test
  void parseLambdaTest() throws ParseException {
    assertParsed("(lambda (x) (* x 3))");
  }

  @Test
  void parseBoxTest() throws ParseException {
    assertParsed("(box (box 7))");
  }

  @Test
  void parseUnboxTest() throws ParseException {
    assertParsed("(unbox (box 5))");
  }

  @Test
  void parseSetBoxTest() throws ParseException {
    assertParsed("(set-box! x (box (* 1 2)))");
  }

  @Test
  void parseApplyTest() throws ParseException {
    assertParsed("(5 (lambda (x) (* x 5)))");
  }

  @Test
  void parseIfTest() throws ParseException {
    assertParsed("(if (zero? 1) 1 (* 2 5))");
  }
}
