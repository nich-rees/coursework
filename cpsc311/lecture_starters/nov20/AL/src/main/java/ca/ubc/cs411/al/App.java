package ca.ubc.cs411.al;

import ca.ubc.cs411.al.eval.EvalError;
import ca.ubc.cs411.al.parse.ALParser;

import ca.ubc.cs411.al.parse.ParseException;
import ca.ubc.cs411.al.expr.Expr;
import ca.ubc.cs411.al.parse.TokenMgrError;

public class App {
    public static void main(String[] args) {
        System.out.println("Welcome to AL!\nType \"exit\" to end.");

        ALParser alp = new ALParser(System.in);

        boolean done = false;
        do {
            try {
                System.out.print("> ");
                Expr result = alp.REPLCmd();
                if (result != null) {
                    System.out.println("" + result.eval());
                } else {
                    done = true;
                }
            } catch (ParseException e) {
                System.out.println("exception: " + e);
                alp.ReInit(System.in); // try again
            }  catch(TokenMgrError | EvalError e) {
                System.out.println("error: " + e);
                alp.ReInit(System.in); // try again
            }
            // This would throw away  the rest of an input line after a legal parse
            // aep.ReInit(System.in);
        } while(!done);

        System.out.println("Goodbye!");
    }


}
