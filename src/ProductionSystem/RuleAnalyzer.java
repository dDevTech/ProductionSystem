/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package ProductionSystem;


import LexicAnalyzer.FDA.FDA;
import LexicAnalyzer.FDA.SemanticAction;
import LexicAnalyzer.FDA.State;
import LexicAnalyzer.FDA.StateOperationCode;
import LexicAnalyzer.Tokenizer.SymbolTable;
import LexicAnalyzer.Tokenizer.Tokenizer;
import Tools.Console;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RuleAnalyzer {
    private static FDA<Character> FDA;
    private static Tokenizer<String>tokenizer;
    private static SymbolTable variables;
    private static SymbolTable constants;
    private static boolean debug = false;
    public static void setupAnalyzer(){
        FDA = new FDA<>() {
            @Override
            public void onReadSequence(List<Character> completeSequence, State<Character> finalNode, int statusCode) {

            }

        };
        variables = new SymbolTable();
        constants = new SymbolTable();
        tokenizer = new Tokenizer<>();

        State<Character> base = new State<>("root");
        base.addTransitionFunction(RuleAnalyzer::isWhiteSpace,base,true,false);
        //Variables
        State<Character> dollar = new State<>("dollar");
        State<Character> letter1 = new State<>("letter-variable");
        State<Character> otherCharacter1 = new State<>("o.c.1");
        otherCharacter1.setFinal();
        base.addTransition('$',dollar);
        dollar.addTransitionFunction(RuleAnalyzer::isLetter,letter1,true,true);
        letter1.addTransitionFunction(RuleAnalyzer::isLetter,letter1,true,true);
        letter1.addTransitionFunction(RuleAnalyzer::isDigit,letter1,true,true);
        letter1.addTransitionFunction(RuleAnalyzer::isOtherCharacter,otherCharacter1,false,false);

        otherCharacter1.addSemanticAction(new SemanticAction<Character>() {
            @Override
            public void onAction(List<Character> sequence, State<Character> state) {
                Stream<Character> charStream = sequence.stream();
                String symbol = charStream.map(String::valueOf).collect(Collectors.joining());
                variables.putSymbol(symbol.trim());
                setToken(StateCodes.VARIABLE.ordinal());
            }
        });
        //Constants
        State<Character> letter2 = new State<>("letter-constant");
        State<Character> otherCharacter2 = new State<>("o.c.2");
        otherCharacter2.addSemanticAction(new SemanticAction<Character>() {
            @Override
            public void onAction(List<Character>sequence, State<Character> state) {
                Stream<Character> charStream = sequence.stream();
                String symbol = charStream.map(String::valueOf).collect(Collectors.joining());
                constants.putSymbol(symbol.trim());
                setToken(StateCodes.CONSTANT.ordinal());
            }
        });
        otherCharacter2.setFinal();
        base.addTransitionFunction(RuleAnalyzer::isLetter,letter2,true,true);
        letter2.addTransitionFunction(RuleAnalyzer::isLetter,letter2,true,true);
        letter2.addTransitionFunction(RuleAnalyzer::isDigit,letter2,true,true);
        letter2.addTransitionFunction(RuleAnalyzer::isOtherCharacter,otherCharacter2,false,false);



        //Consequence
        State<Character> consequence1 = new State<>("-");
        State<Character> consequence2 = new State<>(">");
        consequence2.addSemanticAction(new SemanticAction<Character>() {
            @Override
            public void onAction(List<Character> sequence,  State<Character> state) {
                setToken(StateCodes.CONSEQUENCE.ordinal());
            }
        });

        base.addTransition('-',consequence1);
        consequence1.addTransition('>',consequence2);
        consequence2.setFinal();


        //AND
        State<Character> and = new State<>("^");
        and.addSemanticAction(new SemanticAction<Character>() {
            @Override
            public void onAction(List<Character>sequence,  State<Character> state) {
                setToken(StateCodes.AND.ordinal());
            }
        });
        base.addTransition('^',and);
        and.setFinal();

        //NOT
        State<Character> not = new State<>("~");
        not.addSemanticAction(new SemanticAction<Character>() {
            @Override
            public void onAction(List<Character>sequence, State<Character> state) {
                setToken(StateCodes.NOT.ordinal());
            }
        });
        base.addTransition('~',not);
        not.setFinal();

        //End rule
        State<Character> end = new State<>(";");
        base.addTransition(';',end);
        end.setFinal();

        FDA.setRoot(base);
    }
    public static Character[] toCharacterArray(String text){
        return  text.chars().mapToObj(c -> (char) c).toArray(Character[]::new);
    }
    public static boolean isDigit(Character character){
        return(Character.isDigit(character));

    }
    public static boolean isLetter(Character character){
        return(Character.isLetter(character));
    }
    public static boolean isOtherCharacter(Character character){
        return !(Character.isLetter(character)||Character.isDigit(character));
    }
    public static boolean isWhiteSpace(Character character){
        return Character.isWhitespace(character);
    }

    private static int[] interpret(String ruleElement){
        return FDA.execute(toCharacterArray(ruleElement));//importante añadir espacio por si hayuna variable al final
    }
    public static void processRule(String rule){
        if(debug) Console.printlnInfo("RULE",rule);
        Console.printlnInfo("FDA","Interpreting rule");
        int[] status = interpret(rule);
        StateOperationCode op =  StateOperationCode.values()[status[0]];


        if(op != StateOperationCode.SUCCESS){
            Console.printlnInfo("STATUS",Console.ANSI_RED+"FAIL in position "+ status[1]+" with character '"+rule.charAt(status[1])+"'");
        }else{
            Console.printlnInfo("STATUS",Console.ANSI_GREEN+op);
        }
        Console.printlnInfo("VARIABLE",variables.toString());
        Console.printlnInfo("CONSTANTS",constants.toString());
        Console.printlnInfo("TOKENS",tokenizer.toString());
    }
    public static void setDebug(boolean debug){
        FDA.setDebug(debug);
        RuleAnalyzer.debug = debug;
    }
    private static void setToken(int value){
        tokenizer.addToken(value,null);
    }

}
