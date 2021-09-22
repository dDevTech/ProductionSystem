/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package AFDTest;

import LexicAnalyzer.FDA.*;

import java.util.List;

public class AFDTest2 {
    public static void main(String[]args){
        FDA<Character> FDA = new FDA<Character>() {

            @Override
            public void onReadSequence(List<Character> completeSequence, State<Character> finalNode, int statusCode) {

            }
        };
        State<Character> root = new State<Character>();
        root.setName("root");


        State<Character> stateLetterNumeric = new State<Character>();
        stateLetterNumeric.setName("letter/numeric");

        State<Character> stateMult = new State<Character>();
        stateMult.setName("*");

        State<Character> stateAssign1 = new State<Character>();
        stateAssign1.setName(":");

        State<Character> stateAssign2 = new State<Character>();
        stateAssign2.setName("=");

        State<Character> stateDelimiter = new State<Character>();
        stateDelimiter.setName("\\");

        root.addTransition('*', stateMult);
        root.addTransition(':', stateAssign1);
        stateAssign1.addTransition('=', stateAssign2);

        root.addTransitionFunction(AFDTest::isLetter, stateLetterNumeric,false);


        stateLetterNumeric.addTransitionFunction(AFDTest::isDigit, stateLetterNumeric,false);
        stateLetterNumeric.addTransitionFunction(AFDTest::isLetter, stateLetterNumeric,false);

        stateLetterNumeric.addTransition('\\' , stateDelimiter);
        stateDelimiter.setFinal();
        stateMult.setFinal();
        stateAssign2.setFinal();


        FDA.setRoot(root);
        FDA.setDebug(true);
        String s= ":=";
        System.out.println(s);

        System.out.println("Output: "+ FDA.execute(AFDTest.toCharacterArray(s)));

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
}
