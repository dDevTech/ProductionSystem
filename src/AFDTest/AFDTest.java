/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package AFDTest;


import LexicAnalyzer.FDA.*;

import java.util.List;

public class AFDTest {
    public static void main(String[]args){
        FDA<Character> FDA = new FDA<Character>() {

            @Override
            public void onReadSequence(List<Character> completeSequence, State<Character> finalNode, int statusCode) {

            }
        };
        State<Character> root = new State<Character>();
        root.setName("root");
        State<Character> stateDollar = new State<Character>();
        stateDollar.setName("dollar");

        State<Character> stateLetterNumeric = new State<Character>();
        stateLetterNumeric.setName("Letter/numeric");

        State<Character> stateDelimiter = new State<Character>();
        stateDelimiter.setName("Delimiter");

        root.addTransition('$', stateDollar);

        stateDollar.addTransitionFunction(AFDTest::isDigit, stateLetterNumeric,true,true);
        stateDollar.addTransitionFunction(AFDTest::isLetter, stateLetterNumeric,true,true);

        stateLetterNumeric.addTransitionFunction(AFDTest::isDigit, stateLetterNumeric,true,true);
        stateLetterNumeric.addTransitionFunction(AFDTest::isLetter, stateLetterNumeric,true,true);

        stateLetterNumeric.addTransition('\\' , stateDelimiter);
        stateDelimiter.setFinal();


        FDA.setRoot(root);
        String s= "$$a222dddd\\";
        System.out.println(s);
        FDA.setDebug(false);
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
