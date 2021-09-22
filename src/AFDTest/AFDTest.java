package AFDTest;


import LexicAnalyzer.AFD.*;

import java.util.List;

public class AFDTest {
    public static void main(String[]args){
        AFD<Character> afd = new AFD<Character>() {

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

        stateDollar.addTransitionFunction(AFDTest::isDigit, stateLetterNumeric);
        stateDollar.addTransitionFunction(AFDTest::isLetter, stateLetterNumeric);

        stateLetterNumeric.addTransitionFunction(AFDTest::isDigit, stateLetterNumeric);
        stateLetterNumeric.addTransitionFunction(AFDTest::isLetter, stateLetterNumeric);

        stateLetterNumeric.addTransition('\\' , stateDelimiter);
        stateDelimiter.setFinal(2);


        afd.setRoot(root);
        String s= "$$a222dddd\\";
        System.out.println(s);
        afd.setDebug(false);
        System.out.println("Output: "+afd.execute(AFDTest.toCharacterArray(s)));

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
