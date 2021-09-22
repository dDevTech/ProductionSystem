package AFDTest;

import LexicAnalyzer.AFD.*;

import java.util.List;

public class AFDTest2 {
    public static void main(String[]args){
        AFD<Character> afd = new AFD<Character>() {

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

        root.addTransitionFunction(AFDTest::isLetter, stateLetterNumeric);


        stateLetterNumeric.addTransitionFunction(AFDTest::isDigit, stateLetterNumeric);
        stateLetterNumeric.addTransitionFunction(AFDTest::isLetter, stateLetterNumeric);

        stateLetterNumeric.addTransition('\\' , stateDelimiter);
        stateDelimiter.setFinal(0);
        stateMult.setFinal(1);
        stateAssign2.setFinal(2);


        afd.setRoot(root);
        afd.setDebug(true);
        String s= ":=";
        System.out.println(s);

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
