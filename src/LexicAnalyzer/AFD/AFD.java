package LexicAnalyzer.AFD;


import java.util.*;

public abstract class AFD<T> {
    private State<T> root;
    private boolean debug = false;
    private int checkSequence( Queue<T> sequence){

        if(root == null){
            throw new RuntimeException("Root mustn't be null");
        }
        FinalState<T>finalState = root.checkTransitions(sequence,debug, new ArrayList<>());
        onReadSequence(finalState.getSequence(), finalState.getNode(),finalState.getStatusCode());
        if(!finalState.isEndedSequence()){

            checkSequence(finalState.getRemainingSequence());
        }

        return finalState.getStatusCode();
    }
    public int execute(T[]sequence){
        Queue<T> queue = new LinkedList<>(Arrays.asList(sequence));
        int statusCode = checkSequence(queue);
        return statusCode;

    }
    public abstract void onReadSequence(List<T> completeSequence, State<T> finalNode, int statusCode);


    public void setRoot(State<T> state){
        this.root = state;
    }

    public State<T> getRoot() {
        return root;
    }

    public boolean isDebug() {
        return debug;
    }

    public void setDebug(boolean debug) {
        this.debug = debug;
    }
}
