/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package LexicAnalyzer.FDA;


import Tools.Console;

import java.util.*;

/**
 * Creates a FDA where transition are of type T. FDA will process also sequences of type T
 * @param <T>
 */
public abstract class FDA<T> {
    private State<T> root;
    private boolean debug = false;
    private int initialSequenceSize;
    private int[] checkSequence( Queue<T> sequence){

        if(root == null){
            throw new RuntimeException("Root mustn't be null");
        }
        SequenceData<T> sequenceData = root.checkTransitions(sequence,debug, new ArrayList<>());
        Console.print(Console.ANSI_CYAN+ sequenceData.getSequence().toString()+"\n");
        onReadSequence(sequenceData.getSequence(), sequenceData.getNode(), sequenceData.getStatusCode());
        if(!sequenceData.isEndedSequence()&& sequenceData.getStatusCode()==StateOperationCode.SUCCESS.ordinal()){
            return checkSequence(sequenceData.getRemainingSequence());
        }
        Console.printlnInfo("FDA",Console.ANSI_PURPLE+"STATUS CODE: "+ sequenceData.getStatusCode());
        return new int[]{sequenceData.getStatusCode(), sequenceData.getSizeOfQueueWhenFinished()};
    }

    /**
     * Execute the FDA with the given sequence to subsequences
     * @param sequence
     * @return the status code of the process and the T where it fails in the case it fail
     */
    public int[] execute(T[]sequence){
        initialSequenceSize = sequence.length;
        Queue<T> queue = new LinkedList<>(Arrays.asList(sequence));
        int[]status = checkSequence(queue);
        status[1]=initialSequenceSize-status[1];
        return status;

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
