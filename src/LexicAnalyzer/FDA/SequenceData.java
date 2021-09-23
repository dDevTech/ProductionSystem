/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package LexicAnalyzer.FDA;


import java.util.List;
import java.util.Queue;

public class SequenceData<T> {
    private State<T> node;
    private  List<T> transitedSequence;
    private  Queue<T> remainingSequence;
    private int statusCode = -1;

    private boolean endedSequence = false;
    private int sizeOfQueueWhenFinished;

    public SequenceData(State<T> node, List<T> transitedSequence, Queue<T>remainingSequence, int statusCode, boolean endedSequence, int sizeOfQueueWhenFinished) {
        this.node = node;
        this.transitedSequence = transitedSequence;
        this.remainingSequence = remainingSequence;
        this.statusCode = statusCode;

        this.endedSequence = endedSequence;
        this.sizeOfQueueWhenFinished = sizeOfQueueWhenFinished;
    }

    public List<T> getTransitedSequence() {
        return transitedSequence;
    }

    public void setTransitedSequence(List<T> transitedSequence) {
        this.transitedSequence = transitedSequence;
    }

    public int getSizeOfQueueWhenFinished() {
        return sizeOfQueueWhenFinished;
    }

    public void setSizeOfQueueWhenFinished(int sizeOfQueueWhenFinished) {
        this.sizeOfQueueWhenFinished = sizeOfQueueWhenFinished;
    }

    public State<T> getNode() {
        return node;
    }

    public void setNode(State<T> node) {
        this.node = node;
    }

    public  List<T> getSequence() {
        return transitedSequence;
    }

    public void setSequence( List<T>  sequence) {
        this.transitedSequence = sequence;
    }


    public int getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public Queue<T> getRemainingSequence() {
        return remainingSequence;
    }

    public void setRemainingSequence(Queue<T> remainingSequence) {
        this.remainingSequence = remainingSequence;
    }

    public boolean isEndedSequence() {
        return endedSequence;
    }

    public void setEndedSequence(boolean endedSequence) {
        this.endedSequence = endedSequence;
    }
}
