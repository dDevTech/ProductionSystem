package LexicAnalyzer.AFD;


import java.util.List;
import java.util.Queue;

public class FinalState<T> {
    private State<T> node;
    private  List<T> sequence;
    private  Queue<T> remainingSequence;
    private int statusCode = -1;
    private boolean endedSequence = false;
    public FinalState(State<T> node, List<T> sequence, Queue<T>remainingSequence,int statusCode,boolean endedSequence) {
        this.node = node;
        this.sequence = sequence;
        this.remainingSequence = remainingSequence;
        this.statusCode = statusCode;
        this.endedSequence = endedSequence;
    }

    public State<T> getNode() {
        return node;
    }

    public void setNode(State<T> node) {
        this.node = node;
    }

    public  List<T> getSequence() {
        return sequence;
    }

    public void setSequence( List<T>  sequence) {
        this.sequence = sequence;
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
