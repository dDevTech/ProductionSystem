package LexicAnalyzer.AFD;

import java.util.*;
import java.util.function.Function;

public class State<T> {
    private Map<T, State<T>>transitions = new HashMap<>();
    private Map<Function<T,Boolean>, State<T>>transitionFunctions = new HashMap<>();
    private List<SemanticAction<T>> actions = new ArrayList<>();
    private int codeWhenReached=-1;
    private String name = "Node";
    private boolean isFinal = false;

    public State(String name){
        this.name = name;
    }
    public State(){}

    @Override
    public String toString() {
        return "Node{" +
                "transitions=" + transitions +
                ", transitionFunctions=" + transitionFunctions +
                ", name='" + name + '\'' +
                ", isFinal=" + isFinal +
                '}';
    }


    public void addTransition(T transitionValue, State<T> toTransit){
        if(toTransit== null){
            throw new IllegalArgumentException("Transitioned node must be not null");
        }

        if(transitions.get(transitionValue)==toTransit){
            throw new IllegalArgumentException("Repeated transition value");
        }
        transitions.put(transitionValue,toTransit);
    }
    public void addTransitionFunction(Function<T,Boolean>transitionFunction, State<T> toTransit){
        if(toTransit== null){
            throw new IllegalArgumentException("Transitioned node must be not null");
        }
        if(transitionFunctions.get(transitionFunction)==toTransit){
            throw new IllegalArgumentException("Repeated transition function");
        }
        transitionFunctions.put(transitionFunction,toTransit);

    }
    protected FinalState checkTransitions(Queue<T> sequence, boolean debug,List<T>currentSequence){
        if(currentSequence.size()>0){
            callActions(currentSequence);
        }
        if(debug){
            System.out.print(name+" -> ");
        }

        if(sequence.size()<=0){//ya no hay mas elementos posibles
            if(isFinal){//estado final -> es valido
                if(debug){
                    System.out.println("Valid sequence recognized");
                }
            }else{//error ya que no hay mas posibles elementos a leer
                if(debug){
                    System.err.println("Reached final of sequence. Not gone to final transition");
                }
            }
            return new FinalState<T>(this,currentSequence,sequence,codeWhenReached,true);
        }else if(isFinal){//vamos al siguiente
            if(debug){
                System.out.print("Final state not finished sequence ");
            }
            return new FinalState<T>(this,currentSequence,sequence,codeWhenReached,false);
        }

        //Buscamos la posible transicion
        State<T> stateFound =null;
        for(Map.Entry<T, State<T>>entry:transitions.entrySet()){
            if(entry.getKey().equals(sequence.peek())){
                T element = sequence.poll();
                currentSequence.add(element);
                stateFound = entry.getValue();
                break;
            }
        }
        if(stateFound ==null){
            for(Map.Entry<Function<T,Boolean>, State<T>>entry:transitionFunctions.entrySet()){

                if(entry.getKey().apply(sequence.peek())){
                    T element = sequence.poll();
                    currentSequence.add(element);
                    stateFound = entry.getValue();
                    break;
                }
            }
        }
        //Si no hay posible transicion error
        if(stateFound ==null){
            if(debug){
                System.err.println("Not available transition. Not recognized by LexicAnalyzer.AFD");
            }

            return new FinalState<T>(this,currentSequence,sequence,-1,true);
        }

        return stateFound.checkTransitions(sequence,debug,currentSequence);

    }

    private void callActions(List<T>sequence){
        actions.stream().iterator().forEachRemaining((SemanticAction action)->action.onAction(sequence,this));
    }

    public void setFinal(int codeWhenReached) {
        this.isFinal=true;
        this.codeWhenReached = codeWhenReached;
    }
    public void addSemanticAction(SemanticAction action){
        actions.add(action);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }


}
