/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package LexicAnalyzer.FDA;

import java.util.*;
import java.util.function.Function;

/**
 * A state is a node of the automata where a possible sequence can traverse.
 * @param <T>
 */
public class State<T> {
    private Map<T, State<T>>transitions = new HashMap<>();
    private Map<TransitionFunction<T>, State<T>>transitionFunctions = new HashMap<>();
    private List<SemanticAction<T>> actions = new ArrayList<>();

    private String name = "Node";
    private boolean isFinal = false;

    /**
     *
     * @param name the name to see when debug is on
     */
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

    /**
     * Add a transition defined with a value from this state to the toTransit state
     * @param transitionValue
     * @param toTransit
     */
    public void addTransition(T transitionValue, State<T> toTransit){
        if(toTransit== null){
            throw new IllegalArgumentException("Transitioned node must be not null");
        }

        if(transitions.get(transitionValue)==toTransit){
            throw new IllegalArgumentException("Repeated transition value");
        }
        transitions.put(transitionValue,toTransit);
    }

    /**
     * Create a transition between this node and the next node determined by a custom transition function to apply to upcoming sequences
     * @param transitionFunction get an element of the sequence and return if it is valid to transit or not
     * @param toTransit destination state
     * @param ignore ignored transitions will overpass the state but won't affect niether the complete sequence nor the  transited sequence . Default = false
     *
     */
    public void addTransitionFunction(Function<T,Boolean>transitionFunction, State<T> toTransit,boolean ignore){
        if(toTransit== null){
            throw new IllegalArgumentException("Transitioned node must be not null");
        }
        if(transitionFunctions.get(transitionFunction)==toTransit){
            throw new IllegalArgumentException("Repeated transition function");
        }
        transitionFunctions.put(new TransitionFunction<T>(ignore,transitionFunction),toTransit);

    }
    protected FinalState checkTransitions(Queue<T> sequence, boolean debug,List<T>transitedSequence){
        if(transitedSequence.size()>0){
            callActions(transitedSequence);
        }
        if(debug){
            System.out.print(name+" -> ");
        }

        if(sequence.size()<=0){//ya no hay mas elementos posibles
            if(isFinal){//estado final -> es valido
                if(debug){
                    System.out.println("Valid sequence recognized");
                }
                return new FinalState<T>(this,transitedSequence,sequence,StateOperationCode.SUCCESS.ordinal(),true);
            }else{//error ya que no hay mas posibles elementos a leer
                if(debug){
                    System.err.println("Reached final of sequence. Not gone to final transition");
                }
            }
            return new FinalState<T>(this,transitedSequence,sequence,StateOperationCode.NOT_REACH_FINAL_STATE_END_OF_SEQUENCE.ordinal(), true);
        }else if(isFinal){//vamos al siguiente
            if(debug){
                System.out.print("Final state not finished sequence ");
            }
            return new FinalState<T>(this,transitedSequence,sequence,StateOperationCode.SUCCESS.ordinal(), false);
        }

        //Buscamos la posible transicion
        State<T> stateFound =null;
        for(Map.Entry<T, State<T>>entry:transitions.entrySet()){
            if(entry.getKey().equals(sequence.peek())){
                T element = sequence.poll();
                transitedSequence.add(element);
                stateFound = entry.getValue();
                break;
            }
        }
        if(stateFound ==null){
            for(Map.Entry<TransitionFunction<T>, State<T>>entry:transitionFunctions.entrySet()){

                if(entry.getKey().getTransition().apply(sequence.peek())){
                    if(!entry.getKey().isIgnore()){
                        T element = sequence.poll();
                        transitedSequence.add(element);
                    }
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

            return new FinalState<T>(this,transitedSequence,sequence,StateOperationCode.NOT_TRANSITION_AVAILABLE.ordinal(),true);
        }

        return stateFound.checkTransitions(sequence,debug,transitedSequence);

    }

    private void callActions(List<T>sequence){
        actions.stream().iterator().forEachRemaining((SemanticAction action)->action.onAction(sequence,this));
    }

    /**
     * Set a state final. When a final state is reached the subsequence is accepted by FDA
     */
    public void setFinal() {
        this.isFinal=true;

    }

    /**
     * Add a semantic action to this current state
     * @param action
     * @see SemanticAction
     */
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
