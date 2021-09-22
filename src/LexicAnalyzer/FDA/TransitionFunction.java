/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package LexicAnalyzer.FDA;

import java.util.function.Function;

public class TransitionFunction<T>{
    private Function<T,Boolean>transition;
    private boolean ignore = false;//only change state or also remove from queue and add to current(if ignore = false;)
    public TransitionFunction(boolean ignore,Function<T,Boolean>transition){
        this.ignore = ignore;
        this.transition = transition;
    }

    public Function<T, Boolean> getTransition() {
        return transition;
    }

    public void setTransition(Function<T, Boolean> transition) {
        this.transition = transition;
    }

    public boolean isIgnore() {
        return ignore;
    }

    public void setIgnore(boolean ignore) {
        this.ignore = ignore;
    }
}
