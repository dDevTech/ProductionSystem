/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package LexicAnalyzer.FDA;

import java.util.function.Function;

public class TransitionFunction<T>{
    private Function<T,Boolean>transition;
    //only change state or also remove from queue and add to current(if ignore = false;)
    private boolean read;
    private boolean write;

    public TransitionFunction(Function<T,Boolean>transition,boolean read,boolean write){
        this.read = read;
        this.write = write;

        this.transition = transition;
    }

    public Function<T, Boolean> getTransition() {
        return transition;
    }

    public void setTransition(Function<T, Boolean> transition) {
        this.transition = transition;
    }


    public boolean isWrite() {
        return write;
    }

    public void setWrite(boolean write) {
        this.write = write;
    }

    public boolean isRead() {
        return read;
    }

    public void setRead(boolean read) {
        this.read = read;
    }
}
