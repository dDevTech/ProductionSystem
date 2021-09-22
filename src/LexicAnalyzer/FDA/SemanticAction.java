/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package LexicAnalyzer.FDA;

import java.util.List;
/*
Semantic actions are used when a transition is executed the method onAction is called to perform some tasks. You can add them to a final state to do something with the sequence recognized by the FDA
 */
public abstract class SemanticAction<T> {
    public abstract void onAction(List<T> sequence, State<T> state);
}
