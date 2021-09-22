package LexicAnalyzer.AFD;

import java.util.List;

public abstract class SemanticAction<T> {
    public abstract void onAction(List<T> sequence, State<T> state);
}
