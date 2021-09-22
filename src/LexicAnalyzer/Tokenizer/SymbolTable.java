package LexicAnalyzer.Tokenizer;

import java.util.HashMap;

public class SymbolTable extends HashMap<String,Integer> {
    private int counter = 0;
    public void putSymbol(String nameSymbol){
        if (!containsKey(nameSymbol)) {
            put(nameSymbol,counter++);
        }

    }

}
