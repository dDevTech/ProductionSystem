/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package LexicAnalyzer.Tokenizer;

import java.util.*;

public class Tokenizer<T>{
    private List<Map.Entry<Integer,T>> tokens;
    public Tokenizer(){
        tokens = new ArrayList<>();
    }
    public void addToken(Integer id,T value){
        Map.Entry entry = new AbstractMap.SimpleEntry<Integer, T>(id,value);
        tokens.add(entry);
    }

    @Override
    public String toString() {
        return "Tokenizer{" +
                "tokens=" + tokens +
                '}';
    }
}
