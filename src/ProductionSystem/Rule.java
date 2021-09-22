/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package ProductionSystem;

public class Rule {
    public Rule(String rule){

    }
    public static void analyzeRule(String rule){
       rule =  rule.strip();
       for(int i = 0 ;i<rule.length();i++){
            if( rule.charAt(i) == ':'){//A$BC -> $B

            }
       }
    }
}
