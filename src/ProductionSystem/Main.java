/*
 * Copyright (c) 2021. Developed by dDev Tech. Website: https://www.retopall.com/
 */

package ProductionSystem;

public class Main {
    public static void main(String[]args){
        RuleAnalyzer.setupAnalyzer();
        RuleAnalyzer.setDebug(true);
        RuleAnalyzer.processRule("$aaabv^$bb^~aab-->$aaab;;");

    }
}
