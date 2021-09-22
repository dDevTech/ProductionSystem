package ProductionSystem;

public class Main {
    public static void main(String[]args){
        RuleAnalyzer.setupAnalyzer();
        RuleAnalyzer.setDebug(true);
        RuleAnalyzer.processRule("$aaabv^$bb^~aab->$aaabv");

    }
}
