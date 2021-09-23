# Production AI system - Beta only
### How to use the AFD
How to use an AFD
Create a new AFD in this case is of type Character so the transitions will be of characters (used in compilers for example)

```java
AFD<Character>afd;
afd = new AFD<>() {
            @Override
            public void onReadSequence(List<Character> completeSequence, State<Character> finalNode, int statusCode) {
                System.out.println(completeSequence+"  Status code: "+statusCode);
            }

        };
```
Must override `onReadSequence()`. This method will be called when the AFD detect a new sequence (The AFD finish in an end state). It will give the finalNode the list of characters that were detected and a status code to know how it finished.

Now we need to create the states and transitions of the AFD.
**Note: the algorithm doesn't check infinite loops (you should create a non cyclic structure to be an AFD)**


To create an State and add a transition to another node (in this case from base to dollar)
```java
 State<Character> base = new State<>("root");
 State<Character> dollar = new State<>("dollar");
 base.addTransition('$',dollar);
```
Transition functions are supported. Imagine you want to make a transition if characters are digits:
Create the function (in this case is create in the main class RuleAnalyzer)
 ```java
    public static boolean isDigit(Character character){
        return(Character.isDigit(character));
    }
```
And add the transition to the state determined by the function, the out state and read and write (boolean). Write must be used in final states
```java
 base.addTransitionFunction(RuleAnalyzer::isDigit, dollar,true,true);
```
If you want to make an state final just call the method `setFinal`.
```java
dollar.setFinal();
```

Semantic actions are supported:
```java
dollar.addSemanticAction(new SemanticAction<Character>() {
            @Override
            public void onAction(List<Character> sequence, State<Character> state) {
                Stream<Character> charStream = sequence.stream();
                String symbol = charStream.map(String::valueOf).collect(Collectors.joining());
                variables.putSymbol(symbol.trim());
                setToken(StateCodes.VARIABLE.ordinal());
            }
        });
```
When the node dollar is reached  `onAction` will be called with the current sequence detected and the state.

Its important to set the initial state of the AFD.
```java
 afd.setRoot(base);
```

Now the AFD is done. To execute it just call `execute` method of the AFD:
```java
private static int interpret(String ruleElement){
        return afd.execute(toCharacterArray(ruleElement));
    }
```
Where `toCharacterArray` will convert the String in a Character[]sequence which is neccessary for the AFD
```java
 public static Character[] toCharacterArray(String text){
        return  text.chars().mapToObj(c -> (char) c).toArray(Character[]::new);
    }
```
If you turn on the DEBUG you will be able to test and debug the algorithm
![image](https://user-images.githubusercontent.com/18512841/134489000-e890d8d5-4fd8-40bb-9eab-39764ba709d2.png)

