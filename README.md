# While Abstract Interpreter
## Contents
- [Description](#description)
- [How to use it](#how-to-use-it)
- [While language](#while+-language)

## Description
The following project is an abstract interpreter for the abstract denotational semantics of the **While** language. 
The program takes as input any statement, a representation of an abstract state and a domain, and produces an output state according to the evalutation rules of While. 
In this case, I use the following parametrized intervals domain:
$$Int_{m,n} =  \\{ \emptyset , \mathbb{Z} \\} \cup \\{ [k, k] \vert k \in \mathbb{Z} \\} \cup
\\{ [a, b] | a < b, [a, b] \subseteq [m, n] \\} \\ \cup \\{ (−\infty, k] | k \in [m, n]\\} \cup \\{ [k,+\infty ) | k \in [m, n]\\}$$

## How to use it
1. Download the repo
2. Run `.\build.ps1`
3. Run the executable `.\WhileAbstractInterpreter.exe [file_name.txt] [lower bound] [upper bound] `

## While+ Language
The While syntax is the following:
```
S ::= x := a | skip | S1; S2
  | if b then S1 else S2
  | while b do S
```
where `a` stands for arithmentic expression and `b` for boolean expression:
```
a ::= n | x | a1 + a2
  | a1 ∗ a2 | a1 − a2
  | a1 / a2 | -a

b ::= true | false 
  | b1 ∧ b2 | ¬ b | b1 ∧ b2
  | a1 = a2 | a1 <= a2
  | a1 < a2 | a1 > a2
  | a1 >= a2 | a1 != a2 
```
What makes While be While+ are the following expression:
```
S ::= ... | x++ | x-- 
```






