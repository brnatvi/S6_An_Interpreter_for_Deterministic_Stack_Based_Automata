# Overview
Lexical and grammatical analysator, which transforms stack automata descriptions in abstract syntax, to verify if the automata is deterministic and then to execute the automata on some word given by user.

Exemple of such description:

>input symbols : a, b, c
>
>stack symbols : A, B, C, Z
>
>states : 1, 2
>
>initial state : 1
>
>initial stack : Z
>
>transitions :
>
>(1,a,Z,1,Z;A)
>
>(1,b,Z,1,Z;B)
>
>(1,c,Z,2,Z)
>
>(1,a,A,1,A;A)
>
>(1,b,A,1,A;B)
>
>(1,c,A,2,A)
>
>(1,a,B,1,B;A)
>
>(1,b,B,1,B;B)
>
>(1,c,B,2,B)
>
>2,a,A,2,)
>
>(2,b,B,2,)
>
>(2, ,Z,2,)

This automata recognizes the set  _{ wcẇ | w ∈ {a, b} }_, which is a special case of the palindromes.

# Usage
## Compile
```
make
```
## Run
```
./grammaire [option] file [word]
```

**Options:**
```
--reprint   --v1  - reprint automaton described by transitions
--reprint   --v2  - reprint automaton described by program\
--interpret --v1  - check if word is accepted by automaton described by its transitions which is in file
--interpret --v2  - check if word is accepted by automaton described by a program which is in file
```

# Maintainers
- GANGNEUX Paul

- BRAGINA Natalia
