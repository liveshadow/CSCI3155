#Lab 2

Olivia Abrant

Partner: Michelle Soult

#### 1. Grammars: Synthetic Examples
##### (a)
      ––––––––––––    ––––––––––––    }  Axioms
      a ∈ VObjects    b ∈ VObjects    }

      s ∈ VObjects    s1 ∈ AObjects  s2 ∈ AObjects
      ––––––––––––    ––––––––––––––––––––––––––––
      s ∈ AObjects         s1 & s2 ∈ AObjects 

##### (b)
      
          A                A
         / \              / \
        A & A            A & A
       / \   \          /   / \
      A & A   V        V   A & A
       ...                  ...

      More than one parse tree can be created from the same syntax => ambiguous

##### (c)
       L(G) = { a^n+1 or b^n or c^n+1 | n >= 0 }

       A and C will n + 1 times, as the terminal will always be a or c, respectively, while B will recurse only n times, as the terminal is an empty string.

##### (d)
    1 and 4 are sentences in the given grammar.

    1.
        S ::=> AaBb
            => baBb  (Rule 2)
            => baab  (Rule 3)
    4.
        S ::=> AaBb  
            => AbaBb
            => bbaBb
            => bbaab

##### (e)
    1 and 5 are sentences in the given grammar.

    1.               5.
         S                S
       /| |\            /| |\
      a S c B          a S c B
        |   |            |   |
        b   d            A   A
                         |   |
                         c   c


####2. Grammars: Understanding a Language
##### (a)
      
i. Expressions generated with these grammars will always have either an operand by itself or a repeating sequence such as "operand operator operand operator operand operator operand". There will never be an empty string or an expression that begins or ends with an operator.

ii. These grammars generate the same expression, but they go about it in opposite ways - the first grammar starts building the expression from the end, with "e operator operand" and tacks new things onto the beginning. The second starts with an operand at the beginning and tacks new things onto the end with esuffix.

##### (b)

The "-" operator has precedence over the "<<" operator. I tested multiple orderings of the operators such as "2-3<<2" and "2<<2-3" and in both cases the "-" took precedence. Then I tried putting parentheses around "-", such as with "2<<4-3" and "2<<(4-3)" and each gave the same result, indicating that the "-" was executing first in both cases.

Examples:

  * 2-3<<2 = -4
  * 2-(3<<2) = -10
  * 2<<4-3 = 4
  * 2<<(4-3) = 4

##### (c)
      S ::= a.b E
      a ::= 0 | i | -i
      b ::= 0 | i | 0b | ib
      e ::= Ei | Eib | ∈
      i ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

      Where 0-9, ., -, and E are in the set ∑
      and ∈ is the empty string
















