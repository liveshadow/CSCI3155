# Lab 1

Include your writeup for the Lab 1 questions here. Use correct
Markdown markup. For more details, start with the article
https://help.github.com/articles/github-flavored-markdown

Olivia Abrant

Partner: Michelle Soult

#### 1. Scala Basics: Binding and Scope
##### (a)
  * pi at line 4 is bound at line 3 because pi is redefined within the scope with val pi = 3.14159
  * pi at line 7 is bound at line 1 because line 7 is not within the scope of area() or circumference(), so it is a free variable, where the most recent reference is its definition on line 1

##### (b)  
  * x at line 3 is bound at line 2 because x is referenced in the function declaration
  * x at line 6 is bound at line 5 because x is referenced in the case match and passed through
  * x at line 10 is bound at line 5 because x is outside the scope where val x is set to y + 1, so the previous reference is, again, the case match
  * x at line 13 is bound at line 1 because line 13 is outside the scope of function f, so the previous place where x is referenced is line 1, with the declaration val x = 3
			
#### 2. Scala Basics: Typing
  - Yes, the body of g is well-typed: the function returns a tuple of a tuple and an int.
    - g(x) : ((Int, Int), Int) because
      - (b, 1) : ((Int, Int), Int) because
        - 1 : Int
        - b : (Int, Int) because
          - x : Int
          - 3 : Int
      - (b, a+2) : ((Int, Int), Int) because
        - b : (Int, Int) because
          - x : Int
          - 3 : Int
        - a+2 : Int because
          - a : Int
          - 3 : Int