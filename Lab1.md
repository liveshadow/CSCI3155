# Lab 1

Include your writeup for the Lab 1 questions here. Use correct
Markdown markup. For more details, start with the article
https://help.github.com/articles/github-flavored-markdown

Olivia Abrant

Partner: Michelle Soult

1. Scala Basics: Binding and Scope
  - pi at line 4 is bound at line 3; pi at line 7 is bound at line 1
  - x at line 3 is bound at line 2; x at line 6 is bound at line 5;
    x at line 10 is bound at line 5; x at line 13 is bound at line 1
			
2. Scala Basics: Typing
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