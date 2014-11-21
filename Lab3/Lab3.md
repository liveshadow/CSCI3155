#Lab 3

Olivia Abrant

Partner: Catherine Dewerd

#### 1. 

const x = 1
const plusN = function (n) { return
function f(x) { return x + n };
};
plusN(5)

The above test case behaves differently under dynamic versus static scoping - assuming that the above code is part of a greater program, under static scoping, the function call of plusN(5) will pull the x = 1 from the previous scope, whereas with dynamic scoping, the x that is used may not be that particular x; it could be any x that used previously in any scope in the program.

#### 2.

Yes, the evaluation order is deterministic as specified by the judgement form e -> e'. The function will always evaluate in the same order and result in the same output, assuming the same input is entered.

#### 3.

The evaluation order for e1 + e2 is to first evaluate e1 completely, then evaluate e2. This is determined in the judgement forms - when we are dealing with an e2, there is no e1, just a v1. This means that when we do go on to evaluate e2, e1 will have already been fully evaluated, i.e. it will be a value.

To get the opposite evaluation order in the judgement form you would have e1 instead of v1 and v2 instead of e2, signifying that e2 should be completely evaluated first. We would also have to switch the order on the and/or functions, because the logic would be incorrect and they would short-circuit. In the code we would take every case where we step on e1 and instead step on e2, and every case where we are calling step on e2 and instead step on e1. We would also have to switch the matching on e1 to matching on e2, and require that v2 is a value instead of v1.

#### 4.
##### (a) Concept:

A short circuit evaluation example:
```sh
1 int denominator
2 int num
3 if (denominator != 0 && num / denominator){
4 println("hello world")
5 }

```
The short-circuit occurs in line 3 where the "and" is checked. If the denominator is zero (something that would in this case throw an error, if the second condition was evaluated) the first case in the and will be false. Because it is an "and" check there is no way the rest of the check can evaluate to true, so the "and" check may as well return false without spending the time and space evaluating the second condition. This is useful because if you have something that will throw an error if it is evaluated (like dividing by 0), you can avoid executing it in the one specific case that will throw the error.


(b) JavaScripty:

Yes, e1 && e2 will short-circuit in lab 3. The specific forms we are interested in are the DoAndTrue and the DoAndFalse. The DoAndTrue will evaluate e2, but the DoAndFalse will not, because since the first expression is false it will just return the current value of false. This is implemented in the code as recursing on e2 if e1 is true, else just returning false.