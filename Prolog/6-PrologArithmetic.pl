/******************************************************************************
Arithmetic Example - 
Example program that creates a basic Prolog system that focuses on arithmetic 
operations in Prolog. Prolog handles arithmetic differently from other programming 
languages, relying on predicates to perform calculations and comparisons.

Prolog uses the is operator for arithmetic expressions.
Arithmetic comparisons use special operators like =:=, >, <, etc.
Recursion is often combined with arithmetic to process lists and collections of numbers.

*******************************************************************************/
% Rule to add two numbers
add(X, Y, Sum) :-
    Sum is X + Y.              % Calculate Sum as X + Y

% Rule to subtract two numbers
subtract(X, Y, Difference) :-
    Difference is X - Y.       % Calculate Difference as X - Y

% Rule to multiply two numbers
multiply(X, Y, Product) :-
    Product is X * Y.          % Calculate Product as X * Y

% Rule to divide two numbers
divide(X, Y, Quotient) :-
    Y \= 0,                    % Ensure Y is not zero to avoid division by zero
    Quotient is X / Y.         % Calculate Quotient as X / Y

% Queries:
% 1. What is the sum of 5 and 3?
% ?- add(5, 3, Sum).

% Step-by-step explanation for the first query:
% 1. Initial call: add(5, 3, Sum).
%    - X = 5, Y = 3.
%    - Matches the rule: Sum is X + Y.
%    - Calculates Sum as 5 + 3 = 8.
%    - Returns Sum = 8.
%    - Final result: Sum = 8.

% 2. What is the product of 4 and 6?
% ?- multiply(4, 6, Product).

% Step-by-step explanation for the second query:
% 1. Initial call: multiply(4, 6, Product).
%    - X = 4, Y = 6.
%    - Matches the rule: Product is X * Y.
%    - Calculates Product as 4 * 6 = 24.
%    - Returns Product = 24.
%    - Final result: Product = 24.
/*
The is operator is used to perform arithmetic calculations.
It evaluates the right-hand expression and unifies it with the left-hand variable.
For the query ?- add(5, 3, Sum)., Prolog calculates Sum = 8.
*/


/******************************************************************************/
% Rule to check if X is greater than Y
greater_than(X, Y) :-
    X > Y.                 % Succeeds if X is greater than Y

% Rule to check if X is less than Y
less_than(X, Y) :-
    X < Y.                 % Succeeds if X is less than Y

% Rule to check if two numbers are equal
equal_to(X, Y) :-
    X =:= Y.               % Succeeds if X is equal to Y (using arithmetic equality)

% Queries:
% 1. Is 10 greater than 7?
% ?- greater_than(10, 7).

% Step-by-step explanation for the first query:
% 1. Initial call: greater_than(10, 7).
%    - X = 10, Y = 7.
%    - Matches the rule: X > Y.
%    - Evaluates 10 > 7, which is true.
%    - Succeeds and returns true.
%    - Final result: true.

% 2. Are 5 and 5 equal?
% ?- equal_to(5, 5).

% Step-by-step explanation for the second query:
% 1. Initial call: equal_to(5, 5).
%    - X = 5, Y = 5.
%    - Matches the rule: X =:= Y.
%    - Evaluates 5 =:= 5, which is true.
%    - Succeeds and returns true.
%    - Final result: true.
/*
Arithmetic comparison operators include >, <, >=, =<, and =:=.
=:= is used for equality checks between numeric expressions.
*/


/******************************************************************************/
% Base case: the sum of an empty list is 0
list_sum([], 0).

% Recursive case: sum of a non-empty list
list_sum([H|T], Sum) :-
    list_sum(T, SumT),       % Recursive call to sum the tail
    Sum is H + SumT.         % Add the head to the sum of the tail

% Queries:
% 1. What is the sum of [1, 2, 3, 4]?
% ?- list_sum([1, 2, 3, 4], Sum).

% Step-by-step explanation for the query:

% 1. Initial call: list_sum([1, 2, 3, 4], Sum).
%    - The list is non-empty, so it matches the recursive case.
%    - H = 1 (head of the list), T = [2, 3, 4] (tail of the list).
%    - Recursive call: list_sum([2, 3, 4], SumT).

% 2. Second call: list_sum([2, 3, 4], SumT).
%    - The list is still non-empty, so it matches the recursive case.
%    - H = 2 (head), T = [3, 4] (tail).
%    - Recursive call: list_sum([3, 4], SumT).

% 3. Third call: list_sum([3, 4], SumT).
%    - The list is still non-empty, so it matches the recursive case.
%    - H = 3 (head), T = [4] (tail).
%    - Recursive call: list_sum([4], SumT).

% 4. Fourth call: list_sum([4], SumT).
%    - The list is still non-empty, so it matches the recursive case.
%    - H = 4 (head), T = [] (tail).
%    - Recursive call: list_sum([], SumT).

% 5. Base case: list_sum([], SumT).
%    - The list is empty, so it matches the base case.
%    - SumT = 0.
%    - Returns SumT = 0 to the previous call.

% 6. Back to fourth call: Sum is 4 + 0 = 4.
%    - Returns Sum = 4 to the previous call.

% 7. Back to third call: Sum is 3 + 4 = 7.
%    - Returns Sum = 7 to the previous call.

% 8. Back to second call: Sum is 2 + 7 = 9.
%    - Returns Sum = 9 to the previous call.

% 9. Back to initial call: Sum is 1 + 9 = 10.
%    - Final result: Sum = 10.
/*
The base case sets the sum of an empty list to 0.
The recursive case adds the head of the list to the sum of the tail.
For the query ?- list_sum([1, 2, 3, 4], Sum)., Prolog calculates Sum = 10.
*/


/******************************************************************************/
% Rule to calculate the average of a list
average(List, Avg) :-
    list_sum(List, Sum),       % Get the sum of the list
    length(List, Len),         % Get the length of the list
    Len > 0,                   % Ensure the list is not empty
    Avg is Sum / Len.          % Calculate the average

% Queries:
% 1. What is the average of [2, 4, 6, 8]?
% ?- average([2, 4, 6, 8], Avg).

% Step-by-step explanation for the query:

% 1. Initial call: average([2, 4, 6, 8], Avg).
%    - Matches the average rule and proceeds to calculate the sum of the list.

% 2. Call to list_sum([2, 4, 6, 8], Sum).
%    - Uses the list_sum/2 rule defined earlier.
%    - Recursive calculation of the sum:
%      a. 2 + (4 + (6 + (8 + 0))) = 20.
%    - Returns Sum = 20.

% 3. Call to length([2, 4, 6, 8], Len).
%    - Uses the built-in length/2 predicate to find the length of the list.
%    - The list has 4 elements, so Len = 4.
%    - Returns Len = 4.

% 4. Check if Len > 0.
%    - Len = 4, so the condition is true.
%    - Proceeds to the next step.

% 5. Calculate Avg as Sum / Len.
%    - Avg is 20 / 4 = 5.
%    - Returns Avg = 5.

% Final result: Avg = 5.
/*
This rule uses both list_sum/2 and length/2 to calculate the average.
For the query ?- average([2, 4, 6, 8], Avg)., Prolog calculates Avg = 5.
*/


/******************************************************************************/
% Base case: the max of a single-element list is the element itself
max_in_list([X], X).

% Recursive case: compare head with max of the tail
max_in_list([H|T], Max) :-
    max_in_list(T, MaxT),    % Find max of the tail
    Max is max(H, MaxT).     % Max is the greater of head or tail

% Queries:
% 1. What is the maximum of [2, 7, 5, 10, 3]?
% ?- max_in_list([2, 7, 5, 10, 3], Max).

% Step-by-step explanation for the query:

% 1. Initial call: max_in_list([2, 7, 5, 10, 3], Max).
%    - The list is non-empty and has more than one element, so it matches the recursive case.
%    - H = 2 (head), T = [7, 5, 10, 3] (tail).
%    - Recursive call: max_in_list([7, 5, 10, 3], MaxT).

% 2. Second call: max_in_list([7, 5, 10, 3], MaxT).
%    - H = 7 (head), T = [5, 10, 3] (tail).
%    - Recursive call: max_in_list([5, 10, 3], MaxT).

% 3. Third call: max_in_list([5, 10, 3], MaxT).
%    - H = 5 (head), T = [10, 3] (tail).
%    - Recursive call: max_in_list([10, 3], MaxT).

% 4. Fourth call: max_in_list([10, 3], MaxT).
%    - H = 10 (head), T = [3] (tail).
%    - Recursive call: max_in_list([3], MaxT).

% 5. Fifth call: max_in_list([3], MaxT).
%    - The list has only one element, so it matches the base case.
%    - MaxT = 3.
%    - Returns MaxT = 3 to the previous call.

% 6. Back to fourth call: Max is max(10, 3) = 10.
%    - Returns Max = 10 to the previous call.

% 7. Back to third call: Max is max(5, 10) = 10.
%    - Returns Max = 10 to the previous call.

% 8. Back to second call: Max is max(7, 10) = 10.
%    - Returns Max = 10 to the previous call.

% 9. Back to initial call: Max is max(2, 10) = 10.
%    - Final result: Max = 10.
/*
The base case considers a single-element list, where the max is the element itself.
The recursive case compares the head (H) with the maximum of the tail (MaxT) using the max/2 function.
For the query ?- max_in_list([2, 7, 5, 10, 3], Max)., Prolog finds Max = 10.
*/