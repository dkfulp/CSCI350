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
    Sum is X + Y.

% Rule to subtract two numbers
subtract(X, Y, Difference) :-
    Difference is X - Y.

% Rule to multiply two numbers
multiply(X, Y, Product) :-
    Product is X * Y.

% Rule to divide two numbers
divide(X, Y, Quotient) :-
    Y \= 0,               % Ensure Y is not zero
    Quotient is X / Y.

% Queries:
% 1. What is the sum of 5 and 3?
% ?- add(5, 3, Sum).

% 2. What is the product of 4 and 6?
% ?- multiply(4, 6, Product).
/*
The is operator is used to perform arithmetic calculations.
It evaluates the right-hand expression and unifies it with the left-hand variable.
For the query ?- add(5, 3, Sum)., Prolog calculates Sum = 8.
*/


% Rule to check if X is greater than Y
greater_than(X, Y) :-
    X > Y.

% Rule to check if X is less than Y
less_than(X, Y) :-
    X < Y.

% Rule to check if two numbers are equal
equal_to(X, Y) :-
    X =:= Y.

% Queries:
% 1. Is 10 greater than 7?
% ?- greater_than(10, 7).

% 2. Are 5 and 5 equal?
% ?- equal_to(5, 5).
/*
Arithmetic comparison operators include >, <, >=, =<, and =:=.
=:= is used for equality checks between numeric expressions.
*/


% Base case: the sum of an empty list is 0
list_sum([], 0).

% Recursive case: sum of a non-empty list
list_sum([H|T], Sum) :-
    list_sum(T, SumT),       % Recursive call to sum the tail
    Sum is H + SumT.         % Add the head to the sum of the tail

% Queries:
% 1. What is the sum of [1, 2, 3, 4]?
% ?- list_sum([1, 2, 3, 4], Sum).
/*
The base case sets the sum of an empty list to 0.
The recursive case adds the head of the list to the sum of the tail.
For the query ?- list_sum([1, 2, 3, 4], Sum)., Prolog calculates Sum = 10.
*/


% Rule to calculate the average of a list
average(List, Avg) :-
    list_sum(List, Sum),       % Get the sum of the list
    length(List, Len),         % Get the length of the list
    Len > 0,                   % Ensure the list is not empty
    Avg is Sum / Len.          % Calculate the average

% Queries:
% 1. What is the average of [2, 4, 6, 8]?
% ?- average([2, 4, 6, 8], Avg).
/*
This rule uses both list_sum/2 and length/2 to calculate the average.
For the query ?- average([2, 4, 6, 8], Avg)., Prolog calculates Avg = 5.
*/


% Base case: the max of a single-element list is the element itself
max_in_list([X], X).

% Recursive case: compare head with max of the tail
max_in_list([H|T], Max) :-
    max_in_list(T, MaxT),    % Find max of the tail
    Max is max(H, MaxT).     % Max is the greater of head or tail

% Queries:
% 1. What is the maximum of [2, 7, 5, 10, 3]?
% ?- max_in_list([2, 7, 5, 10, 3], Max).
/*
The base case considers a single-element list, where the max is the element itself.
The recursive case compares the head (H) with the maximum of the tail (MaxT) using the max/2 function.
For the query ?- max_in_list([2, 7, 5, 10, 3], Max)., Prolog finds Max = 10.
*/