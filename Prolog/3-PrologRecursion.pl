/******************************************************************************
Unification and Pattern Matching Example - 
Example program that creates a basic Prolog system that focus on recursion in 
Prolog, which is a fundamental concept that allows Prolog to handle iterative 
logic, list processing, and more complex relationships. We’ll cover basic 
recursion with some examples to help understand how Prolog uses recursive 
calls.

Base cases are essential to stop recursion.
Recursive cases define how Prolog should break down the problem step by step.
Prolog uses recursion extensively for list processing, arithmetic, and logic.

*******************************************************************************/
% Base case: the factorial of 0 is 1
factorial(0, 1).

% Recursive case: N! = N * (N-1)!
factorial(N, F) :-
    N > 0,               % Ensure N is greater than 0
    N1 is N - 1,         % Calculate N-1
    factorial(N1, F1),   % Recursive call to calculate factorial of N-1
    F is N * F1.         % Multiply N by the result of the recursive call

% Queries:
% 1. What is the factorial of 5?
% ?- factorial(5, F).


% Base case: the length of an empty list is 0
list_length([], 0).

% Recursive case: the length of a non-empty list is 1 + the length of its tail
list_length([_|T], L) :-
    list_length(T, L1),  % Recursive call to find length of the tail
    L is L1 + 1.         % Add 1 to the result of the recursive call

% Queries:
% 1. What is the length of [cat, dog, elephant]?
% ?- list_length([cat, dog, elephant], L).


% Base case: the sum of an empty list is 0
list_sum([], 0).

% Recursive case: the sum of a non-empty list
list_sum([H|T], Sum) :-
    list_sum(T, Sum1),   % Recursive call to find sum of the tail
    Sum is H + Sum1.     % Add the head to the sum of the tail

% Queries:
% 1. What is the sum of [1, 2, 3, 4]?
% ?- list_sum([1, 2, 3, 4], Sum).
