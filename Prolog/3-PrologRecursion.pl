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

% Step-by-step explanation for the query:
% 1. Initial call: factorial(5, F).
%    - N = 5, N > 0, so proceed.
%    - N1 is 5 - 1 = 4.
%    - Recursive call: factorial(4, F1).

% 2. Second call: factorial(4, F1).
%    - N = 4, N > 0, so proceed.
%    - N1 is 4 - 1 = 3.
%    - Recursive call: factorial(3, F1).

% 3. Third call: factorial(3, F1).
%    - N = 3, N > 0, so proceed.
%    - N1 is 3 - 1 = 2.
%    - Recursive call: factorial(2, F1).

% 4. Fourth call: factorial(2, F1).
%    - N = 2, N > 0, so proceed.
%    - N1 is 2 - 1 = 1.
%    - Recursive call: factorial(1, F1).

% 5. Fifth call: factorial(1, F1).
%    - N = 1, N > 0, so proceed.
%    - N1 is 1 - 1 = 0.
%    - Recursive call: factorial(0, F1).

% 6. Base case: factorial(0, F1).
%    - Matches the base case, F1 = 1.
%    - Returns F1 = 1 to the previous call.

% 7. Back to fifth call: F is 1 * 1 = 1. Returns F = 1 to the previous call.
% 8. Back to fourth call: F is 2 * 1 = 2. Returns F = 2 to the previous call.
% 9. Back to third call: F is 3 * 2 = 6. Returns F = 6 to the previous call.
% 10. Back to second call: F is 4 * 6 = 24. Returns F = 24 to the previous call.
% 11. Back to initial call: F is 5 * 24 = 120. Final result: F = 120.


/******************************************************************************/
% Base case: the length of an empty list is 0
list_length([], 0).

% Recursive case: the length of a non-empty list is 1 + the length of its tail
list_length([_|T], L) :-
    list_length(T, L1),  % Recursive call to find the length of the tail
    L is L1 + 1.         % Add 1 to the result of the recursive call

% Queries:
% 1. What is the length of [cat, dog, elephant]?
% ?- list_length([cat, dog, elephant], L).

% Step-by-step explanation for the query:
% 1. Initial call: list_length([cat, dog, elephant], L).
%    - The list is non-empty, so the code proceeds to the recursive case.
%    - The tail of the list is [dog, elephant].
%    - Recursive call: list_length([dog, elephant], L1).

% 2. Second call: list_length([dog, elephant], L1).
%    - The list is still non-empty, so the code proceeds.
%    - The tail of the list is [elephant].
%    - Recursive call: list_length([elephant], L1).

% 3. Third call: list_length([elephant], L1).
%    - The list is still non-empty, so the code proceeds.
%    - The tail of the list is [] (empty list).
%    - Recursive call: list_length([], L1).

% 4. Base case: list_length([], L1).
%    - Matches the base case, so L1 = 0.
%    - Returns L1 = 0 to the previous call.

% 5. Back to third call: L is 0 + 1 = 1. Returns L = 1 to the previous call.
% 6. Back to second call: L is 1 + 1 = 2. Returns L = 2 to the previous call.
% 7. Back to initial call: L is 2 + 1 = 3. Final result: L = 3.


/******************************************************************************/
% Base case: the sum of an empty list is 0
list_sum([], 0).

% Recursive case: the sum of a non-empty list
list_sum([H|T], Sum) :-
    list_sum(T, Sum1),   % Recursive call to find the sum of the tail
    Sum is H + Sum1.     % Add the head to the sum of the tail

% Queries:
% 1. What is the sum of [1, 2, 3, 4]?
% ?- list_sum([1, 2, 3, 4], Sum).

% Step-by-step explanation for the query:
% 1. Initial call: list_sum([1, 2, 3, 4], Sum).
%    - The list is non-empty, so the code proceeds to the recursive case.
%    - H = 1, the head of the list.
%    - The tail is [2, 3, 4].
%    - Recursive call: list_sum([2, 3, 4], Sum1).

% 2. Second call: list_sum([2, 3, 4], Sum1).
%    - The list is still non-empty, so the code proceeds.
%    - H = 2, the head of the list.
%    - The tail is [3, 4].
%    - Recursive call: list_sum([3, 4], Sum1).

% 3. Third call: list_sum([3, 4], Sum1).
%    - The list is still non-empty, so the code proceeds.
%    - H = 3, the head of the list.
%    - The tail is [4].
%    - Recursive call: list_sum([4], Sum1).

% 4. Fourth call: list_sum([4], Sum1).
%    - The list is still non-empty, so the code proceeds.
%    - H = 4, the head of the list.
%    - The tail is [], an empty list.
%    - Recursive call: list_sum([], Sum1).

% 5. Base case: list_sum([], Sum1).
%    - Matches the base case, so Sum1 = 0.
%    - Returns Sum1 = 0 to the previous call.

% 6. Back to fourth call: Sum is 4 + 0 = 4. Returns Sum = 4 to the previous call.
% 7. Back to third call: Sum is 3 + 4 = 7. Returns Sum = 7 to the previous call.
% 8. Back to second call: Sum is 2 + 7 = 9. Returns Sum = 9 to the previous call.
% 9. Back to initial call: Sum is 1 + 9 = 10. Final result: Sum = 10.

