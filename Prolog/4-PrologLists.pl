/******************************************************************************
Lists Example - 
Example program that creates a basic Prolog system that focuses on list 
manipulation in Prolog, using basic operations like extracting elements, 
appending, and reversing lists. This will provide a good foundation for 
working with lists in Prolog.

Lists are a fundamental data structure in Prolog.
Manipulation of lists is done using pattern matching and recursion.
Understanding the base case and recursive case is crucial for working with lists.

*******************************************************************************/
% Rule to find the first element of a list
first_element([H|_], H).

% Rule to find the last element of a list
last_element([X], X).           % Base case: the list has one element
last_element([_|T], X) :-       % Recursive case: check the tail of the list
    last_element(T, X).

% Queries:
% 1. What is the first element of [a, b, c, d]?
% ?- first_element([a, b, c, d], X).

% 2. What is the last element of [a, b, c, d]?
% ?- last_element([a, b, c, d], X).


% Base case: appending an empty list to L gives L
append_list([], L, L).

% Recursive case: adding head of List1 to the result of 
% appending the tail of List1 to List2
append_list([H|T], L2, [H|L3]) :-
    append_list(T, L2, L3).

% Queries:
% 1. What is the result of appending [1, 2] to [3, 4]?
% ?- append_list([1, 2], [3, 4], X).


% Base case: the reverse of an empty list is an empty list
reverse_list([], []).

% Recursive case: reverse the tail and append the head at the end
reverse_list([H|T], Rev) :-
    reverse_list(T, RevT),         % Recursive call to reverse the tail
    append_list(RevT, [H], Rev).   % Append the head to the reversed tail

% Queries:
% 1. What is the reverse of [1, 2, 3, 4]?
% ?- reverse_list([1, 2, 3, 4], X).


% Base case: element X is found at the head of the list
is_member(X, [X|_]).

% Recursive case: check if X is in the tail of the list
is_member(X, [_|T]) :-
    is_member(X, T).

% Queries:
% 1. Is 'b' a member of [a, b, c, d]?
% ?- is_member(b, [a, b, c, d]).
