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

% Step-by-step explanation for the first query:
% 1. Initial call: first_element([a, b, c, d], X).
%    - The list matches the pattern [H|_], where H is the head of the list.
%    - H = a, so X = a.
%    - Returns X = a. Final result: X = a.

% 2. What is the last element of [a, b, c, d]?
% ?- last_element([a, b, c, d], X).

% Step-by-step explanation for the second query:
% 1. Initial call: last_element([a, b, c, d], X).
%    - The list is not a single element, so it proceeds to the recursive case.
%    - The tail is [b, c, d].
%    - Recursive call: last_element([b, c, d], X).

% 2. Second call: last_element([b, c, d], X).
%    - The list is still not a single element, so it proceeds to the recursive case.
%    - The tail is [c, d].
%    - Recursive call: last_element([c, d], X).

% 3. Third call: last_element([c, d], X).
%    - The list is still not a single element, so it proceeds to the recursive case.
%    - The tail is [d].
%    - Recursive call: last_element([d], X).

% 4. Base case: last_element([d], X).
%    - The list has one element, so it matches the base case.
%    - X = d.
%    - Returns X = d. Final result: X = d.


/******************************************************************************/
% Base case: appending an empty list to L gives L
append_list([], L, L).

% Recursive case: adding head of List1 to the result of 
% appending the tail of List1 to List2
append_list([H|T], L2, [H|L3]) :-
    append_list(T, L2, L3).

% Queries:
% 1. What is the result of appending [1, 2] to [3, 4]?
% ?- append_list([1, 2], [3, 4], X).

% Step-by-step explanation for the query:
% 1. Initial call: append_list([1, 2], [3, 4], X).
%    - The list is non-empty, so it matches the recursive case.
%    - H = 1, the head of the first list.
%    - T = [2], the tail of the first list.
%    - L2 = [3, 4], the second list.
%    - Recursive call: append_list([2], [3, 4], L3).

% 2. Second call: append_list([2], [3, 4], L3).
%    - The list is still non-empty, so it matches the recursive case.
%    - H = 2, the head of the first list.
%    - T = [], the tail of the first list.
%    - L2 = [3, 4], the second list.
%    - Recursive call: append_list([], [3, 4], L3).

% 3. Base case: append_list([], [3, 4], L3).
%    - The first list is empty, so it matches the base case.
%    - L3 = [3, 4].
%    - Returns L3 = [3, 4] to the previous call.

% 4. Back to second call: [H|L3] becomes [2|[3, 4]] = [2, 3, 4].
%    - Returns [2, 3, 4] to the previous call.

% 5. Back to initial call: [H|L3] becomes [1|[2, 3, 4]] = [1, 2, 3, 4].
%    - Final result: X = [1, 2, 3, 4].


/******************************************************************************/
% Base case: the reverse of an empty list is an empty list
reverse_list([], []).

% Recursive case: reverse the tail and append the head at the end
reverse_list([H|T], Rev) :-
    reverse_list(T, RevT),         % Recursive call to reverse the tail
    append_list(RevT, [H], Rev).   % Append the head to the reversed tail

% Queries:
% 1. What is the reverse of [1, 2, 3, 4]?
% ?- reverse_list([1, 2, 3, 4], X).

% Step-by-step explanation for the query:
% 1. Initial call: reverse_list([1, 2, 3, 4], X).
%    - The list is non-empty, so it matches the recursive case.
%    - H = 1, the head of the list.
%    - T = [2, 3, 4], the tail of the list.
%    - Recursive call: reverse_list([2, 3, 4], RevT).

% 2. Second call: reverse_list([2, 3, 4], RevT).
%    - The list is still non-empty, so it matches the recursive case.
%    - H = 2, the head of the list.
%    - T = [3, 4], the tail of the list.
%    - Recursive call: reverse_list([3, 4], RevT).

% 3. Third call: reverse_list([3, 4], RevT).
%    - The list is still non-empty, so it matches the recursive case.
%    - H = 3, the head of the list.
%    - T = [4], the tail of the list.
%    - Recursive call: reverse_list([4], RevT).

% 4. Fourth call: reverse_list([4], RevT).
%    - The list is still non-empty, so it matches the recursive case.
%    - H = 4, the head of the list.
%    - T = [], the tail of the list.
%    - Recursive call: reverse_list([], RevT).

% 5. Base case: reverse_list([], RevT).
%    - Matches the base case, so RevT = [].
%    - Returns RevT = [] to the previous call.

% 6. Back to fourth call: append_list([], [4], Rev).
%    - Appends [4] to [], so Rev = [4].
%    - Returns Rev = [4] to the previous call.

% 7. Back to third call: append_list([4], [3], Rev).
%    - Appends [3] to [4], so Rev = [4, 3].
%    - Returns Rev = [4, 3] to the previous call.

% 8. Back to second call: append_list([4, 3], [2], Rev).
%    - Appends [2] to [4, 3], so Rev = [4, 3, 2].
%    - Returns Rev = [4, 3, 2] to the previous call.

% 9. Back to initial call: append_list([4, 3, 2], [1], Rev).
%    - Appends [1] to [4, 3, 2], so Rev = [4, 3, 2, 1].
%    - Final result: X = [4, 3, 2, 1].


/******************************************************************************/
% Base case: element X is found at the head of the list
is_member(X, [X|_]).

% Recursive case: check if X is in the tail of the list
is_member(X, [_|T]) :-
    is_member(X, T).

% Queries:
% 1. Is 'b' a member of [a, b, c, d]?
% ?- is_member(b, [a, b, c, d]).

% Step-by-step explanation for the query:
% 1. Initial call: is_member(b, [a, b, c, d]).
%    - The head of the list is 'a', which is not 'b'.
%    - Matches the recursive case.
%    - The tail of the list is [b, c, d].
%    - Recursive call: is_member(b, [b, c, d]).

% 2. Second call: is_member(b, [b, c, d]).
%    - The head of the list is 'b', which matches X.
%    - Matches the base case.
%    - Returns true. Final result: X is a member of the list.

