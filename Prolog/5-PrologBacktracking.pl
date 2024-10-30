/******************************************************************************
Backtracking Example - 
Example program that creates a basic Prolog system that focuses on backtracking 
in Prolog, which is essential for exploring multiple solutions and finding all 
possible answers that satisfy a given query. Prolog’s backtracking mechanism tries 
to satisfy rules and facts, exploring all possible solutions by going back and 
trying different paths when a failure occurs.

Prolog tries to satisfy a goal and, if it fails, it backtracks to try alternative paths.
Backtracking is useful for finding all possible solutions to a query.
The cut operator (!) can be used to limit backtracking when needed.

*******************************************************************************/
% Facts: parent relationships
parent(john, mary).
parent(john, mike).
parent(susan, mary).
parent(susan, mike).
parent(mary, ann).
parent(mary, tom).

% Rule: finding all parents of a child
find_parent(Child, Parent) :-
    parent(Parent, Child).

% Queries:
% 1. Who are the parents of Mary?
% ?- find_parent(mary, Parent).

% Step-by-step explanation for the query:
% 1. Initial call: find_parent(mary, Parent).
%    - Matches the rule: find_parent(Child, Parent).
%    - Searches for facts where 'mary' is the child.

% 2. First match: parent(john, mary).
%    - 'Parent' becomes 'john'.
%    - Returns Parent = john.

% 3. Second match: parent(susan, mary).
%    - 'Parent' becomes 'susan'.
%    - Returns Parent = susan.

% Final result: Parent = john; Parent = susan.
/*
When running the query ?- find_parent(mary, Parent)., Prolog will first find 
Parent = john, then backtrack and find Parent = susan. 
Backtracking allows Prolog to find multiple solutions, one at a time, by 
exploring different facts that match the goal.
*/


/******************************************************************************/
% Rule: finding siblings
sibling(X, Y) :-
    parent(P, X),     % X and Y have the same parent
    parent(P, Y),
    X \= Y.           % X and Y are not the same person

% Queries:
% 1. Who are the siblings of Mary?
% ?- sibling(mary, Sibling).

% Step-by-step explanation for the query:
% 1. Initial call: sibling(mary, Sibling).
%    - Matches the rule: sibling(X, Y).
%    - Sets X = mary and looks for Y where both X and Y share a parent.

% 2. Check for parent relationships:
%    - First match: parent(john, mary) and parent(john, mike).
%      - X = mary, Y = mike, P = john.
%      - X \= Y, so Sibling = mike.
%      - Returns Sibling = mike.

% 3. Check for additional parent relationships:
%    - Second match: parent(susan, mary) and parent(susan, mike).
%      - X = mary, Y = mike, P = susan.
%      - X \= Y, so Sibling = mike.
%      - Returns Sibling = mike again.

% Final result: Sibling = mike (appears twice due to two different parent facts).
/*
Backtracking ensures that Prolog checks all possible parents of X and Y to 
find all siblings.
For the query ?- sibling(mary, Sibling)., Prolog will first find Sibling = mike 
and then stop when no more solutions are possible.
*/


/******************************************************************************/
% Rule: inserting an element into a list
insert(X, List, [X|List]).          % Base case: insert X at the beginning of the list
insert(X, [H|T], [H|T1]) :-         % Recursive case: insert X into the tail of the list
    insert(X, T, T1).

% Rule: finding permutations of a list
permute([], []).                    % Base case: the permutation of an empty list is an empty list
permute([H|T], P) :-                % Recursive case: find permutations by inserting the head in all positions
    permute(T, PT),                 % Recursively find permutations of the tail
    insert(H, PT, P).               % Insert the head into all positions of the permutation of the tail

% Queries:
% 1. What are the permutations of [a, b, c]?
% ?- permute([a, b, c], P).

% Step-by-step explanation for the query:

% 1. Initial call: permute([a, b, c], P).
%    - The list is non-empty, so it matches the recursive case.
%    - H = a, the head of the list.
%    - T = [b, c], the tail of the list.
%    - Recursive call: permute([b, c], PT).

% 2. Second call: permute([b, c], PT).
%    - The list is non-empty, so it matches the recursive case.
%    - H = b, the head of the list.
%    - T = [c], the tail of the list.
%    - Recursive call: permute([c], PT).

% 3. Third call: permute([c], PT).
%    - The list is non-empty, so it matches the recursive case.
%    - H = c, the head of the list.
%    - T = [], the tail of the list.
%    - Recursive call: permute([], PT).

% 4. Base case: permute([], PT).
%    - The list is empty, so it matches the base case.
%    - PT = [].
%    - Returns PT = [] to the previous call.

% 5. Back to third call: insert(c, [], P).
%    - Inserts 'c' into all positions of the empty list, resulting in P = [c].
%    - Returns P = [c] to the previous call.

% 6. Back to second call: insert(b, [c], P).
%    - Inserts 'b' into all positions of [c], resulting in P = [b, c] and P = [c, b].
%    - Returns P = [b, c] and P = [c, b] to the previous call.

% 7. Back to initial call: insert(a, [b, c], P) and insert(a, [c, b], P).
%    - Inserts 'a' into all positions of [b, c], resulting in P = [a, b, c], [b, a, c], and [b, c, a].
%    - Inserts 'a' into all positions of [c, b], resulting in P = [a, c, b], [c, a, b], and [c, b, a].

% Final result: P = [a, b, c], [b, a, c], [b, c, a], [a, c, b], [c, a, b], [c, b, a].
/*
Prolog uses backtracking to generate all permutations of the list.
In the query ?- permute([a, b, c], P)., Prolog will find all possible 
permutations, such as [a, b, c], [a, c, b], [b, a, c], etc.
*/


/******************************************************************************/
% Rule: finding combinations of K elements from a list
combination(0, _, []).            % Base case: 0 elements needed, combination is an empty list
combination(K, [H|T], [H|Comb]) :- % Recursive case: include the head in the combination
    K > 0,                        % Ensure K is greater than 0
    K1 is K - 1,                  % Decrease K by 1
    combination(K1, T, Comb).     % Find the remaining combination from the tail
combination(K, [_|T], Comb) :-    % Recursive case: exclude the head and check the tail
    K > 0,                        % Ensure K is greater than 0
    combination(K, T, Comb).

% Queries:
% 1. What are the combinations of 2 elements from [a, b, c]?
% ?- combination(2, [a, b, c], Comb).

% Step-by-step explanation for the query:

% 1. Initial call: combination(2, [a, b, c], Comb).
%    - K = 2, H = a, T = [b, c].
%    - Matches the second rule: include 'a' in the combination.
%    - K1 = 2 - 1 = 1.
%    - Recursive call: combination(1, [b, c], Comb).

% 2. Second call: combination(1, [b, c], Comb).
%    - K = 1, H = b, T = [c].
%    - Matches the second rule: include 'b' in the combination.
%    - K1 = 1 - 1 = 0.
%    - Recursive call: combination(0, [c], Comb).

% 3. Third call: combination(0, [c], Comb).
%    - K = 0, matches the base case.
%    - Comb = [].
%    - Returns Comb = [] to the previous call.

% 4. Back to second call: Comb = [b|[]] = [b].
%    - Returns Comb = [b] to the previous call.

% 5. Back to initial call: Comb = [a|[b]] = [a, b].
%    - Returns Comb = [a, b].

% 6. Next, consider the other recursive branch where 'a' is not included:
%    - Initial call again: combination(2, [a, b, c], Comb).
%    - This time, match the third rule to exclude 'a'.
%    - Recursive call: combination(2, [b, c], Comb).

% 7. Fourth call: combination(2, [b, c], Comb).
%    - K = 2, H = b, T = [c].
%    - Matches the second rule: include 'b' in the combination.
%    - K1 = 2 - 1 = 1.
%    - Recursive call: combination(1, [c], Comb).

% 8. Fifth call: combination(1, [c], Comb).
%    - K = 1, H = c, T = [].
%    - Matches the second rule: include 'c' in the combination.
%    - K1 = 1 - 1 = 0.
%    - Recursive call: combination(0, [], Comb).

% 9. Base case: combination(0, [], Comb).
%    - K = 0, matches the base case.
%    - Comb = [].
%    - Returns Comb = [] to the previous call.

% 10. Back to fifth call: Comb = [c|[]] = [c].
%     - Returns Comb = [c] to the previous call.

% 11. Back to fourth call: Comb = [b|[c]] = [b, c].
%     - Returns Comb = [b, c].

% Final result: Comb = [a, b]; Comb = [b, c].
/*
This rule finds combinations of K elements from a list.
Prolog’s backtracking helps find all possible combinations 
that satisfy the condition.
In the query ?- combination(2, [a, b, c], Comb)., Prolog will 
find [a, b], [a, c], and [b, c].
*/


/******************************************************************************/
% Rule: finding the first male child of a parent
first_male_child(Parent, Child) :-
    parent(Parent, Child),  % Check if 'Child' is a child of 'Parent'
    male(Child),            % Check if 'Child' is male
    !.                      % Cut operator to prevent further backtracking

% Facts: gender information
male(mike).
male(tom).

% Queries:
% 1. Who is the first male child of Mary?
% ?- first_male_child(mary, Child).

% Step-by-step explanation for the query:

% 1. Initial call: first_male_child(mary, Child).
%    - Matches the rule: first_male_child(Parent, Child).
%    - Sets Parent = mary.

% 2. Checks parent-child relationship:
%    - parent(mary, ann) is found, but ann is not male.
%    - Continues to the next parent fact.

% 3. Next parent-child match: parent(mary, tom).
%    - 'tom' is a child of 'mary' and is male (as defined by male(tom)).
%    - Matches both conditions: parent(Parent, Child) and male(Child).

% 4. Cut operator (!):
%    - Prevents further backtracking after finding the first male child.
%    - The search stops once 'tom' is found, and no further male children are considered.

% Final result: Child = tom.
/*
The cut operator (!) stops Prolog from backtracking past this point.
In the query ?- first_male_child(mary, Child)., Prolog will find 
Child = tom and stop further exploration.
*/
