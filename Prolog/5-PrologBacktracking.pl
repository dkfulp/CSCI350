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
/*
When running the query ?- find_parent(mary, Parent)., Prolog will first find 
Parent = john, then backtrack and find Parent = susan. 
Backtracking allows Prolog to find multiple solutions, one at a time, by 
exploring different facts that match the goal.
*/


% Rule: finding siblings
sibling(X, Y) :-
    parent(P, X),     % X and Y have the same parent
    parent(P, Y),
    X \= Y.           % X and Y are not the same person

% Queries:
% 1. Who are the siblings of Mary?
% ?- sibling(mary, Sibling).
/*
Backtracking ensures that Prolog checks all possible parents of X and Y to 
find all siblings.
For the query ?- sibling(mary, Sibling)., Prolog will first find Sibling = mike 
and then stop when no more solutions are possible.
*/


% Rule: inserting an element into a list
insert(X, List, [X|List]).
insert(X, [H|T], [H|T1]) :-
    insert(X, T, T1).

% Rule: finding permutations of a list
permute([], []).
permute([H|T], P) :-
    permute(T, PT),
    insert(H, PT, P).

% Queries:
% 1. What are the permutations of [a, b, c]?
% ?- permute([a, b, c], P).
/*
Prolog uses backtracking to generate all permutations of the list.
In the query ?- permute([a, b, c], P)., Prolog will find all possible 
permutations, such as [a, b, c], [a, c, b], [b, a, c], etc.
*/


% Rule: finding combinations of K elements from a list
combination(0, _, []).
combination(K, [H|T], [H|Comb]) :-
    K > 0,
    K1 is K - 1,
    combination(K1, T, Comb).
combination(K, [_|T], Comb) :-
    K > 0,
    combination(K, T, Comb).

% Queries:
% 1. What are the combinations of 2 elements from [a, b, c]?
% ?- combination(2, [a, b, c], Comb).
/*
This rule finds combinations of K elements from a list.
Prolog’s backtracking helps find all possible combinations 
that satisfy the condition.
In the query ?- combination(2, [a, b, c], Comb)., Prolog will 
find [a, b], [a, c], and [b, c].
*/


% Rule: finding the first male child of a parent
first_male_child(Parent, Child) :-
    parent(Parent, Child),
    male(Child),      % Assuming male/1 fact is defined
    !.                % Cut operator to prevent further backtracking

% Facts: gender information
male(mike).
male(tom).

% Queries:
% 1. Who is the first male child of Mary?
% ?- first_male_child(mary, Child).
/*
The cut operator (!) stops Prolog from backtracking past this point.
In the query ?- first_male_child(mary, Child)., Prolog will find 
Child = tom and stop further exploration.
*/
