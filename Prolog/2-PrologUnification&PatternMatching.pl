/******************************************************************************
Unification and Pattern Matching Example - 
Example program that creates a basic Prolog system that focuses on unification 
and pattern matching, which are central to Prolog's reasoning. This code will 
demonstrate how Prolog unifies variables with facts and how pattern matching works.

Unification:

    Prolog tries to match variables with facts or other variables to satisfy queries.
    For example, child(X, john) unifies X with the child of john, giving mary and 
    mike as answers.

Pattern Matching with Lists:

    The list notation [H|T] is used to match the head (H) and tail (T) of a list.
    The first_element([H|_], H) rule extracts the first element of a list.
    The rest_of_list([_|T], T) rule extracts the rest of the list, excluding the first element.
    The is_member(X, List) rule checks if X is a member of a list using recursion.

*******************************************************************************/
% Facts: family relationships
parent(john, mary).
parent(john, mike).
parent(susan, mary).
parent(susan, mike).
parent(mary, ann).
parent(mary, tom).

% Facts: gender information
male(john).
male(mike).
male(tom).
female(susan).
female(mary).
female(ann).

% Unification Examples
% Rule to find a child of a parent
child(X, Y) :- parent(Y, X).

% Rule to check if someone is male or female
is_male(X) :- male(X).
is_female(X) :- female(X).

% Pattern Matching with Lists
% Facts: list of animals
animals([cat, dog, elephant, tiger, lion]).

% Rule to match the first element of a list
first_element([H|_], H).

% Rule to match the rest of the list (tail)
rest_of_list([_|T], T).

% Rule to check if an element is in a list
is_member(X, [X|_]).
is_member(X, [_|T]) :- is_member(X, T).

% Queries:
% 1. Who is John's child?
% ?- child(Child, john).

% 2. What is the first animal in the list?
% ?- animals(AnimalList), first_element(AnimalList, FirstAnimal).
% ?- animals(AnimalList), rest_of_list(AnimalList, LastAnimals).

% 3. Is 'elephant' a member of the animal list?
% ?- animals(AnimalList), is_member(elephant, AnimalList).
