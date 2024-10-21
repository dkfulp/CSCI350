/******************************************************************************
Control Structures Example - 
Example program that creates a basic Prolog system that focuses on control 
structures in Prolog, including how to use the cut operator (!), the fail 
predicate (fail), and negation (\+) to manage Prolog’s backtracking behavior 
and introduce logical control flow.

Cut (!) stops backtracking, committing to a solution.
Fail (fail) forces Prolog to backtrack, often used to explore all solutions.
Negation (\+) succeeds when a goal cannot be proven.
If-then-else (->, ;) provides conditional logic in Prolog.

*******************************************************************************/
% Rule: finding the first female child of a parent
first_female_child(Parent, Child) :-
    parent(Parent, Child),      % Check if Parent has a Child
    female(Child),              % Check if Child is female
    !.                          % Cut operator to prevent further solutions

% Facts: parent relationships
parent(john, mary).
parent(john, lisa).
parent(john, sara).

% Facts: gender information
female(mary).
female(lisa).
female(sara).

% Queries:
% 1. Who is the first female child of John?
% ?- first_female_child(john, Child).
/*
In the query ?- first_female_child(john, Child)., Prolog will find Child = mary 
and stop due to the cut operator (!).
The cut operator prevents further backtracking, committing to the first 
solution found.
*/


% Rule: printing all children of a parent
print_children(Parent) :-
    parent(Parent, Child),
    write(Child), nl,           % Print the child's name
    fail.                       % Force backtracking to find all children

% Queries:
% 1. Print all children of John.
% ?- print_children(john).
/*
In the query ?- print_children(john)., Prolog will print mary, lisa, 
and sara, then fail, triggering backtracking to find more children.
The fail predicate is often used in conjunction with side effects 
like write/1 to iterate over all solutions.
*/


% Facts: who owns pets
owns(john, dog).
owns(susan, cat).

% Rule: checking if someone does not own a dog
does_not_own_dog(Person) :-
    \+ owns(Person, dog).

% Queries:
% 1. Does Susan not own a dog?
% ?- does_not_own_dog(susan).
/*
In the query ?- does_not_own_dog(susan)., Prolog will return true 
because there’s no fact proving that Susan owns a dog.
The negation operator checks whether a goal cannot be satisfied.
*/


% Rule: check if someone is the only child
only_child(Person) :-
    parent(Parent, Person),         % Check if the person has a parent
    \+ (parent(Parent, Sibling),    % Ensure there's no sibling
        Sibling \= Person),
    !.                              % Cut to prevent further solutions

% Facts: parent relationships
parent(jack, emma).

% Queries:
% 1. Is Emma an only child?
% ?- only_child(emma).
/*
In the query ?- only_child(emma)., Prolog will return true because Emma 
has no siblings.
The combination of cut (!) and negation (\+) ensures that the rule commits 
to the result and stops further backtracking.
*/


% Rule: check if a person is an adult or minor
age_category(Person, Category) :-
    age(Person, Age),
    (Age >= 18 -> Category = adult ; Category = minor).

% Facts: age information
age(john, 25).
age(susan, 15).

% Queries:
% 1. What is John's age category?
% ?- age_category(john, Category).

% 2. What is Susan's age category?
% ?- age_category(susan, Category).
/*
If-then-else (-> and ;) is used to select between alternatives based on a condition.
In the query ?- age_category(john, Category)., Prolog will return Category = adult.
*/