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

% Step-by-step explanation for the query:

% 1. Initial call: first_female_child(john, Child).
%    - Matches the rule: first_female_child(Parent, Child).
%    - Sets Parent = john.

% 2. Check parent-child relationship:
%    - First match: parent(john, mary).
%      - 'mary' is a child of 'john'.
%    - Checks if 'mary' is female using the fact: female(mary).
%      - 'mary' is female, so it matches.

% 3. Cut operator (!):
%    - Prevents further backtracking after finding 'mary'.
%    - The search stops once 'mary' is found, even if there are other female children.

% Final result: Child = mary.
/*
In the query ?- first_female_child(john, Child)., Prolog will find Child = mary 
and stop due to the cut operator (!).
The cut operator prevents further backtracking, committing to the first 
solution found.
*/


/******************************************************************************/
% Rule: printing all children of a parent
print_children(Parent) :-
    parent(Parent, Child),    % Find a child of the given Parent
    write(Child), nl,         % Print the childs name followed by a newline
    fail.                     % Force backtracking to find more children

% Queries:
% 1. Print all children of John.
% ?- print_children(john).

% Step-by-step explanation for the query:

% 1. Initial call: print_children(john).
%    - Matches the rule: print_children(Parent).
%    - Sets Parent = john.

% 2. Check parent-child relationship:
%    - First match: parent(john, mary).
%    - 'mary' is a child of 'john'.
%    - Calls write(mary), which prints 'mary'.
%    - Calls nl, which prints a newline.
%    - Executes fail, forcing backtracking to find the next child.

% 3. Backtracking:
%    - Second match: parent(john, lisa).
%    - 'lisa' is a child of 'john'.
%    - Calls write(lisa), which prints 'lisa'.
%    - Calls nl, which prints a newline.
%    - Executes fail, forcing backtracking to find the next child.

% 4. Backtracking:
%    - Third match: parent(john, sara).
%    - 'sara' is a child of 'john'.
%    - Calls write(sara), which prints 'sara'.
%    - Calls nl, which prints a newline.
%    - Executes fail, but there are no more children to find.

% 5. No more parent-child matches:
%    - Backtracking ends, and the rule completes.

% Final output:
% mary
% lisa
% sara
/*
In the query ?- print_children(john)., Prolog will print mary, lisa, 
and sara, then fail, triggering backtracking to find more children.
The fail predicate is often used in conjunction with side effects 
like write/1 to iterate over all solutions.
*/


/******************************************************************************/
% Facts: who owns pets
owns(john, dog).
owns(susan, cat).

% Rule: checking if someone does not own a dog
does_not_own_dog(Person) :-
    \+ owns(Person, dog).   % Succeeds if 'Person' does not own a dog

% Queries:
% 1. Does Susan not own a dog?
% ?- does_not_own_dog(susan).

% Step-by-step explanation for the query:

% 1. Initial call: does_not_own_dog(susan).
%    - Matches the rule: does_not_own_dog(Person).
%    - Sets Person = susan.

% 2. Check the negation: \+ owns(susan, dog).
%    - The \+ operator checks if the goal 'owns(susan, dog)' fails.
%    - In this case, there is no fact 'owns(susan, dog)'.

% 3. Since 'owns(susan, dog)' is false:
%    - The negation succeeds, meaning 'susan' does not own a dog.

% Final result: true.
/*
In the query ?- does_not_own_dog(susan)., Prolog will return true 
because there’s no fact proving that Susan owns a dog.
The negation operator checks whether a goal cannot be satisfied.
*/


/******************************************************************************/
% Rule: check if someone is the only child
only_child(Person) :-
    parent(Parent, Person),         % Check if the person has a parent
    \+ (parent(Parent, Sibling),    % Ensure theres no sibling
        Sibling \= Person),
    !.                              % Cut to prevent further solutions

% Facts: parent relationships
parent(jack, emma).

% Queries:
% 1. Is Emma an only child?
% ?- only_child(emma).

% Step-by-step explanation for the query:

% 1. Initial call: only_child(emma).
%    - Matches the rule: only_child(Person).
%    - Sets Person = emma.

% 2. Check parent-child relationship:
%    - Finds parent(jack, emma), meaning 'emma' has a parent 'jack'.

% 3. Check for siblings:
%    - The negation \+ (parent(jack, Sibling), Sibling \= emma) is evaluated.
%    - This means: "There is no Sibling of 'emma' who also has 'jack' as a parent."

% 4. Since there is no fact indicating another child of 'jack':
%    - The condition \+ (parent(jack, Sibling), Sibling \= emma) succeeds.
%    - The cut operator (!) prevents further solutions from being considered.

% Final result: true.
/*
In the query ?- only_child(emma)., Prolog will return true because Emma 
has no siblings.
The combination of cut (!) and negation (\+) ensures that the rule commits 
to the result and stops further backtracking.
*/


/******************************************************************************/
% Rule: check if a person is an adult or minor
age_category(Person, Category) :-
    age(Person, Age),                   % Find the age of the person
    (Age >= 18 -> Category = adult ;    % If age is 18 or more, set category to 'adult'
     Category = minor).                 % Otherwise, set category to 'minor'

% Facts: age information
age(john, 25).
age(susan, 15).

% Queries:
% 1. What is Johns age category?
% ?- age_category(john, Category).

% Step-by-step explanation for the first query:

% 1. Initial call: age_category(john, Category).
%    - Matches the rule: age_category(Person, Category).
%    - Sets Person = john.

% 2. Call to age(john, Age).
%    - Finds age(john, 25).
%    - Sets Age = 25.

% 3. Check the age condition: (Age >= 18).
%    - Age = 25, which is greater than 18.
%    - The condition succeeds, so Category is set to 'adult'.

% Final result for query 1: Category = adult.

% 2. What is Susans age category?
% ?- age_category(susan, Category).

% Step-by-step explanation for the second query:

% 1. Initial call: age_category(susan, Category).
%    - Matches the rule: age_category(Person, Category).
%    - Sets Person = susan.

% 2. Call to age(susan, Age).
%    - Finds age(susan, 15).
%    - Sets Age = 15.

% 3. Check the age condition: (Age >= 18).
%    - Age = 15, which is less than 18.
%    - The condition fails, so the alternative sets Category to 'minor'.

% Final result for query 2: Category = minor.
/*
If-then-else (-> and ;) is used to select between alternatives based on a condition.
In the query ?- age_category(john, Category)., Prolog will return Category = adult.
*/