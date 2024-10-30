/******************************************************************************
Built-In Function Example - 
Example program that creates a basic Prolog system that  explores built-in 
predicates and functions in Prolog, which are crucial for effective programming. 
We'll cover some commonly used built-in predicates for input/output, list processing, 
and set operations.

Built-ins simplify list processing, input/output, set operations, and arithmetic.
Prolog's built-ins are efficient, versatile, and used extensively in practical 
applications.
Understanding the use of built-ins like findall/3, setof/3, and sort/2 is key 
for data processing in Prolog.

*******************************************************************************/
% Rule: greeting a user by name
greet_user :-
    write('Enter your name: '),     % Prompt the user to enter their name
    read(Name),                     % Read the user's input into the variable 'Name'
    write('Hello, '),               % Write the beginning of the greeting
    write(Name),                    % Write the user's name
    write('!'), nl.                 % Write an exclamation mark and move to a new line

% Queries:
% 1. Greet the user.
% ?- greet_user.

% Step-by-step explanation for the query:

% 1. Initial call: greet_user.
%    - Matches the rule: greet_user.

% 2. write('Enter your name: ').
%    - Displays the prompt "Enter your name: " to the console.

% 3. read(Name).
%    - Waits for the user to input a name.
%    - Stores the entered name in the variable 'Name'.
%    - For example, if the user inputs 'John', then Name = 'John'.

% 4. write('Hello, ').
%    - Writes "Hello, " to the console.

% 5. write(Name).
%    - Writes the value of 'Name' (e.g., 'John') to the console.

% 6. write('!'), nl.
%    - Writes "!" to the console, followed by a newline.

% Final output:
% If the user inputs 'John', the output will be:
% Enter your name: John
% Hello, John!
/*
write/1 prints text to the console.
read/1 reads user input.
nl/0 prints a newline.
*/


/******************************************************************************/
% Facts: a list of animals
animals([cat, dog, elephant, tiger, lion]).

% Queries:
% 1. What is the length of the animal list?
% ?- animals(List), length(List, Len).

% Step-by-step explanation for the first query:

% 1. Initial call: animals(List), length(List, Len).
%    - Matches the fact: animals([cat, dog, elephant, tiger, lion]).
%    - Sets List = [cat, dog, elephant, tiger, lion].

% 2. Call to length(List, Len).
%    - Uses the built-in length/2 predicate to find the length of the list.
%    - The list has 5 elements.
%    - Sets Len = 5.

% Final result for query 1: Len = 5.


% 2. Is 'tiger' a member of the animal list?
% ?- animals(List), member(tiger, List).

% Step-by-step explanation for the second query:

% 1. Initial call: animals(List), member(tiger, List).
%    - Matches the fact: animals([cat, dog, elephant, tiger, lion]).
%    - Sets List = [cat, dog, elephant, tiger, lion].

% 2. Call to member(tiger, List).
%    - Uses the built-in member/2 predicate to check if 'tiger' is in the list.
%    - Finds 'tiger' in the list [cat, dog, elephant, tiger, lion].
%    - Returns true.

% Final result for query 2: true.


% 3. Append two lists: [1, 2] and [3, 4].
% ?- append([1, 2], [3, 4], Result).

% Step-by-step explanation for the third query:

% 1. Initial call: append([1, 2], [3, 4], Result).
%    - Uses the built-in append/3 predicate to concatenate the two lists.

% 2. The append/3 predicate combines [1, 2] and [3, 4].
%    - Result = [1, 2, 3, 4].

% Final result for query 3: Result = [1, 2, 3, 4].
/*
length/2 calculates the length of a list.
member/2 checks if an element is in a list.
append/3 concatenates two lists.
*/


/******************************************************************************/
% Facts: who owns which pets
owns(john, dog).
owns(susan, cat).
owns(mary, bird).
owns(john, fish).
owns(susan, rabbit).

% Using findall/3 to get all pets owned by John
all_johns_pets(Pets) :-
    findall(Pet, owns(john, Pet), Pets).  % Collects all pets owned by John into a list

% Using setof/3 to get a sorted list of pet owners
all_pet_owners(Owners) :-
    setof(Owner, Pet^owns(Owner, Pet), Owners).  % Collects unique, sorted list of pet owners

% Queries:
% 1. Find all pets owned by John.
% ?- all_johns_pets(Pets).

% Step-by-step explanation for the first query:

% 1. Initial call: all_johns_pets(Pets).
%    - Matches the rule: all_johns_pets(Pets).
%    - Uses findall/3 to collect all Pet values for which owns(john, Pet) is true.

% 2. findall(Pet, owns(john, Pet), Pets).
%    - Iterates through the facts:
%      a. First match: owns(john, dog) -> Pet = dog.
%      b. Second match: owns(john, fish) -> Pet = fish.

% 3. Collects these pets into a list: Pets = [dog, fish].

% Final result for query 1: Pets = [dog, fish].


% 2. Find all unique pet owners.
% ?- all_pet_owners(Owners).

% Step-by-step explanation for the second query:

% 1. Initial call: all_pet_owners(Owners).
%    - Matches the rule: all_pet_owners(Owners).
%    - Uses setof/3 to collect unique owners for all Pet values in the owns/2 predicate.

% 2. setof(Owner, Pet^owns(Owner, Pet), Owners).
%    - Iterates through the facts:
%      a. First match: owns(john, dog) -> Owner = john.
%      b. Second match: owns(susan, cat) -> Owner = susan.
%      c. Third match: owns(mary, bird) -> Owner = mary.
%      d. Fourth match: owns(john, fish) -> Owner = john.
%      e. Fifth match: owns(susan, rabbit) -> Owner = susan.

% 3. Collects and sorts the unique owners into a list: Owners = [john, mary, susan].

% Final result for query 2: Owners = [john, mary, susan].
/*
findall/3 collects all solutions of a goal into a list, ignoring duplicates
and order.
bagof/3 collects all solutions of a goal, but keeps duplicates and organizes
results based on free variables.
setof/3 collects all solutions of a goal into a sorted list of unique elements.
*/


/******************************************************************************/
% Sorting a list
sort_list(Unsorted, Sorted) :-
    sort(Unsorted, Sorted).    % Uses the built-in sort/2 predicate to sort the list

% Queries:
% 1. Sort the list [3, 1, 4, 1, 5, 9].
% ?- sort_list([3, 1, 4, 1, 5, 9], Sorted).

% Step-by-step explanation for the query:

% 1. Initial call: sort_list([3, 1, 4, 1, 5, 9], Sorted).
%    - Matches the rule: sort_list(Unsorted, Sorted).
%    - Sets Unsorted = [3, 1, 4, 1, 5, 9].

% 2. Call to sort(Unsorted, Sorted).
%    - The built-in sort/2 predicate sorts the list in ascending order and removes duplicates.

% 3. Sorting process:
%    - The list [3, 1, 4, 1, 5, 9] is sorted to [1, 3, 4, 5, 9].
%    - The duplicate element '1' is removed, as sort/2 eliminates duplicates.

% Final result: Sorted = [1, 3, 4, 5, 9].
/*
sort/2 removes duplicates and sorts elements in ascending order.
*/


/******************************************************************************/
% Rule: finding the maximum of two numbers
max_of_two(X, Y, Max) :-
    Max is max(X, Y).      % Calculate Max as the greater of X and Y

% Rule: finding the minimum of two numbers
min_of_two(X, Y, Min) :-
    Min is min(X, Y).      % Calculate Min as the lesser of X and Y

% Queries:
% 1. What is the maximum of 10 and 20?
% ?- max_of_two(10, 20, Max).

% Step-by-step explanation for the first query:

% 1. Initial call: max_of_two(10, 20, Max).
%    - Matches the rule: max_of_two(X, Y, Max).
%    - Sets X = 10, Y = 20.

% 2. Calculate Max using max(10, 20).
%    - Evaluates to Max = 20.

% Final result for query 1: Max = 20.


% 2. What is the minimum of 15 and 5?
% ?- min_of_two(15, 5, Min).

% Step-by-step explanation for the second query:

% 1. Initial call: min_of_two(15, 5, Min).
%    - Matches the rule: min_of_two(X, Y, Min).
%    - Sets X = 15, Y = 5.

% 2. Calculate Min using min(15, 5).
%    - Evaluates to Min = 5.

% Final result for query 2: Min = 5.
/*
max/2 and min/2 find the maximum and minimum of two numbers.
abs/1 calculates the absolute value of a number.
*/