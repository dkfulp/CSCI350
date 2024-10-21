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
    write('Enter your name: '),     % Prompt for input
    read(Name),                     % Read input
    write('Hello, '),               % Write greeting
    write(Name), 
    write('!'), nl.

% Queries:
% 1. Greet the user.
% ?- greet_user.
/*
write/1 prints text to the console.
read/1 reads user input.
nl/0 prints a newline.
*/


% Facts: a list of animals
animals([cat, dog, elephant, tiger, lion]).

% Queries:
% 1. What is the length of the animal list?
% ?- animals(List), length(List, Len).

% 2. Is 'tiger' a member of the animal list?
% ?- animals(List), member(tiger, List).

% 3. Append two lists: [1, 2] and [3, 4].
% ?- append([1, 2], [3, 4], Result).

/*
length/2 calculates the length of a list.
member/2 checks if an element is in a list.
append/3 concatenates two lists.
*/


% Facts: who owns which pets
owns(john, dog).
owns(susan, cat).
owns(mary, bird).
owns(john, fish).
owns(susan, rabbit).

% Using findall/3 to get all pets owned by John
all_johns_pets(Pets) :-
    findall(Pet, owns(john, Pet), Pets).

% Using setof/3 to get a sorted list of pet owners
all_pet_owners(Owners) :-
    setof(Owner, Pet^owns(Owner, Pet), Owners).

% Queries:
% 1. Find all pets owned by John.
% ?- all_johns_pets(Pets).

% 2. Find all unique pet owners.
% ?- all_pet_owners(Owners).
/*
findall/3 collects all solutions of a goal into a list, ignoring duplicates
and order.
bagof/3 collects all solutions of a goal, but keeps duplicates and organizes
results based on free variables.
setof/3 collects all solutions of a goal into a sorted list of unique elements.
*/


% Sorting a list
sort_list(Unsorted, Sorted) :-
    sort(Unsorted, Sorted).

% Queries:
% 1. Sort the list [3, 1, 4, 1, 5, 9].
% ?- sort_list([3, 1, 4, 1, 5, 9], Sorted).
/*
sort/2 removes duplicates and sorts elements in ascending order.
*/


% Rule: finding the maximum of two numbers
max_of_two(X, Y, Max) :-
    Max is max(X, Y).

% Rule: finding the minimum of two numbers
min_of_two(X, Y, Min) :-
    Min is min(X, Y).

% Queries:
% 1. What is the maximum of 10 and 20?
% ?- max_of_two(10, 20, Max).

% 2. What is the minimum of 15 and 5?
% ?- min_of_two(15, 5, Min).
/*
max/2 and min/2 find the maximum and minimum of two numbers.
abs/1 calculates the absolute value of a number.
*/