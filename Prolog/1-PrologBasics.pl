/******************************************************************************
Basics Example - 
Example program that creates a basic Prolog system that includes Facts, Rules, 
and Queries

Atoms: These are lowercase words like cat, dog, sparrow, which are used to represent 
constants or objects.

Variables: Begin with an uppercase letter or underscore, like X, What, or WhichBird. 
Variables are placeholders that Prolog tries to unify with facts or rules.

Facts: Simple statements that declare relationships or properties, such as 
animal(cat). or pet(dog)..

Rules: Define logic by combining facts and or other rules. For instance, 
is_pet(X) :- animal(X), pet(X). says that an entity X is a pet if it is both 
an animal and listed as a pet.

Queries: Allow users to ask questions about the facts and rules. For example, 
?- is_pet(What). asks which animals are pets.

*******************************************************************************/
% Facts
% Declaring simple facts about animals
animal(cat).
animal(dog).
animal(elephant).

% Facts about pets
pet(cat).
pet(dog).

% Facts about bird species
bird(sparrow).
bird(parrot).
bird(penguin).

% Rules
% Declaring a rule: An animal is a pet if it is listed as a pet
is_pet(X) :- animal(X), pet(X).

% Declaring another rule: A bird can fly if it is not an eagle
can_fly(X) :- bird(X), X \= penguin.

% Queries:
% 1. What animals are pets?
% ?- is_pet(What).

% 2. Which birds can fly?
% ?- can_fly(WhichBird).
