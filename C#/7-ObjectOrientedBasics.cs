/******************************************************************************
C# Object Oriented Basics Example - 
Example program that creates a basic C# program that demonstrates:
1. Classes and Objects
2. Fields, Properties, Methods
3. Constructors (default, parameterized)
4. Encapsulation and access modifiers
5. Static vs dynamic binding

*******************************************************************************/
using System;

class Program
{
    static void Main(string[] args)
    {
        // PART 1: Classes and Objects

        // Creating an object of the 'Person' class
        Person person1 = new Person("Dakota", 30);
        person1.Introduce();

        // Creating another object of the 'Person' class
        Person person2 = new Person("Alice", 25);
        person2.Introduce();


        /*****************************************************************************/
        // PART 2: Fields, Properties, and Methods

        // Accessing properties
        Console.WriteLine($"\nPerson1: Name = {person1.Name}, Age = {person1.Age}");
        person1.Age = 31; // Modifying a property value
        Console.WriteLine($"Updated Age of Person1: {person1.Age}");

        // Accessing a private field through a method
        Console.WriteLine($"Person1's Private ID: {person1.GetPersonId()}");


        /*****************************************************************************/
        // PART 3: Constructors (default and parameterized)

        // Using default constructor
        Person person3 = new Person();
        Console.WriteLine($"\nPerson3: Name = {person3.Name}, Age = {person3.Age}");

        // Using parameterized constructor
        Person person4 = new Person("Bob", 45);
        Console.WriteLine($"Person4: Name = {person4.Name}, Age = {person4.Age}");


        /*****************************************************************************/
        // PART 4: Encapsulation and Access Modifiers

        // Trying to access private fields directly (will cause error if uncommented)
        // person1.personId = 100; // Not accessible

        // Using methods to interact with private fields
        person1.SetPersonId(200);
        Console.WriteLine($"Updated Person1's Private ID: {person1.GetPersonId()}");


        /*****************************************************************************/
        // PART 5: Static vs Dynamic Binding

        // Dynamic binding: Calls the overridden method
        Animal animal = new Dog();
        animal.Speak();

        // Static binding: Calls the base class method
        Dog dog = new Dog();
        dog.Speak();


        /*****************************************************************************/
        // PART 6: Wait for user to exit
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }
}

// PART 1: Classes and Objects

// Defining a 'Person' class
class Person
{
    /*****************************************************************************/
    // PART 2: Fields, Properties, and Methods

    // Private field
    private int personId;

    // Public properties
    public string Name { get; set; }
    public int Age { get; set; }

    // Method to access private field
    public int GetPersonId()
    {
        return personId;
    }

    // Method to set private field
    public void SetPersonId(int id)
    {
        personId = id;
    }

    // Method to introduce the person
    public void Introduce()
    {
        Console.WriteLine($"Hi, my name is {Name} and I am {Age} years old.");
    }


    /*****************************************************************************/
    // PART 3: Constructors

    // Default constructor
    public Person()
    {
        Name = "Unknown";
        Age = 0;
        personId = 0;
    }

    // Parameterized constructor
    public Person(string name, int age)
    {
        Name = name;
        Age = age;
        personId = new Random().Next(1, 1000); // Generating a random ID
    }
}


/*****************************************************************************/
// PART 5: Static vs Dynamic Binding

// Base class
class Animal
{
    public virtual void Speak()
    {
        Console.WriteLine("The animal makes a sound.");
    }
}

// Derived class
class Dog : Animal
{
    // Overriding the base class method
    public override void Speak()
    {
        Console.WriteLine("The dog barks.");
    }
}
