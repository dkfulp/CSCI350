/******************************************************************************
C# Core OOP Principles Example - 
Example program that creates a basic C# program that demonstrates:
1. Inheritance Basics – creating base and derived classes, and understanding the : base keyword
2. Method Overloading – implementing and understanding compile-time polymorphism
3. Method Overriding – using virtual, override, and base keywords for run-time polymorphism
4. Accessing Base Class Members – calling base class constructors and methods

Inheritance Basics: 
    The Dog class inherits from the Animal class using the : base keyword in its constructor to call the Animal constructor.

Method Overloading: 
    The SayHello method in the Animal class is overloaded to show multiple versions with different parameter lists.

Method Overriding: 
    The MakeSound method in the Dog class overrides the base class's MakeSound method using the override keyword, providing a specific implementation for Dog.

Accessing Base Class Members: 
    The BaseSound method in the Dog class calls the base class version of MakeSound using base.MakeSound().

Key Points for Students:
    Polymorphism: The Animal reference animalRef pointing to a Dog instance demonstrates runtime polymorphism where the overridden MakeSound method in Dog is called.
    Method Overloading vs. Overriding: Overloading involves multiple methods with the same name but different parameters, while overriding changes the behavior of an inherited method in a derived class.
    base Keyword: Used to call base class methods and constructors, allowing derived classes to access and extend base class functionality.
*******************************************************************************/
using System;

namespace CoreOOPPrinciples
{
    // Base class representing a generic Animal
    public class Animal
    {
        // Property to hold the name of the animal
        public string Name { get; set; }

        // Constructor that initializes the name
        public Animal(string name)
        {
            Name = name;
            Console.WriteLine($"{Name} is an animal.");
        }

        // Method that describes the sound an animal makes
        // The virtual keyword in C# is used to indicate that a 
        // method or property in a base class can be overridden in any derived class.
        public virtual void MakeSound()
        {
            Console.WriteLine($"{Name} makes a generic animal sound.");
        }

        // Method Overloading: SayHello method with different parameter lists
        public void SayHello()
        {
            Console.WriteLine("Hello!");
        }

        public void SayHello(string greeting)
        {
            Console.WriteLine(greeting);
        }
    }

    // Derived class representing a Dog, inheriting from Animal
    public class Dog : Animal
    {
        // Property specific to Dog
        public string Breed { get; set; }

        // Constructor that calls the base class constructor using ': base'
        public Dog(string name, string breed) : base(name)
        {
            Breed = breed;
            Console.WriteLine($"{Name} is a {Breed}.");
        }

        // Method Overriding: Providing a more specific implementation for Dog
        public override void MakeSound()
        {
            Console.WriteLine($"{Name} barks: Woof Woof!");
        }

        // Accessing base class method
        public void BaseSound()
        {
            // Call the base class's MakeSound method
            base.MakeSound();
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Creating an instance of the base class
            Animal genericAnimal = new Animal("Generic Animal");
            genericAnimal.MakeSound(); // Calls the base class method

            // Using method overloading
            genericAnimal.SayHello(); // Calls the method without parameters
            genericAnimal.SayHello("Greetings from the animal kingdom!"); // Calls the overloaded method

            Console.WriteLine();

            // Creating an instance of the derived class
            Dog dog = new Dog("Buddy", "Golden Retriever");
            dog.MakeSound(); // Calls the overridden method in the Dog class

            // Demonstrating base class method access
            dog.BaseSound(); // Calls the base class's MakeSound method

            // Demonstrating inherited method from the base class
            dog.SayHello("Woof! Hello from Buddy!");

            Console.WriteLine();

            // Showing polymorphism through method overriding
            Animal animalRef = dog; // Base class reference pointing to a derived class object
            animalRef.MakeSound(); // Calls the Dog class's overridden method due to runtime polymorphism
        }
    }
}
