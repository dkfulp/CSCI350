/******************************************************************************
C# Polymorphism and Abstraction Example - 
Example program that creates a basic C# program that demonstrates:
1. Run-Time Polymorphism – dynamic binding through method overriding
2. Interfaces – defining and implementing interfaces for flexible code structures
3. Abstract Classes and Methods – using abstract classes as blueprints
4. Interfaces vs. Abstract Classes – understanding their distinctions and use cases

Abstract Classes:
    Used when you want to provide a common base class that can include both implemented and abstract (unimplemented) methods.
    Ideal when classes share some common behavior but need to enforce specific method implementations in derived classes.

Interfaces:
    Used to define a contract that any implementing class must follow.
    A class can implement multiple interfaces, providing more flexibility than single inheritance.

Differences between Abstract Classes and Interfaces:
    Abstract classes can have fields, constructors, and both abstract and non-abstract methods, while interfaces only have method signatures and properties (without implementation until C# 8.0 and above, which allows default implementations).
    Abstract classes support inheritance of implementation, while interfaces do not.

Practical Usage:
    Use abstract classes when classes share common functionality but also need to enforce certain behaviors.
    Use interfaces when you want to enforce certain behaviors without enforcing any inheritance hierarchy, promoting more flexible code.
*******************************************************************************/
using System;

namespace PolymorphismAndAbstraction
{
    // Abstract class: Represents a blueprint for different types of animals
    // An abstract class cannot be instantiated directly. It can have abstract methods (no implementation) and non-abstract methods (with implementation).
    // The MakeSound method is abstract and must be implemented by any derived class.
    // The Eat method is a regular method shared by all animals.
    public abstract class Animal
    {
        // Abstract method: Must be implemented by derived classes
        public abstract void MakeSound();

        // Non-abstract method: Common behavior shared among all animals
        public void Eat()
        {
            Console.WriteLine("This animal is eating.");
        }
    }

    // Interface: Defines behavior that implementing classes must have
    // An interface cannot have any implementation. It only defines the method signature that any implementing class must follow.
    // IWalkable has the Walk method, which any class that implements the interface must define.
    public interface IWalkable
    {
        void Walk(); // Method declaration (no implementation)
    }

    // Derived class inheriting from the abstract class and implementing an interface
    public class Dog : Animal, IWalkable
    {
        // Implementing the abstract method from Animal
        public override void MakeSound()
        {
            Console.WriteLine("The dog barks: Woof Woof!");
        }

        // Implementing the method from the IWalkable interface
        public void Walk()
        {
            Console.WriteLine("The dog walks on four legs.");
        }
    }

    // Another class implementing only the interface
    public class Robot : IWalkable
    {
        // Implementing the method from the IWalkable interface
        public void Walk()
        {
            Console.WriteLine("The robot walks on mechanical legs.");
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Demonstrating polymorphism with abstract classes
            Animal animal = new Dog(); // Animal reference holding a Dog object
            animal.MakeSound(); // Calls the overridden MakeSound method in Dog
            animal.Eat(); // Calls the shared method in the abstract class

            Console.WriteLine();

            // Demonstrating interface usage
            IWalkable walkableDog = new Dog();
            walkableDog.Walk(); // Calls the Walk method in Dog

            IWalkable walkableRobot = new Robot();
            walkableRobot.Walk(); // Calls the Walk method in Robot

            Console.WriteLine();

            // Example of polymorphic behavior through interfaces
            IWalkable[] walkables = { walkableDog, walkableRobot };
            foreach (var walker in walkables)
            {
                walker.Walk(); // Each object calls its own implementation of Walk
            }
        }
    }
}
