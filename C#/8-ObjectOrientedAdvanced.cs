/******************************************************************************
C# Object Oriented Advanced Example - 
Example program that creates a basic C# program that demonstrates:
1. Inheritance
2. Polymorphism
3. Abstract classes and interfaces
4. Interfaces vs Abstract classes
5. Overriding methods and properties
6. Sealed classes and methods
7. Dependency Injection

*******************************************************************************/
using System;

// Main program class
class Program
{
    static void Main(string[] args)
    {
        // PART 1: Inheritance and Polymorphism

        // Creating objects of derived classes
        Animal myDog = new Dog();
        Animal myCat = new Cat();

        // Demonstrating polymorphism
        myDog.Speak(); // Calls Dog's Speak()
        myCat.Speak(); // Calls Cat's Speak()


        /*****************************************************************************/
        // PART 2: Abstract Classes

        // Creating an object of the derived abstract class
        Vehicle myCar = new Car();
        myCar.Start();
        myCar.Drive();


        /*****************************************************************************/
        // PART 3: Interfaces

        // Using an interface
        IWorker myEmployee = new Employee();
        myEmployee.Work();


        /*****************************************************************************/
        // PART 4: Overriding Methods and Properties

        // Creating a Manager object and demonstrating overriding
        Manager myManager = new Manager();
        myManager.Work();
        Console.WriteLine($"Manager's Salary: {myManager.Salary}");


        /*****************************************************************************/
        // PART 5: Sealed Classes

        // Creating a RegularEmployee object from a sealed class
        RegularEmployee regularEmployee = new RegularEmployee();
        regularEmployee.Work();


        /*****************************************************************************/
        // PART 6: Dependency Injection

        // Demonstrating dependency injection
        ILogger consoleLogger = new ConsoleLogger();
        Application app = new Application(consoleLogger);
        app.Run();

        // Wait for user to exit
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }
}

// PART 1: Inheritance and Polymorphism

// Base class
class Animal
{
    public virtual void Speak()
    {
        Console.WriteLine("The animal makes a sound.");
    }
}

// Derived class - Dog
class Dog : Animal
{
    public override void Speak()
    {
        Console.WriteLine("The dog barks.");
    }
}

// Derived class - Cat
class Cat : Animal
{
    public override void Speak()
    {
        Console.WriteLine("The cat meows.");
    }
}


/*****************************************************************************/
// PART 2: Abstract Classes

// Abstract base class
abstract class Vehicle
{
    public abstract void Start(); // Abstract method

    public virtual void Drive() // Virtual method
    {
        Console.WriteLine("The vehicle is driving.");
    }
}

// Derived class from abstract class
class Car : Vehicle
{
    public override void Start()
    {
        Console.WriteLine("The car is starting.");
    }

    public override void Drive()
    {
        Console.WriteLine("The car is driving smoothly.");
    }
}


/*****************************************************************************/
// PART 3: Interfaces

// Interface definition
interface IWorker
{
    void Work(); // Interface method
}

// Class implementing interface
class Employee : IWorker
{
    public void Work()
    {
        Console.WriteLine("The employee is working.");
    }
}


/*****************************************************************************/
// PART 4: Overriding Methods and Properties

// Base class
class Worker
{
    public virtual double Salary { get; set; } = 30000;

    public virtual void Work()
    {
        Console.WriteLine("The worker is working.");
    }
}

// Derived class with overriding
class Manager : Worker
{
    public override double Salary { get; set; } = 60000;

    public override void Work()
    {
        Console.WriteLine("The manager is managing the team.");
    }
}


/*****************************************************************************/
// PART 5: Sealed Classes

// Sealed class
sealed class RegularEmployee : IWorker
{
    public void Work()
    {
        Console.WriteLine("The regular employee is working on tasks.");
    }
}


/*****************************************************************************/
// PART 6: Dependency Injection

// Logger interface
interface ILogger
{
    void Log(string message);
}

// Console logger implementing ILogger
class ConsoleLogger : ILogger
{
    public void Log(string message)
    {
        Console.WriteLine($"Log: {message}");
    }
}

// Application class demonstrating dependency injection
class Application
{
    private readonly ILogger _logger;

    // Constructor accepting a dependency
    public Application(ILogger logger)
    {
        _logger = logger;
    }

    public void Run()
    {
        _logger.Log("Application is running.");
    }
}
