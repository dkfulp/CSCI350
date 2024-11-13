/******************************************************************************
C# Design and Architecture of OOP Systems Example - 
Example program that creates a basic C# program that demonstrates:
1. Class Relationships – association, aggregation, and composition with practical examples
2. Has-A vs. Is-A Relationships – applying composition and inheritance for complex designs
3. Basic Design Patterns – introduction to common patterns like Singleton and Factory

Class Relationships (Has-A and Is-A):
    Is-A Relationship: The Car and Truck classes are derived from the Vehicle class, meaning Car and Truck are types of Vehicle.
    Has-A Relationship: The Car and Truck classes have an Engine object, demonstrating composition.

Design Patterns:
    Singleton Pattern: The Logger class ensures that only one instance is created and shared globally.
    Factory Pattern: The VehicleFactory class provides a way to create different Vehicle types based on input parameters.

Key Points for Students:
    Singleton Pattern: Used when a single instance of a class is needed throughout the application, such as for logging.
    Factory Pattern: Encapsulates the logic of creating objects, promoting flexibility and code maintainability.
*******************************************************************************/
using System;

namespace OOPDesignArchitecture
{
    // Class representing a simple Engine
    public class Engine
    {
        public int Horsepower { get; set; }

        public Engine(int horsepower)
        {
            Horsepower = horsepower;
            Console.WriteLine($"Engine with {Horsepower} HP created.");
        }

        public void Start()
        {
            Console.WriteLine("Engine started.");
        }
    }

    // Base class representing a Vehicle (demonstrating an "Is-A" relationship)
    public class Vehicle
    {
        public string Brand { get; set; }
        public string Model { get; set; }

        public Vehicle(string brand, string model)
        {
            Brand = brand;
            Model = model;
            Console.WriteLine($"{Brand} {Model} vehicle created.");
        }

        public void Drive()
        {
            Console.WriteLine($"{Brand} {Model} is driving.");
        }
    }

    // Derived class representing a Car, which "Is-A" Vehicle
    public class Car : Vehicle
    {
        // "Has-A" relationship: Car has an Engine
        private Engine engine;

        public Car(string brand, string model, Engine engine) : base(brand, model)
        {
            this.engine = engine;
        }

        public void StartCar()
        {
            engine.Start();
            Console.WriteLine($"{Brand} {Model} is ready to go!");
        }
    }

    // Derived class representing a Truck, which "Is-A" Vehicle
    public class Truck : Vehicle
    {
        private Engine engine;

        public Truck(string brand, string model, Engine engine) : base(brand, model)
        {
            this.engine = engine;
        }

        public void StartTruck()
        {
            engine.Start();
            Console.WriteLine($"{Brand} {Model} truck is ready to haul!");
        }
    }

    // Singleton pattern for a Logger class
    public class Logger
    {
        private static Logger _instance;
        private static readonly object _lock = new object();

        private Logger() { }

        public static Logger GetInstance()
        {
            if (_instance == null)
            {
                // Lock ensures thread safety
                // Prevents multiple threads from creating separate instances of the Logger class
                lock (_lock)
                {
                    if (_instance == null)
                    {
                        _instance = new Logger();
                        Console.WriteLine("Logger instance created.");
                    }
                }
            }
            return _instance;
        }

        public void Log(string message)
        {
            Console.WriteLine($"Log entry: {message}");
        }
    }

    // Factory pattern for creating different types of vehicles
    public static class VehicleFactory
    {
        public static Vehicle CreateVehicle(string type, string brand, string model, Engine engine = null)
        {
            switch (type.ToLower())
            {
                case "car":
                    return new Car(brand, model, engine);
                case "truck":
                    return new Truck(brand, model, engine);
                default:
                    Console.WriteLine("Vehicle type not recognized.");
                    return null;
            }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Demonstrating class relationships and "Has-A" vs. "Is-A"
            Engine carEngine = new Engine(200);
            Vehicle myCar = new Car("Toyota", "Corolla", carEngine);
            myCar.Drive(); // Calls method from base class (Vehicle)
            ((Car)myCar).StartCar(); // Calls method specific to Car

            Console.WriteLine();

            // Demonstrating Singleton pattern
            Logger logger1 = Logger.GetInstance();
            Logger logger2 = Logger.GetInstance();
            logger1.Log("Car started.");
            logger2.Log("Driving in progress.");

            Console.WriteLine($"Are logger1 and logger2 the same instance? {logger1 == logger2}");

            Console.WriteLine();

            // Demonstrating Factory pattern
            Vehicle newCar = VehicleFactory.CreateVehicle("car", "Honda", "Civic", new Engine(180));
            if (newCar != null)
            {
                newCar.Drive();
            }
        }
    }
}
