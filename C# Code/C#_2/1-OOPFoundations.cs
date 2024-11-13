/******************************************************************************
C# OOP Foundations Example - 
Example program that creates a basic C# program that demonstrates:
1. Defining Classes and Creating Objects
2. Fields and Properties – understanding their roles and differences
3. Methods – writing methods, understanding method signatures, and return types
4. Constructors – creating parameterless and parameterized constructors
5. Static vs Instance Members – usage, examples, and when to use each
6. Encapsulation and Access Modifiers – exploring public, private, protected, and internal modifiers
7. Getters and Setters – using properties for controlled access to class members

Explanation:
    Fields and Properties: 
        The brand field is private and accessed through the Brand property, which demonstrates encapsulation.
    
    Constructors: 
        Two constructors are shown—one with parameters and one parameterless—to demonstrate how to initialize objects.
    
    Static vs. Instance Members: 
        The CarCount field and DisplayCarCount method illustrate static members, while brand, Color, and Drive are instance members.
    
    Encapsulation: 
        The StartEngine method is private and called within the public StartCar method, showing how to hide internal implementation details.
    
    Getters and Setters: 
        The Brand property uses a getter and setter to control how the brand field is accessed and modified.
*******************************************************************************/
using System;

namespace OOPFundamentals
{
    // This class represents a simple model for a "Car".
    public class Car
    {
        // Field: A private variable that holds the state of the car's brand
        private string brand;

        // Property: Provides controlled access to the 'brand' field
        public string Brand
        {
            get { return brand; }
            set 
            { 
                // Ensure the brand name is not empty
                if (!string.IsNullOrEmpty(value))
                {
                    brand = value;
                }
                else
                {
                    Console.WriteLine("Brand name cannot be empty.");
                }
            }
        }

        // Auto-implemented property for the color of the car
        public string Color { get; set; }

        // Static field: A shared property across all instances of the Car class
        public static int CarCount = 0;

        // Constructor: Initializes a Car object with brand and color
        public Car(string brand, string color)
        {
            this.Brand = brand; // Using the property to set the value
            this.Color = color; // Directly setting the auto-implemented property
            CarCount++; // Increment the static field to keep track of how many cars have been created
        }

        // Parameterless constructor to show the use of overloaded constructors
        public Car()
        {
            this.Brand = "Unknown";
            this.Color = "Unpainted";
            CarCount++;
        }

        // Method: Represents an action that a Car can perform
        public void Drive()
        {
            Console.WriteLine($"{Brand} is driving.");
        }

        // Static method: An action that applies to the class as a whole, not any particular instance
        public static void DisplayCarCount()
        {
            Console.WriteLine($"Total number of cars created: {CarCount}");
        }

        // Encapsulation: Using private methods for internal class logic
        private void StartEngine()
        {
            Console.WriteLine("Engine started.");
        }

        // Public method that calls the private method
        public void StartCar()
        {
            StartEngine();
            Console.WriteLine("Car is now running.");
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Creating instances using the parameterized constructor
            Car car1 = new Car("Toyota", "Red");
            Car car2 = new Car("Honda", "Blue");

            // Displaying properties and using methods
            Console.WriteLine($"Car 1: Brand = {car1.Brand}, Color = {car1.Color}");
            Console.WriteLine($"Car 2: Brand = {car2.Brand}, Color = {car2.Color}");
            
            car1.Drive(); // Calling an instance method
            car2.StartCar(); // Calling a public method that uses a private method internally

            // Using the static method to display the total number of Car instances
            Car.DisplayCarCount();

            // Creating an instance using the parameterless constructor
            Car car3 = new Car();
            Console.WriteLine($"Car 3: Brand = {car3.Brand}, Color = {car3.Color}");

            // Checking encapsulation by trying to access the private field (should fail)
            // Uncommenting the line below would cause a compile-time error
            // Console.WriteLine(car1.brand); // 'brand' is inaccessible due to its protection level
        }
    }
}
