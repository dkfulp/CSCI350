/******************************************************************************
C# Enhancing Inheritance and Polymorphism Example - 
Example program that creates a basic C# program that demonstrates:
1. Virtual, Override, and New Keywords – in-depth examples and their effects on overriding and method hiding
2. Clarifying Method Overloading vs. Overriding – best practices and applications
3. Advanced Polymorphic Behavior – analyzing and implementing polymorphism in complex systems

virtual Keyword:
    Used in the Employee class to mark DisplayInfo() as a method that can be overridden in derived classes.

override Keyword:
    Used in the Manager and Developer classes to provide a specific implementation for DisplayInfo().

Method Overloading:
    Demonstrated with multiple Work() methods in Employee and Developer classes, showing how methods with the same name but different parameters are used.

new Keyword:
    Used in the Manager class to hide the Work() method from the base class. When called on a Manager object directly, it uses the new implementation. When called on a Manager object cast as Employee, it uses the base class implementation.

Polymorphic Behavior:
    The emp variable (of type Employee) is used to show runtime polymorphism. The DisplayInfo() method called depends on the actual type of the object (e.g., Manager or Developer), not the type of the reference.

Key Points for Students:
    Polymorphism:
        Allows base class references to call derived class methods that are overridden, enabling flexible and dynamic behavior in programs.
    Method Overloading vs. Overriding:
        Overloading is having methods with the same name but different signatures.
        Overriding replaces the base class implementation with a derived class implementation.
    Method Hiding (new Keyword):
        Hides the base class method and introduces a new method in the derived class. The base class method can still be accessed by casting the object to the base type.
*******************************************************************************/
using System;

namespace InheritancePolymorphismRefinement
{
    // Base class representing an Employee
    public class Employee
    {
        public string Name { get; set; }
        public int ID { get; set; }

        // Constructor to initialize employee details
        public Employee(string name, int id)
        {
            Name = name;
            ID = id;
        }

        // Virtual method to be overridden by derived classes
        public virtual void DisplayInfo()
        {
            Console.WriteLine($"Employee Name: {Name}, ID: {ID}");
        }

        // Method overloading: multiple versions of the method with different parameters
        public void Work()
        {
            Console.WriteLine($"{Name} is working on general tasks.");
        }

        public void Work(string task)
        {
            Console.WriteLine($"{Name} is working on {task}.");
        }
    }

    // Derived class representing a Manager
    public class Manager : Employee
    {
        public int NumberOfReports { get; set; }

        // Constructor to initialize manager details, calling the base class constructor
        public Manager(string name, int id, int numberOfReports) : base(name, id)
        {
            NumberOfReports = numberOfReports;
        }

        // Overriding the base class method
        public override void DisplayInfo()
        {
            base.DisplayInfo(); // Call the base class implementation
            Console.WriteLine($"Number of Reports: {NumberOfReports}");
        }

        // Method hiding using 'new' keyword
        public new void Work()
        {
            Console.WriteLine($"{Name} is strategizing for the team.");
        }
    }

    // Derived class representing a Developer
    public class Developer : Employee
    {
        public string ProgrammingLanguage { get; set; }

        // Constructor to initialize developer details, calling the base class constructor
        public Developer(string name, int id, string programmingLanguage) : base(name, id)
        {
            ProgrammingLanguage = programmingLanguage;
        }

        // Overriding the base class method
        public override void DisplayInfo()
        {
            base.DisplayInfo(); // Call the base class implementation
            Console.WriteLine($"Programming Language: {ProgrammingLanguage}");
        }

        // Overloading Work method with different parameters
        public void Work(int hours)
        {
            Console.WriteLine($"{Name} is coding in {ProgrammingLanguage} for {hours} hours.");
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Base class reference pointing to a derived class object (polymorphism)
            Employee emp = new Manager("Alice", 1001, 5);
            emp.DisplayInfo(); // Calls the overridden method in Manager
            emp.Work(); // Calls the base class Work method because Work() in Manager is hidden, not overridden

            Console.WriteLine();

            // Demonstrating polymorphism with Developer
            emp = new Developer("Bob", 1002, "C#");
            emp.DisplayInfo(); // Calls the overridden method in Developer

            // Demonstrating method overloading
            Developer dev = new Developer("Charlie", 1003, "Python");
            dev.Work(); // Calls the base class Work method
            dev.Work(5); // Calls the overloaded Work method specific to Developer

            Console.WriteLine();

            // Demonstrating method hiding with 'new'
            Manager mgr = new Manager("David", 1004, 10);
            mgr.Work(); // Calls the new Work method specific to Manager

            // Casting to base type to show the effect of method hiding
            ((Employee)mgr).Work(); // Calls the base class Work method

            Console.WriteLine();

            // Polymorphic behavior with an array of Employee references
            Employee[] employees = {
                new Manager("Emma", 1005, 3),
                new Developer("Frank", 1006, "JavaScript"),
                new Employee("George", 1007)
            };

            foreach (Employee e in employees)
            {
                e.DisplayInfo(); // Calls the appropriate overridden method
                e.Work(); // Demonstrates method behavior based on type and method hiding/overriding
                Console.WriteLine();
            }
        }
    }
}
