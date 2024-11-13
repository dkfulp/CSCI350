/******************************************************************************
C# Data Structures Example - 
Example program that creates a basic C# program that demonstrates:
1. Arrays
2. Lists
3. Tuples
4. Enums
5. Structs
6. Dictionaries / Key-Value Pair
7. Queue
8. Stack

*******************************************************************************/
using System;
using System.Collections.Generic;

class Program
{
    static void Main(string[] args)
    {
        // PART 1: Arrays
        int[] numbers = { 1, 2, 3, 4, 5 };
        Console.WriteLine("Array Elements:");
        foreach (int num in numbers)
        {
            Console.WriteLine(num);
        }

        // Allocating a larger array with a specific size
        int[] largeArray = new int[10]; // Array of 10 elements, all initialized to 0
        for (int i = 0; i < largeArray.Length; i++)
        {
            largeArray[i] = i * 10; // Assigning values to the array elements
        }
        Console.WriteLine("\nLarger Array Elements:");
        foreach (int num in largeArray)
        {
            Console.WriteLine(num);
        }

        // 2D Arrays
        int[,] matrix = new int[3, 3];
        matrix[0, 0] = 1; matrix[0, 1] = 2; matrix[0, 2] = 3;
        matrix[1, 0] = 4; matrix[1, 1] = 5; matrix[1, 2] = 6;
        matrix[2, 0] = 7; matrix[2, 1] = 8; matrix[2, 2] = 9;

        Console.WriteLine("\n2D Array Elements:");
        for (int i = 0; i < matrix.GetLength(0); i++)
        {
            for (int j = 0; j < matrix.GetLength(1); j++)
            {
                Console.Write(matrix[i, j] + " ");
            }
            Console.WriteLine();
        }

        /*****************************************************************************/
        // PART 2: Lists
        // A List is a dynamic-size collection that can grow or shrink.
        List<string> names = new List<string> { "Alice", "Bob", "Charlie" };
        names.Add("Dakota"); // Adding an element to the list
        Console.WriteLine("\nList Elements:");
        foreach (string name in names)
        {
            Console.WriteLine(name);
        }

        // Advanced List Operations

        // Inserting elements at a specific index
        names.Insert(2, "Eve"); // Insert "Eve" at index 2
        Console.WriteLine("\nList After Insert:");
        foreach (string name in names)
        {
            Console.WriteLine(name);
        }

        // Removing elements
        names.Remove("Bob"); // Removes the first occurrence of "Bob"
        Console.WriteLine("\nList After Remove:");
        foreach (string name in names)
        {
            Console.WriteLine(name);
        }

        // Removing elements at a specific index
        names.RemoveAt(1); // Removes the element at index 1
        Console.WriteLine("\nList After RemoveAt:");
        foreach (string name in names)
        {
            Console.WriteLine(name);
        }

        // Finding elements
        int indexOfDakota = names.IndexOf("Dakota"); // Get index of "Dakota"
        Console.WriteLine($"\nIndex of 'Dakota': {indexOfDakota}");

        // Checking if an element exists in the list
        bool containsAlice = names.Contains("Alice");
        Console.WriteLine($"List contains 'Alice': {containsAlice}");

        // Sorting the list
        names.Sort(); // Sorts the list in ascending order
        Console.WriteLine("\nList After Sort:");
        foreach (string name in names)
        {
            Console.WriteLine(name);
        }

        // Reversing the list
        names.Reverse(); // Reverses the order of the elements in the list
        Console.WriteLine("\nList After Reverse:");
        foreach (string name in names)
        {
            Console.WriteLine(name);
        }

        // Converting a list to an array
        string[] namesArray = names.ToArray();
        Console.WriteLine("\nList Converted to Array:");
        foreach (string name in namesArray)
        {
            Console.WriteLine(name);
        }

        // Clearing all elements from the list
        names.Clear();
        Console.WriteLine($"\nList After Clear: Count = {names.Count}");


        /*****************************************************************************/
        // PART 3: Tuples
        var human = (Name: "John", Age: 30, IsStudent: false);
        Console.WriteLine($"\nTuple Example: Name = {human.Name}, Age = {human.Age}, IsStudent = {human.IsStudent}");


        /*****************************************************************************/
        // PART 4: Enums
        Days today = Days.Wednesday;
        Console.WriteLine($"\nEnum Example: Today is {today}");


        /*****************************************************************************/
        // PART 5: Structs
        // A basic struct representing a point in 2D space
        Point basicPoint = new Point(10, 20);
        Console.WriteLine($"\nBasic Struct: Point = ({basicPoint.X}, {basicPoint.Y})");

        // Advanced Struct Operations

        // 1. Creating a struct with a parameterized constructor
        Rectangle rect = new Rectangle(10, 5);
        Console.WriteLine($"\nRectangle Struct: Length = {rect.Length}, Width = {rect.Width}, Area = {rect.Area()}");

        // 2. Modifying struct fields (structs are value types)
        Point modifiablePoint = new Point(5, 5);
        Console.WriteLine($"\nOriginal Point: ({modifiablePoint.X}, {modifiablePoint.Y})");
        modifiablePoint.X = 15; // Modifying struct field
        modifiablePoint.Y = 25;
        Console.WriteLine($"Modified Point: ({modifiablePoint.X}, {modifiablePoint.Y})");

        // 3. Using structs with methods
        var circle = new Circle(5);
        Console.WriteLine($"\nCircle Struct: Radius = {circle.Radius}, Area = {circle.Area()}");

        // 4. Structs with properties and validation
        var person = new Person("Dakota", "Smith");
        Console.WriteLine($"\nPerson Struct: FirstName = {person.FirstName}, LastName = {person.LastName}");

        // 5. Copying structs (value types)
        Point point1 = new Point(10, 20);
        Point point2 = point1; // Creates a copy
        point2.X = 30; // Modifies the copy only
        Console.WriteLine($"\nOriginal Point1: ({point1.X}, {point1.Y})");
        Console.WriteLine($"Modified Copy Point2: ({point2.X}, {point2.Y})");

        // 6. Structs with readonly fields and methods
        var immutablePoint = new ImmutablePoint(5, 10);
        Console.WriteLine($"\nImmutablePoint Struct: ({immutablePoint.X}, {immutablePoint.Y})");

        // 7. Using 'ref' and 'out' with structs
        ModifyStruct(ref modifiablePoint);
        Console.WriteLine($"\nModified Point using ref: ({modifiablePoint.X}, {modifiablePoint.Y})");


        /*****************************************************************************/
        // PART 6: Dictionaries
        // A dictionary is a collection of key-value pairs.
        Dictionary<string, int> ages = new Dictionary<string, int>
        {
            { "Alice", 25 },
            { "Bob", 30 },
            { "Charlie", 35 }
        };
        ages["Dakota"] = 28; // Adding a new key-value pair
        Console.WriteLine("\nDictionary Elements:");
        foreach (var kvp in ages)
        {
            Console.WriteLine($"{kvp.Key}: {kvp.Value}");
        }

        // Advanced Dictionary Operations

        // Checking if a key exists
        string keyToCheck = "Alice";
        if (ages.ContainsKey(keyToCheck))
        {
            Console.WriteLine($"\nDictionary contains key '{keyToCheck}': {ages[keyToCheck]}");
        }
        else
        {
            Console.WriteLine($"\nDictionary does not contain key '{keyToCheck}'.");
        }

        // Checking if a value exists
        int valueToCheck = 30;
        bool hasValue = ages.ContainsValue(valueToCheck);
        Console.WriteLine($"\nDictionary contains value '{valueToCheck}': {hasValue}");

        // Removing a key-value pair
        string keyToRemove = "Charlie";
        if (ages.Remove(keyToRemove))
        {
            Console.WriteLine($"\n'{keyToRemove}' removed from dictionary.");
        }
        else
        {
            Console.WriteLine($"\n'{keyToRemove}' not found in dictionary.");
        }

        // Iterating over keys and values separately
        Console.WriteLine("\nDictionary Keys:");
        foreach (string key in ages.Keys)
        {
            Console.WriteLine(key);
        }

        Console.WriteLine("\nDictionary Values:");
        foreach (int value in ages.Values)
        {
            Console.WriteLine(value);
        }

        // Using TryGetValue to safely retrieve a value
        if (ages.TryGetValue("Bob", out int bobAge))
        {
            Console.WriteLine($"\nBob's age: {bobAge}");
        }
        else
        {
            Console.WriteLine("\n'Bob' not found in dictionary.");
        }

        // Updating the value associated with a key
        ages["Dakota"] = 29; // Updates Dakota's age
        Console.WriteLine("\nUpdated Dictionary Elements:");
        foreach (var kvp in ages)
        {
            Console.WriteLine($"{kvp.Key}: {kvp.Value}");
        }

        // Merging dictionaries
        Dictionary<string, int> newAges = new Dictionary<string, int>
        {
            { "Eve", 22 },
            { "Frank", 45 }
        };
        foreach (var kvp in newAges)
        {
            // Add or update existing keys with new values
            ages[kvp.Key] = kvp.Value;
        }

        Console.WriteLine("\nMerged Dictionary Elements:");
        foreach (var kvp in ages)
        {
            Console.WriteLine($"{kvp.Key}: {kvp.Value}");
        }

        // Clearing the dictionary
        ages.Clear();
        Console.WriteLine($"\nDictionary cleared. Count = {ages.Count}");


        /*****************************************************************************/
        // PART 7: Queue
        Queue<string> queue = new Queue<string>();
        queue.Enqueue("First");
        queue.Enqueue("Second");
        queue.Enqueue("Third");
        Console.WriteLine("\nQueue Elements:");
        while (queue.Count > 0)
        {
            Console.WriteLine(queue.Dequeue());
        }

        /*****************************************************************************/
        // PART 8: Stack
        Stack<string> stack = new Stack<string>();
        stack.Push("Bottom");
        stack.Push("Middle");
        stack.Push("Top");
        Console.WriteLine("\nStack Elements:");
        while (stack.Count > 0)
        {
            Console.WriteLine(stack.Pop());
        }

        // PART 9: Wait for user to exit
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }

    // Method to modify a struct using 'ref'
    static void ModifyStruct(ref Point p)
    {
        p.X += 10;
        p.Y += 10;
    }
}

enum Days { Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday };

// Basic struct representing a 2D point
struct Point
{
    public int X { get; set; }
    public int Y { get; set; }

    public Point(int x, int y)
    {
        X = x;
        Y = y;
    }
}

// Advanced struct representing a rectangle
struct Rectangle
{
    public int Length { get; set; }
    public int Width { get; set; }

    public Rectangle(int length, int width)
    {
        Length = length;
        Width = width;
    }

    // Method to calculate area
    public int Area()
    {
        return Length * Width;
    }
}

// Struct representing a circle with a method
struct Circle
{
    public double Radius { get; set; }

    public Circle(double radius)
    {
        Radius = radius;
    }

    // Method to calculate area
    public double Area()
    {
        return Math.PI * Radius * Radius;
    }
}

// Struct with properties and validation
struct Person
{
    private string firstName;
    private string lastName;

    public string FirstName
    {
        get { return firstName; }
        set
        {
            if (!string.IsNullOrEmpty(value))
                firstName = value;
        }
    }

    public string LastName
    {
        get { return lastName; }
        set
        {
            if (!string.IsNullOrEmpty(value))
                lastName = value;
        }
    }

    public Person(string firstName, string lastName)
    {
        this.firstName = firstName;
        this.lastName = lastName;
    }
}

// Struct with readonly fields
struct ImmutablePoint
{
    public int X { get; }
    public int Y { get; }

    public ImmutablePoint(int x, int y)
    {
        X = x;
        Y = y;
    }
}
