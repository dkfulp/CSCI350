/******************************************************************************
C# Methods Example - 
Example program that creates a basic C# program that demonstrates:
1. Declaring and calling methods
2. Method parameters and return values
3. Value vs reference parameter passing
4. Method overloading
5. Static vs instance methods
6. Extension methods
7. Generics

*******************************************************************************/
using System;

class Program
{
    static void Main(string[] args)
    {
        // PART 1: Declaring and Calling Methods

        // Calling a simple method
        Greet();

        // Calling a method with parameters and return value
        int sum = Add(5, 7);
        Console.WriteLine($"Sum: {sum}");


        /*****************************************************************************/
        // PART 2: Method Parameters and Return Values

        // Method with parameters and different return types
        double result = Divide(10, 3);
        Console.WriteLine($"Division Result: {result}");


        /*****************************************************************************/
        // PART 3: Value vs Reference Parameter Passing

        // Passing by value
        int num = 5;
        Console.WriteLine($"\nBefore PassByValue: {num}");
        PassByValue(num);
        Console.WriteLine($"After PassByValue: {num}");

        // Passing by reference
        int refNum = 5;
        Console.WriteLine($"\nBefore PassByRef: {refNum}");
        PassByRef(ref refNum);
        Console.WriteLine($"After PassByRef: {refNum}");


        /*****************************************************************************/
        // PART 4: Method Overloading

        // Calling overloaded methods
        Console.WriteLine($"\nOverloaded Add (int, int): {Add(5, 7)}");
        Console.WriteLine($"Overloaded Add (double, double): {Add(5.5, 7.5)}");


        /*****************************************************************************/
        // PART 5: Static vs Instance Methods

        // Calling a static method
        Console.WriteLine($"\nStatic Method: {MathUtilities.Multiply(3, 4)}");

        // Calling an instance method
        MathUtilities mathUtil = new MathUtilities();
        Console.WriteLine($"Instance Method: {mathUtil.Subtract(10, 3)}");


        /*****************************************************************************/
        // PART 6: Extension Methods

        // Using an extension method on a string
        string originalString = "hello";
        Console.WriteLine($"\nOriginal String: {originalString}");
        Console.WriteLine($"Reversed String: {originalString.ReverseString()}");


        /*****************************************************************************/
        // PART 7: Generics

        // Using a generic method
        Console.WriteLine($"\nGeneric Add (int): {GenericAdd(5, 7)}");
        Console.WriteLine($"Generic Add (double): {GenericAdd(5.5, 7.5)}");

        // Using a generic class
        Box<string> stringBox = new Box<string>("Hello, Generics!");
        Console.WriteLine($"Boxed Value: {stringBox.Value}");


        /*****************************************************************************/
        // PART 8: Wait for user to exit
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }

    // PART 1: Declaring and Calling Methods
    static void Greet()
    {
        Console.WriteLine("Hello, World!");
    }

    static int Add(int a, int b)
    {
        return a + b;
    }


    /*****************************************************************************/
    // PART 2: Method Parameters and Return Values
    static double Divide(int a, int b)
    {
        return (double)a / b; // Casting to double for decimal division
    }


    /*****************************************************************************/
    // PART 3: Value vs Reference Parameter Passing
    static void PassByValue(int num)
    {
        num += 5; // Changes will not affect the original variable
    }

    static void PassByRef(ref int num)
    {
        num += 5; // Changes will affect the original variable
    }


    /*****************************************************************************/
    // PART 4: Method Overloading
    static int Add(int a, int b)
    {
        return a + b;
    }

    static double Add(double a, double b)
    {
        return a + b;
    }


    /*****************************************************************************/
    // PART 5: Static vs Instance Methods
    class MathUtilities
    {
        // Static method
        public static int Multiply(int a, int b)
        {
            return a * b;
        }

        // Instance method
        public int Subtract(int a, int b)
        {
            return a - b;
        }
    }


    /*****************************************************************************/
    // PART 6: Extension Methods
}

// Extension method definition
static class StringExtensions
{
    public static string ReverseString(this string str)
    {
        char[] charArray = str.ToCharArray();
        Array.Reverse(charArray);
        return new string(charArray);
    }
}


/*****************************************************************************/
// PART 7: Generics

// Generic method
static T GenericAdd<T>(T a, T b) where T : struct
{
    dynamic da = a;
    dynamic db = b;
    return da + db;
}

// Generic class
class Box<T>
{
    public T Value { get; private set; }

    public Box(T value)
    {
        Value = value;
    }
}
