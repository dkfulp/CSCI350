/******************************************************************************
C# Basics Example - 
Example program that creates a basic C# program that demonstrates:
1. Main method and program structure
2. Variables, data types, constants, and statics
3. Input and Output
4. Comments and code formatting

*******************************************************************************/
// This is the namespace declaration. It groups related classes, interfaces, and functions together.
// "System" is a built-in namespace that provides fundamental classes and base methods.
using System;

// This is the class declaration. In C#, all code must be inside a class.
// "Program" is the name of our class.
class Program
{
    // PART 3: Constants and Static Variables
    // A static variable is shared among all instances of the class.
    // It retains its value between method calls.
    static int staticCounter = 0;

    
    // This is the Main method, the entry point of any C# console application.
    // When the program starts, the Main method is executed first.
    static void Main(string[] args)
    {
        // PART 1: Basic Output
        // This line prints "Hello, World!" to the console.
        Console.WriteLine("Hello, World!");


        /*****************************************************************************/
        // PART 2: Variables and Primitive Data Types
        // Integer types
        byte smallNumber = 255; // 8-bit unsigned integer
        sbyte signedSmallNumber = -128; // 8-bit signed integer
        short shortNumber = -32768; // 16-bit signed integer
        ushort unsignedShortNumber = 65535; // 16-bit unsigned integer
        int number = 10; // 32-bit signed integer
        uint unsignedNumber = 4294967295; // 32-bit unsigned integer
        long longNumber = -9223372036854775808; // 64-bit signed integer
        ulong unsignedLongNumber = 18446744073709551615; // 64-bit unsigned integer

        // Floating-point types
        float floatNumber = 3.14f; // Single-precision floating-point
        double doubleNumber = 19.99; // Double-precision floating-point
        decimal decimalNumber = 79228162514264337593543950335m; // High-precision decimal for financial calculations

        // Character and Boolean types
        char singleCharacter = 'A'; // Single 16-bit Unicode character
        bool isTrue = true; // Boolean (true/false)

        // String type (not primitive, but commonly used)
        string name = "Dakota";

        // Displaying the values of all the variables
        Console.WriteLine("Byte: " + smallNumber);
        Console.WriteLine("SByte: " + signedSmallNumber);
        Console.WriteLine("Short: " + shortNumber);
        Console.WriteLine("UShort: " + unsignedShortNumber);
        Console.WriteLine("Int: " + number);
        Console.WriteLine("UInt: " + unsignedNumber);
        Console.WriteLine("Long: " + longNumber);
        Console.WriteLine("ULong: " + unsignedLongNumber);
        Console.WriteLine("Float: " + floatNumber);
        Console.WriteLine("Double: " + doubleNumber);
        Console.WriteLine("Decimal: " + decimalNumber);
        Console.WriteLine("Char: " + singleCharacter);
        Console.WriteLine("Bool: " + isTrue);
        Console.WriteLine("String: " + name);


        /*****************************************************************************/
        // PART 3: Constants and Statics
        // Constants are variables whose values cannot be changed after they are defined.
        // Declare a constant of type 'double' for the value of Pi.
        const double Pi = 3.14159;
        Console.WriteLine("Pi: " + Pi);
        
        // Demonstrating static variable behavior
        IncrementStaticCounter();
        Console.WriteLine("Static Counter after 1st increment: " + staticCounter);

        IncrementStaticCounter();
        Console.WriteLine("Static Counter after 2nd increment: " + staticCounter);


        /*****************************************************************************/
        // PART 4: Input and Output
        Console.Write("Enter your favorite number: ");
        string userInput = Console.ReadLine();
        int favoriteNumber;

        // Try to parse the input to an integer
        if (int.TryParse(userInput, out favoriteNumber))
        {
            Console.WriteLine($"Your favorite number is {favoriteNumber}!");
        }
        else
        {
            Console.WriteLine("Invalid input! Please enter a valid integer.");
        }


        /*****************************************************************************/
        // PART 5: Single-Line and Multi-Line Comments
        // This is a single-line comment. It is used to explain one line of code.

        /*
            This is a multi-line comment.
            It is used to explain multiple lines of code
            or provide detailed information about the program.
        */


        /*****************************************************************************/
        // PART 6: Code Formatting
        // Proper code formatting helps to make the code more readable and maintainable.
        // Ensure you use consistent indentation and spacing throughout your code.
        if (isTrue)
        {
            Console.WriteLine("The program is active.");
        }
        else
        {
            Console.WriteLine("The program is not active.");
        }

        // Wait for the user to press a key before closing the console window.
        // This prevents the console from closing immediately after the program runs.
        Console.WriteLine("Press any key to exit...");
        Console.ReadKey();
    }
    
    // Static method to increment the static variable
    static void IncrementStaticCounter()
    {
        staticCounter++;
    }
}

