/******************************************************************************
C# Data Types and Variables Example - 
Example program that creates a basic C# program that demonstrates:
1. Primitive data types
2. Type conversion and casting
3. Nullable types

*******************************************************************************/
using System;

class Program
{
    static void Main(string[] args)
    {
        // PART 1: Primitive Data Types
        // These are examples of different primitive data types in C#

        // Integer types
        byte smallByte = 255; // 8-bit unsigned integer
        sbyte signedByte = -128; // 8-bit signed integer
        short shortNum = -32768; // 16-bit signed integer
        ushort unsignedShort = 65535; // 16-bit unsigned integer
        int integerNum = 2147483647; // 32-bit signed integer
        uint unsignedInt = 4294967295; // 32-bit unsigned integer
        long longNum = -9223372036854775808; // 64-bit signed integer
        ulong unsignedLong = 18446744073709551615; // 64-bit unsigned integer

        // Floating-point types
        float floatNum = 3.14f; // Single-precision floating-point
        double doubleNum = 19.99; // Double-precision floating-point
        decimal decimalNum = 79228162514264337593543950335m; // High-precision decimal

        // Character and Boolean types
        char letter = 'A'; // Single 16-bit Unicode character
        bool isTrue = true; // Boolean value (true/false)

        // Displaying all primitive data types
        Console.WriteLine("Byte: " + smallByte);
        Console.WriteLine("SByte: " + signedByte);
        Console.WriteLine("Short: " + shortNum);
        Console.WriteLine("UShort: " + unsignedShort);
        Console.WriteLine("Int: " + integerNum);
        Console.WriteLine("UInt: " + unsignedInt);
        Console.WriteLine("Long: " + longNum);
        Console.WriteLine("ULong: " + unsignedLong);
        Console.WriteLine("Float: " + floatNum);
        Console.WriteLine("Double: " + doubleNum);
        Console.WriteLine("Decimal: " + decimalNum);
        Console.WriteLine("Char: " + letter);
        Console.WriteLine("Bool: " + isTrue);


        /*****************************************************************************/
        // PART 2: Type Conversion and Casting

        // Implicit conversion: No data loss, done automatically
        int implicitInt = 10;
        double implicitDouble = implicitInt; // Converts int to double automatically
        Console.WriteLine("Implicit Conversion: " + implicitDouble);

        // Explicit conversion: Potential data loss, must be done manually
        double explicitDouble = 10.99;
        int explicitInt = (int)explicitDouble; // Converts double to int, truncates decimal part
        Console.WriteLine("Explicit Conversion: " + explicitInt);

        // Conversion using helper methods
        string numberString = "123";
        int parsedInt = int.Parse(numberString); // Parses string to int
        Console.WriteLine("Parsed Int: " + parsedInt);

        // Using TryParse to handle invalid conversion safely
        string invalidString = "abc";
        bool success = int.TryParse(invalidString, out int tryParsedInt);
        if (success)
        {
            Console.WriteLine("TryParsed Int: " + tryParsedInt);
        }
        else
        {
            Console.WriteLine("TryParse failed. Input is not a valid integer.");
        }


        /*****************************************************************************/
        // PART 3: Nullable Types
        // Nullable types allow value types to be assigned 'null'

        int? nullableInt = null; // Declaration of a nullable integer
        if (nullableInt.HasValue)
        {
            Console.WriteLine("Nullable Int has value: " + nullableInt.Value);
        }
        else
        {
            Console.WriteLine("Nullable Int is null.");
        }

        // Assign a value to nullableInt and check again
        nullableInt = 42;
        if (nullableInt.HasValue)
        {
            Console.WriteLine("Nullable Int now has value: " + nullableInt.Value);
        }

        // Using the null-coalescing operator (??)
        int defaultValue = nullableInt ?? -1; // If nullableInt is null, use -1
        Console.WriteLine("Using null-coalescing operator: " + defaultValue);


        /*****************************************************************************/
        // PART 4: Wait for user to exit
        Console.WriteLine("Press any key to exit...");
        Console.ReadKey();
    }
}
