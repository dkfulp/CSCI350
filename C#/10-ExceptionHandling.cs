/******************************************************************************
C# Exception Handling Example - 
Example program that creates a basic C# program that demonstrates:
1. Try, catch, and finally blocks
2. Throwing exceptions
3. Custom exceptions

*******************************************************************************/
using System;

class Program
{
    static void Main(string[] args)
    {
        // PART 1: Try-Catch-Finally Blocks

        try
        {
            Console.WriteLine("Enter a number: ");
            int num = int.Parse(Console.ReadLine()); // May throw FormatException
            Console.WriteLine($"You entered: {num}");
        }
        catch (FormatException ex)
        {
            Console.WriteLine($"Error: {ex.Message} - Please enter a valid number.");
        }
        catch (OverflowException ex)
        {
            Console.WriteLine($"Error: {ex.Message} - Number is too large or too small.");
        }
        finally
        {
            Console.WriteLine("This will always execute, whether an exception occurs or not.");
        }


        /*****************************************************************************/
        // PART 2: Throwing Exceptions

        try
        {
            Console.WriteLine("\nChecking a number for validation...");
            ValidateNumber(-5); // This will throw an exception
        }
        catch (ArgumentException ex)
        {
            Console.WriteLine($"Caught Exception: {ex.Message}");
        }


        /*****************************************************************************/
        // PART 3: Custom Exceptions

        try
        {
            Console.WriteLine("\nTrying a custom exception...");
            PerformOperation(0); // This will trigger the custom exception
        }
        catch (InvalidOperationException ex)
        {
            Console.WriteLine($"Custom Exception Caught: {ex.Message}");
        }

        // Wait for user to exit
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }


    /*****************************************************************************/
    // PART 2: Throwing Exceptions

    static void ValidateNumber(int number)
    {
        if (number < 0)
        {
            throw new ArgumentException("Number cannot be negative.");
        }
        Console.WriteLine($"Valid number: {number}");
    }


    /*****************************************************************************/
    // PART 3: Custom Exceptions

    // Method that uses a custom exception
    static void PerformOperation(int divisor)
    {
        if (divisor == 0)
        {
            throw new InvalidOperationException("Cannot divide by zero in this operation.");
        }
        Console.WriteLine($"Operation result: {100 / divisor}");
    }
}

// Custom exception class
class InvalidOperationException : Exception
{
    public InvalidOperationException(string message) : base(message)
    {
    }
}


