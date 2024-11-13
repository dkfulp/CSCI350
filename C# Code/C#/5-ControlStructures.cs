/******************************************************************************
C# Control Structures Example - 
Example program that creates a basic C# program that demonstrates:
1. Conditional statements (if, else, switch-case)
2. Loops (for, while, do-while)
3. Iterating through collections (foreach)
4. Break, continue

*******************************************************************************/
using System;
using System.Collections.Generic;

class Program
{
    static void Main(string[] args)
    {
        // PART 1: Conditional Statements

        // 1. if-else statement
        int num = 10;
        if (num > 0)
        {
            Console.WriteLine("Positive number");
        }
        else if (num < 0)
        {
            Console.WriteLine("Negative number");
        }
        else
        {
            Console.WriteLine("Zero");
        }

        // 2. switch-case statement
        int day = 3;
        switch (day)
        {
            case 1:
                Console.WriteLine("Monday");
                break;
            case 2:
                Console.WriteLine("Tuesday");
                break;
            case 3:
                Console.WriteLine("Wednesday");
                break;
            default:
                Console.WriteLine("Another day");
                break;
        }

        // 3. Nested if statement
        bool isActive = true;
        int age = 25;
        if (isActive)
        {
            if (age > 18)
            {
                Console.WriteLine("Active adult");
            }
            else
            {
                Console.WriteLine("Active minor");
            }
        }
        else
        {
            Console.WriteLine("Inactive");
        }


        /*****************************************************************************/
        // PART 2: Loops

        // 1. for loop
        Console.WriteLine("\nFor Loop:");
        for (int i = 0; i < 5; i++)
        {
            Console.WriteLine($"Iteration {i + 1}");
        }

        // 2. while loop
        Console.WriteLine("\nWhile Loop:");
        int j = 0;
        while (j < 5)
        {
            Console.WriteLine($"Iteration {j + 1}");
            j++;
        }

        // 3. do-while loop
        Console.WriteLine("\nDo-While Loop:");
        int k = 0;
        do
        {
            Console.WriteLine($"Iteration {k + 1}");
            k++;
        } while (k < 5);


        /*****************************************************************************/
        // PART 3: Iterating Through Collections

        // 1. foreach loop for arrays
        Console.WriteLine("\nForeach Loop for Array:");
        int[] numbers = { 10, 20, 30, 40, 50 };
        foreach (int number in numbers)
        {
            Console.WriteLine(number);
        }

        // 2. foreach loop for lists
        Console.WriteLine("\nForeach Loop for List:");
        List<string> names = new List<string> { "Alice", "Bob", "Charlie" };
        foreach (string name in names)
        {
            Console.WriteLine(name);
        }


        /*****************************************************************************/
        // PART 4: Break and Continue Statements

        // 1. Using break in a loop
        Console.WriteLine("\nBreak in For Loop:");
        for (int i = 0; i < 10; i++)
        {
            if (i == 5)
            {
                Console.WriteLine("Break at 5");
                break;
            }
            Console.WriteLine(i);
        }

        // 2. Using continue in a loop
        Console.WriteLine("\nContinue in For Loop:");
        for (int i = 0; i < 10; i++)
        {
            if (i % 2 == 0)
            {
                continue; // Skip even numbers
            }
            Console.WriteLine(i);
        }

        // 3. Using break in a while loop
        Console.WriteLine("\nBreak in While Loop:");
        int count = 0;
        while (true)
        {
            if (count == 3)
            {
                Console.WriteLine("Breaking loop at 3");
                break;
            }
            Console.WriteLine($"Count: {count}");
            count++;
        }


        /*****************************************************************************/
        // PART 5: Wait for user to exit
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }
}

