/******************************************************************************
C# Delegates Events and Lambdas Example - 
Example program that creates a basic C# program that demonstrates:
1. Delegates and callback functions
2. Lambda expressions and anonymous methods
3. Events and event handling

*******************************************************************************/
using System;

class Program
{
    static void Main(string[] args)
    {
        // PART 1: Delegates and Callback Functions

        // Creating a delegate instance pointing to a method
        MathOperation addOperation = Add;
        MathOperation multiplyOperation = Multiply;

        // Using the delegate to call the methods
        Console.WriteLine($"Add: {addOperation(5, 3)}");
        Console.WriteLine($"Multiply: {multiplyOperation(5, 3)}");

        // Passing a delegate as a callback function
        PerformOperation(10, 5, addOperation);
        PerformOperation(10, 5, multiplyOperation);


        /*****************************************************************************/
        // PART 2: Lambda Expressions

        // Using a lambda expression with a delegate
        MathOperation subtractOperation = (a, b) => a - b;
        Console.WriteLine($"\nSubtract using Lambda: {subtractOperation(10, 3)}");

        // Using lambda expressions for simple operations
        Func<int, int, int> divideOperation = (a, b) => a / b;
        Console.WriteLine($"Divide using Lambda: {divideOperation(10, 2)}");

        // Lambda expression with multiple lines
        Func<int, int, int> moduloOperation = (a, b) =>
        {
            Console.WriteLine("Calculating modulus...");
            return a % b;
        };
        Console.WriteLine($"Modulo using Lambda: {moduloOperation(10, 3)}");


        /*****************************************************************************/
        // PART 3: Events and Event Handling

        // Creating a publisher and a subscriber
        Publisher publisher = new Publisher();
        Subscriber subscriber = new Subscriber();

        // Subscribing to the event
        publisher.NumberReached += subscriber.OnNumberReached;

        // Triggering the event by counting to a number
        publisher.CountTo(5);

        // Wait for user to exit
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }


    /*****************************************************************************/
    // PART 1: Delegates and Callback Functions

    // Delegate definition
    delegate int MathOperation(int a, int b);

    // Methods to be used with the delegate
    static int Add(int a, int b)
    {
        return a + b;
    }

    static int Multiply(int a, int b)
    {
        return a * b;
    }

    // Method accepting a delegate as a callback
    static void PerformOperation(int a, int b, MathOperation operation)
    {
        int result = operation(a, b);
        Console.WriteLine($"PerformOperation Result: {result}");
    }
}


/*****************************************************************************/
// PART 3: Events and Event Handling

// Publisher class that raises an event
class Publisher
{
    // Declaring an event using EventHandler
    public event EventHandler NumberReached;

    // Method that triggers the event when a certain number is reached
    public void CountTo(int number)
    {
        for (int i = 0; i <= number; i++)
        {
            Console.WriteLine($"Counting: {i}");
            if (i == number)
            {
                OnNumberReached(EventArgs.Empty); // Raise the event
            }
        }
    }

    // Protected method to raise the event
    protected virtual void OnNumberReached(EventArgs e)
    {
        NumberReached?.Invoke(this, e);
    }
}

// Subscriber class that handles the event
class Subscriber
{
    // Event handler method
    public void OnNumberReached(object sender, EventArgs e)
    {
        Console.WriteLine("Event triggered: Number reached!");
    }
}

