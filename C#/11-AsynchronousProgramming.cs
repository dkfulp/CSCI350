/******************************************************************************
C# Asynchronous Programming Example - 
Example program that creates a basic C# program that demonstrates:
1. Introduction to async and await
2. Task-based asynchronous programming
3. Handling asynchronous exceptions
4. Cancellation Tokens

*******************************************************************************/
using System;
using System.Threading;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        // PART 1: Introduction to async and await

        Console.WriteLine("Starting async task...");
        await AsyncTaskExample(); // Calls an async method
        Console.WriteLine("Async task completed.");


        /*****************************************************************************/
        // PART 2: Task-based Asynchronous Programming

        Console.WriteLine("\nStarting two tasks concurrently...");
        Task<int> task1 = TaskBasedAsyncExample(5);
        Task<int> task2 = TaskBasedAsyncExample(10);

        int result1 = await task1;
        int result2 = await task2;
        Console.WriteLine($"Task 1 Result: {result1}, Task 2 Result: {result2}");


        /*****************************************************************************/
        // PART 3: Handling Asynchronous Exceptions

        try
        {
            Console.WriteLine("\nAttempting an asynchronous operation with potential error...");
            await FaultyAsyncTask();
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Caught Exception: {ex.Message}");
        }


        /*****************************************************************************/
        // PART 4: Cancellation Tokens

        CancellationTokenSource cts = new CancellationTokenSource();
        CancellationToken token = cts.Token;

        Console.WriteLine("\nStarting cancellable task. Press any key to cancel...");
        Task cancelableTask = CancellableAsyncTask(token);

        // Cancel the task when a key is pressed
        Console.ReadKey();
        cts.Cancel();

        try
        {
            await cancelableTask;
        }
        catch (OperationCanceledException)
        {
            Console.WriteLine("Task was canceled.");
        }

        // Wait for user to exit
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }


    /*****************************************************************************/
    // PART 1: Introduction to async and await

    // Asynchronous method with async and await
    static async Task AsyncTaskExample()
    {
        Console.WriteLine("Async task is running...");
        await Task.Delay(2000); // Simulates a delay of 2 seconds
        Console.WriteLine("Async task is done.");
    }


    /*****************************************************************************/
    // PART 2: Task-based Asynchronous Programming

    // Task-based asynchronous method
    static async Task<int> TaskBasedAsyncExample(int number)
    {
        await Task.Delay(1000); // Simulates work
        return number * 2;
    }


    /*****************************************************************************/
    // PART 3: Handling Asynchronous Exceptions

    // Asynchronous method that throws an exception
    static async Task FaultyAsyncTask()
    {
        await Task.Delay(500); // Simulates work
        throw new InvalidOperationException("Something went wrong in the async operation.");
    }


    /*****************************************************************************/
    // PART 4: Cancellation Tokens

    // Asynchronous method with cancellation support
    static async Task CancellableAsyncTask(CancellationToken token)
    {
        int counter = 0;
        while (!token.IsCancellationRequested)
        {
            Console.WriteLine($"Cancellable task running... {counter}");
            counter++;
            await Task.Delay(500); // Simulates work
        }
        token.ThrowIfCancellationRequested(); // Throws an exception if cancellation is requested
    }
}

// Custom exception for demonstration purposes
class InvalidOperationException : Exception
{
    public InvalidOperationException(string message) : base(message)
    {
    }
}
