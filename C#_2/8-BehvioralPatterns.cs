/******************************************************************************
C# Behavioral Patterns Example - 
Example program that creates a basic C# program that demonstrates:
1. Observer Pattern
2. Strategy Pattern
3. Command Pattern

Observer Pattern:
    The TaskManager class acts as the subject that notifies observers when a task is completed.
    TaskObserver objects represent users who need to be notified about task completions.

Strategy Pattern:
    The ITaskExecutionStrategy interface defines different task execution strategies.
    QuickExecutionStrategy and ThoroughExecutionStrategy are two concrete implementations of the strategy.
    The TaskExecutor class allows switching strategies at runtime to change the way tasks are executed.

Command Pattern:
    The ICommand interface represents a command with an Execute method.
    CreateTaskCommand is a concrete command that creates and executes a task.
    The TaskInvoker class is used to trigger the execution of the command, following the command pattern's invoker role.

Key Points for Students:
    Observer Pattern: Useful for implementing the publish-subscribe model, where multiple objects need to be informed about changes.
    Strategy Pattern: Enables the selection of different algorithms or methods at runtime, promoting flexibility and cleaner code.
    Command Pattern: Encapsulates a request as an object, allowing for parameterization, queuing, and logging of requests.
*******************************************************************************/
using System;
using System.Collections.Generic;

namespace BehavioralPatterns
{
    // Observer pattern: Observer interface
    public interface IObserver
    {
        void Update(string taskName);
    }

    // Concrete observer
    public class TaskObserver : IObserver
    {
        private string _name;

        public TaskObserver(string name)
        {
            _name = name;
        }

        public void Update(string taskName)
        {
            Console.WriteLine($"{_name} has been notified that the task '{taskName}' has been completed.");
        }
    }

    // Subject interface for the Observer pattern
    public interface ISubject
    {
        void Attach(IObserver observer);
        void Detach(IObserver observer);
        void Notify(string taskName);
    }

    // Concrete subject for task notifications
    public class TaskManager : ISubject
    {
        private List<IObserver> _observers = new List<IObserver>();

        public void Attach(IObserver observer)
        {
            _observers.Add(observer);
        }

        public void Detach(IObserver observer)
        {
            _observers.Remove(observer);
        }

        public void Notify(string taskName)
        {
            foreach (var observer in _observers)
            {
                observer.Update(taskName);
            }
        }
    }

    // Strategy pattern: Strategy interface
    public interface ITaskExecutionStrategy
    {
        void ExecuteTask(string taskName);
    }

    // Concrete strategy for quick execution
    public class QuickExecutionStrategy : ITaskExecutionStrategy
    {
        public void ExecuteTask(string taskName)
        {
            Console.WriteLine($"Task '{taskName}' executed quickly.");
        }
    }

    // Concrete strategy for thorough execution
    public class ThoroughExecutionStrategy : ITaskExecutionStrategy
    {
        public void ExecuteTask(string taskName)
        {
            Console.WriteLine($"Task '{taskName}' executed thoroughly with detailed checks.");
        }
    }

    // Context class for Strategy pattern
    public class TaskExecutor
    {
        private ITaskExecutionStrategy _strategy;

        public TaskExecutor(ITaskExecutionStrategy strategy)
        {
            _strategy = strategy;
        }

        public void SetStrategy(ITaskExecutionStrategy strategy)
        {
            _strategy = strategy;
        }

        public void ExecuteTask(string taskName)
        {
            _strategy.ExecuteTask(taskName);
        }
    }

    // Command pattern: Command interface
    public interface ICommand
    {
        void Execute();
    }

    // Concrete command to create a task
    public class CreateTaskCommand : ICommand
    {
        private string _taskName;
        private TaskManager _taskManager;
        private TaskExecutor _taskExecutor;

        public CreateTaskCommand(string taskName, TaskManager taskManager, TaskExecutor taskExecutor)
        {
            _taskName = taskName;
            _taskManager = taskManager;
            _taskExecutor = taskExecutor;
        }

        public void Execute()
        {
            Console.WriteLine($"Creating task '{_taskName}'...");
            _taskExecutor.ExecuteTask(_taskName);
            _taskManager.Notify(_taskName);
        }
    }

    // Invoker for the Command pattern
    public class TaskInvoker
    {
        private ICommand _command;

        public void SetCommand(ICommand command)
        {
            _command = command;
        }

        public void ExecuteCommand()
        {
            _command?.Execute();
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Setting up the Observer pattern
            TaskManager taskManager = new TaskManager();
            TaskObserver observer1 = new TaskObserver("User1");
            TaskObserver observer2 = new TaskObserver("User2");

            taskManager.Attach(observer1);
            taskManager.Attach(observer2);

            // Setting up the Strategy pattern
            TaskExecutor taskExecutor = new TaskExecutor(new QuickExecutionStrategy());

            // Using the Command pattern to create and execute tasks
            CreateTaskCommand createTaskCommand1 = new CreateTaskCommand("Clean Database", taskManager, taskExecutor);

            // Invoker setup
            TaskInvoker invoker = new TaskInvoker();
            invoker.SetCommand(createTaskCommand1);
            invoker.ExecuteCommand();

            Console.WriteLine();

            // Change strategy and execute another task
            taskExecutor.SetStrategy(new ThoroughExecutionStrategy());
            CreateTaskCommand createTaskCommand2 = new CreateTaskCommand("Run Security Audit", taskManager, taskExecutor);
            invoker.SetCommand(createTaskCommand2);
            invoker.ExecuteCommand();
        }
    }
}
