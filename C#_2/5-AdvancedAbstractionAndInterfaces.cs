/******************************************************************************
C# Advanced Class Design Concepts Example - 
Example program that creates a basic C# program that demonstrates:
1. Deep Dive into Abstract Classes and Interfaces – comprehensive comparisons, use cases, and practical examples
2. Sealed Classes and Methods – understanding their role in limiting inheritance
3. Nested Classes – usage scenarios and benefits for encapsulation

Abstract Class (Shape):
    Serves as a template for other classes. It has an abstract method CalculateArea() that must be implemented by any derived class.
    Contains a virtual method DescribeShape() that provides a default implementation but can be overridden in derived classes for more specific behavior.

Derived Class (Circle):
    Inherits from Shape and implements CalculateArea(). The area of a circle is calculated using the formula π×r2π×r2.
    Overrides the DescribeShape() method to give a more specific description for circles.

Sealed Class (Square):
    Inherits from Shape and implements CalculateArea() using the formula side2side2.
    Is marked as sealed, which means no other class can inherit from Square.
    Provides an overridden version of DescribeShape() for square-specific descriptions.

Nested Class (Book):
    The Book class is nested within the Library class to encapsulate it as a part of the Library's functionality. This makes sense logically because a Book is inherently part of a Library.
    The Book class is kept private to the Library class context. The Library manages the Book instances, ensuring they are only accessed through Library's methods.

Key Takeaways for Students:
    Abstract Classes: Useful for defining a base class with shared behavior and enforcing implementation of specific methods in derived classes.
    Sealed Classes: Prevent further inheritance. This can be useful for security reasons or when extending a class would break the intended design.
    Nested Classes: Can help logically group related code and improve the organization of larger classes or modules.
*******************************************************************************/
using System;
using System.Collections.Generic;

namespace AdvancedClassDesign
{
    // Abstract class: Acts as a blueprint for other classes and cannot be instantiated directly
    public abstract class Shape
    {
        // Abstract method: Must be overridden in derived classes
        public abstract double CalculateArea();

        // Virtual method: Can be overridden but provides a default implementation
        public virtual void DescribeShape()
        {
            Console.WriteLine("This is a shape.");
        }
    }

    // Derived class from Shape that represents a Circle
    public class Circle : Shape
    {
        // Property for the radius of the circle
        public double Radius { get; set; }

        // Constructor to initialize the circle with a radius
        public Circle(double radius)
        {
            Radius = radius;
        }

        // Implementing the abstract method to calculate the area of the circle
        public override double CalculateArea()
        {
            return Math.PI * Math.Pow(Radius, 2); // Formula: π * r^2
        }

        // Overriding the DescribeShape method to provide a specific message for circles
        public override void DescribeShape()
        {
            Console.WriteLine($"This is a circle with a radius of {Radius}.");
        }
    }

    // Sealed class: Represents a Square and cannot be inherited by other classes
    public sealed class Square : Shape
    {
        // Property for the side length of the square
        public double SideLength { get; set; }

        // Constructor to initialize the square with a side length
        public Square(double sideLength)
        {
            SideLength = sideLength;
        }

        // Implementing the abstract method to calculate the area of the square
        public override double CalculateArea()
        {
            return SideLength * SideLength; // Formula: side^2
        }

        // Overriding the DescribeShape method to provide a specific message for squares
        public override void DescribeShape()
        {
            Console.WriteLine($"This is a square with side length {SideLength}.");
        }
    }

    // Uncommenting the following class will result in a compile-time error, as Square is sealed
    // public class SpecialSquare : Square
    // {
    //     // Error: Cannot derive from 'Square' because it is sealed
    // }

    // Class containing a nested class for demonstrating nested class usage
    public class Library
    {
        // List to store books within the library
        private List<Book> books = new List<Book>();

        // Nested class representing a Book
        public class Book
        {
            public string Title { get; set; }
            public string Author { get; set; }

            // Constructor to initialize the book with title and author
            public Book(string title, string author)
            {
                Title = title;
                Author = author;
            }

            // Method to display book details
            public void DisplayDetails()
            {
                Console.WriteLine($"Title: {Title}, Author: {Author}");
            }
        }

        // Method to add a book to the library
        public void AddBook(string title, string author)
        {
            Book newBook = new Book(title, author); // Creating an instance of the nested Book class
            books.Add(newBook);
            Console.WriteLine($"Book '{title}' by {author} added to the library.");
        }

        // Method to list all books in the library
        public void ListBooks()
        {
            Console.WriteLine("\nListing all books in the library:");
            foreach (Book book in books)
            {
                book.DisplayDetails();
            }
        }

        // Method to find a book by title
        public Book FindBookByTitle(string title)
        {
            return books.Find(book => book.Title.Equals(title, StringComparison.OrdinalIgnoreCase));
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Demonstrating the use of abstract classes and method overriding
            Shape myCircle = new Circle(5); // Creating an instance of Circle (derived from Shape)
            myCircle.DescribeShape(); // Calls the overridden DescribeShape method in Circle
            Console.WriteLine($"Area of the circle: {myCircle.CalculateArea()}"); // Outputs the area

            Console.WriteLine();

            // Demonstrating the use of a sealed class
            Shape mySquare = new Square(4); // Creating an instance of Square (derived from Shape)
            mySquare.DescribeShape(); // Calls the overridden DescribeShape method in Square
            Console.WriteLine($"Area of the square: {mySquare.CalculateArea()}"); // Outputs the area

            Console.WriteLine();

            // Demonstrating nested class usage
            // Creating an instance of the Library class
            Library myLibrary = new Library();

            // Adding books to the library
            myLibrary.AddBook("The Great Gatsby", "F. Scott Fitzgerald");
            myLibrary.AddBook("To Kill a Mockingbird", "Harper Lee");
            myLibrary.AddBook("1984", "George Orwell");

            // Listing all books in the library
            myLibrary.ListBooks();

            Console.WriteLine();

            // Finding a book by title
            string searchTitle = "1984";
            Library.Book foundBook = myLibrary.FindBookByTitle(searchTitle);
            if (foundBook != null)
            {
                Console.WriteLine($"\nBook found: {foundBook.Title} by {foundBook.Author}");
            }
            else
            {
                Console.WriteLine($"\nBook '{searchTitle}' not found.");
            }
        }
    }
}

