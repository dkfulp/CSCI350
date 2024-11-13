/******************************************************************************
C# Operators Example - 
Example program that creates a basic C# program that demonstrates:
1. Arithmetic, relational, logical, and assignment operators
2. Operator precedence
3. Operator overloading

*******************************************************************************/
using System;

class Program
{
    static void Main(string[] args)
    {
        // PART 1: Arithmetic Operators
        int a = 10;
        int b = 3;

        // Addition
        int sum = a + b;
        Console.WriteLine($"Addition: {a} + {b} = {sum}");

        // Subtraction
        int difference = a - b;
        Console.WriteLine($"Subtraction: {a} - {b} = {difference}");

        // Multiplication
        int product = a * b;
        Console.WriteLine($"Multiplication: {a} * {b} = {product}");

        // Division
        int quotient = a / b; // Integer division
        Console.WriteLine($"Division: {a} / {b} = {quotient}");

        // Modulus (remainder)
        int remainder = a % b;
        Console.WriteLine($"Modulus: {a} % {b} = {remainder}");


        /*****************************************************************************/
        // PART 2: Relational Operators
        Console.WriteLine("\nRelational Operators:");

        // Greater than
        Console.WriteLine($"{a} > {b}: {a > b}");

        // Less than
        Console.WriteLine($"{a} < {b}: {a < b}");

        // Greater than or equal to
        Console.WriteLine($"{a} >= {b}: {a >= b}");

        // Less than or equal to
        Console.WriteLine($"{a} <= {b}: {a <= b}");

        // Equal to
        Console.WriteLine($"{a} == {b}: {a == b}");

        // Not equal to
        Console.WriteLine($"{a} != {b}: {a != b}");


        /*****************************************************************************/
        // PART 3: Logical Operators
        bool x = true;
        bool y = false;

        Console.WriteLine("\nLogical Operators:");

        // Logical AND
        Console.WriteLine($"{x} && {y}: {x && y}");

        // Logical OR
        Console.WriteLine($"{x} || {y}: {x || y}");

        // Logical NOT
        Console.WriteLine($"!{x}: {!x}");


        /*****************************************************************************/
        // PART 4: Assignment Operators
        Console.WriteLine("\nAssignment Operators:");

        int c = 5;
        Console.WriteLine($"Initial value of c: {c}");

        // Addition assignment
        c += 2;
        Console.WriteLine($"c += 2: {c}");

        // Subtraction assignment
        c -= 1;
        Console.WriteLine($"c -= 1: {c}");

        // Multiplication assignment
        c *= 3;
        Console.WriteLine($"c *= 3: {c}");

        // Division assignment
        c /= 2;
        Console.WriteLine($"c /= 2: {c}");

        // Modulus assignment
        c %= 2;
        Console.WriteLine($"c %= 2: {c}");


        /*****************************************************************************/
        // PART 5: Operator Precedence
        Console.WriteLine("\nOperator Precedence:");

        // Operators are evaluated based on precedence rules
        int result = a + b * 2; // Multiplication is performed first
        Console.WriteLine($"a + b * 2 = {result}");

        // Parentheses can change precedence
        result = (a + b) * 2;
        Console.WriteLine($"(a + b) * 2 = {result}");


        /*****************************************************************************/
        // PART 6: Operator Overloading (for a custom struct)
        var p1 = new Point(3, 4);
        var p2 = new Point(1, 2);

        // Using overloaded + operator
        var p3 = p1 + p2;
        Console.WriteLine($"\nOperator Overloading for +:");
        Console.WriteLine($"Point1: ({p1.X}, {p1.Y}), Point2: ({p2.X}, {p2.Y}), Result: ({p3.X}, {p3.Y})");

        // Using overloaded == and != operators
        Console.WriteLine($"\nOperator Overloading for == and !=:");
        Console.WriteLine($"Point1 == Point2: {p1 == p2}");
        Console.WriteLine($"Point1 != Point2: {p1 != p2}");


        /*****************************************************************************/
        // PART 7: Wait for user to exit
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }
}

// Struct for operator overloading example
struct Point
{
    public int X { get; set; }
    public int Y { get; set; }

    public Point(int x, int y)
    {
        X = x;
        Y = y;
    }

    // Overloading the + operator
    public static Point operator +(Point p1, Point p2)
    {
        return new Point(p1.X + p2.X, p1.Y + p2.Y);
    }

    // Overloading the == operator
    public static bool operator ==(Point p1, Point p2)
    {
        return p1.X == p2.X && p1.Y == p2.Y;
    }

    // Overloading the != operator
    public static bool operator !=(Point p1, Point p2)
    {
        return !(p1 == p2);
    }

    // Overriding Equals and GetHashCode (required for == and != overloading)
    public override bool Equals(object obj)
    {
        if (!(obj is Point))
            return false;

        Point p = (Point)obj;
        return X == p.X && Y == p.Y;
    }

    public override int GetHashCode()
    {
        return X.GetHashCode() ^ Y.GetHashCode();
    }
}
