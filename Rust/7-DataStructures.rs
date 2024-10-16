/******************************************************************************
Data Structures Example - 
Example program that creates a main function and demonstrates how to use if 
statments, while loops, loops, for loops, and match statements.

*******************************************************************************/
#[allow(dead_code)]
#[allow(unused_variables)]

// STRUCTS -------------------------
struct Point
{
    x: f64,
    y: f64
}
// We can create structures in Rust that hold multiple pieces of data

struct Line
{
    start: Point,
    end: Point
}
// We can create structures that hold other structures

fn structures(){
    let p = Point { x: 3.0, y: 4.0 };
    println!("Point p is at ({}, {})", p.x, p.y);
    // We create an access a struct using common notation
    
    let p2 = Point {x: 5.0, y: 10.0};
    let l = Line {start: p, end: p2};
    println!("Line goes from ({}, {}) to ({}, {})", l.start.x, l.start.y, l.end.x, l.end.y);
    // We can assign structs to other structs as well
}



// ENUMERATIONS -------------------------
enum Color {
    Red, 
    Green,
    Blue,
    RgbColor(u8,u8,u8), // Uses tuple style (no hints)
}
// We can create Enumeration types as well that have a fixed number
// of possible values


fn enums() {
    let c: Color = Color::Red;
    // We create a enum instance by calling the enum and one of its values
    
    let c2: Color = Color::RgbColor(0, 0, 0);
    // When creating an enum instance we need to make sure we provide all parts
    
    match c {
        Color::Red => println!("It's Red!"),
        Color::Green => println!("It's Green!"),
        Color::Blue => println!("It's Blue!"),
        Color::RgbColor(0,0,0) => println!("It's Black!"),
        Color::RgbColor(r,g,b) => println!("It's rbg({},{},{})",r,g,b)
    }
    // We do not need to provide a default case as the match statement is 
    // smart enough to see that we have covered every possible case
}



// UNIONS -------------------------
union IntOrFloat{
    i: i32,
    f: f32
}
// Will either be an integer or a floating point value

fn unions(){
    let mut iof = IntOrFloat{ i: 123 };
    iof.i = 234;
    // We can set and update the value like normally by choosing i or f
    
    let value = unsafe { iof.i };
    println!("Value of iof is {}", value);
    // When we want to access it we need to use unsafe block as we 
    // don't know if it is truly an integer or float value
    
    unsafe {
        match iof {
            IntOrFloat {i: 42} => {
                println!("Meaning of life value!");
            },
            IntOrFloat { f } => {
                println!("value = {}", f);
            }
        }
        // When using match statment we can check for a specific 
        // i or f value. But we don't need to check for both i and f 
        // default values as no one knows if it is a int or float anyways.
        // So rust just treats it as an f in this case.
    }
}



// Option<T> -------------------------
fn option_T(){
    let x = 3.0;
    let y = 2.0;
    
    let result = if y != 0.0 { Some(x/y)} else { None };
    
    match result {
        Some (z) => println!("{}/{}={}", x,y,z ),
        None => println!("Cannot divide by zero!"),
    }
    // We can use Some in a match statement
    
    if let Some(z) = result {
        println!("result = {}", z );
    }
    // Here we see if we can assign the value of result to Some(z)
    // this is impossible if result is None
    // This uses the let keyword that allows us to make this check first
    
    
}
// If a value may or may not exist, the Option enum exists that can be either
// Some or None. This allows us to check to see if a value exists using a match



// Arrays -------------------------
fn arrays(){
    let mut a:[i32; 5] = [1,2,3,4,5];
    //let mut a = [1,2,3,4,5];
    println!("a has {} elements, first is {}", a.len(), a[0]);
    
    a[0] = 321;
    println!("a[0] is {}", a[0]);
    
    println!("{:?}", a);
    // Use this notation to display the entire array
    
    let a2 = &a[1..3];
    println!("{:?}", a2);
    // We can grab slices of arrays as well using range notation

    if a != [1,2,3,4,5] {
        println!("Does not match!");
    }
    // You can compare arrays using standard notation but the arrays must 
    // be the exact same length. If not you will not be able to compile.
    
    let mut b = [1; 10];
    // You can use shorthand notation to specify multiple elements
    for i in 0..b.len() {
        println!("{}", b[i]);
    }
    
    let mtx:[[f32; 3]; 2] = 
    [
        [1.0, 0.0, 0.0],
        [0.0, 2.0, 0.0]
    ];
    // To create two dimensional arrays you specify using nested notation
    println!("{:?}", mtx);
    // Use this notation to display the entire array
    
    for i in 0..mtx.len(){
        for j in 0..mtx[i].len(){
            if i == j {
                println!("{:?}", mtx[i][j]);
            }
        }
    }
    // To display multi-dimensional arrays, use multiple for loops.
    
    
}
// Used to hold multiple values of same time



// Tuples -------------------------
fn sum_and_product(x:i32, y:i32) -> (i32, i32){
    return (x+y, x*y);
}
fn tuples(){
    let x = 3;
    let y = 4;
    let sp = sum_and_product(x, y);
    // We can create a tuple using parentheses
    
    println!("sp = {:?}", sp);
    println!("{0} + {1} = {2}, {0} * {1} = {3}", x, y, sp.0, sp.1);
    // To access tuple elements use . notation
    
    let (a, b) = sp;
    println!("a = {}, b = {}", a, b);
    // Can use destructuring to seperate values

}

fn main() {
    structures();
    enums();
    unions();
    option_T();
    arrays();
    tuples();
}