/******************************************************************************
Functions and Methods Example - 
Example program that creates a main function and demonstrates how to use 
functions and struct methods in Rust

*******************************************************************************/
#[allow(dead_code)]
#[allow(unused_variables)]

fn print_value(x: i32){
    println!("{}", x);
}
// Can pass values to do things

fn increase (x: &mut i32){
    *x += 1;
}
// Can pass values and update them through reference

fn product(x: i32, y: i32) -> i32
{
    return x * y;
}
// Can return a value back from a function

fn functions()
{
    print_value(33);
    
    let mut z = 1;
    increase(&mut z);
    println!("{}", z);
    
    let a = 3;
    let b = 5;
    let p = product(a, b);
    println!("{}", p);
}



struct Point
{
    x: f64,
    y: f64
}

struct Line
{
    start: Point,
    end: Point
}

impl Line
{
    fn len(&self) -> f64
    {
        let dx = self.start.x - self.end.x;
        let dy = self.start.y - self.end.y;
        return (dx * dx + dy * dy).sqrt();
    }
}
// Using impl we can create methods that work on structs

fn methods()
{
    let p = Point{ x: 3.0, y: 4.0 };
    let p2 = Point{ x: 5.0, y: 10.0 };
    let l = Line{ start: p, end: p2 };
    
    println!("Length = {}", l.len());
    
}


fn main()
{
    functions();
    methods();
}