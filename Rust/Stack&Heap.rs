/******************************************************************************
Stack and Heap Example - 
Example program that creates a main function and demonstrates how to use const 
and static keywords in rust.

*******************************************************************************/
#[allow(dead_code)]
#[allow(unused_variables)]
use std::mem;

struct Point
{
    x: f64,
    y: f64
}

fn origin() -> Point
{
    Point{x: 0.0, y: 0.0}
}

fn main() {
    let p1 = origin();
    let p2 = Box::new(origin());

    println!("P1 takes up {} bytes", mem::size_of_val(&p1));
    println!("P2 takes up {} bytes", mem::size_of_val(&p2));

    let p3 = *p2;
    


}