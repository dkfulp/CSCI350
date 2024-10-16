/******************************************************************************
Constants and Static Example - 
Example program that creates a main function and demonstrates how to use const 
and static keywords in rust.

*******************************************************************************/
#[allow(dead_code)]
#[allow(unused_variables)]

const PI_VALUE:f64 = std::f64::consts::PI; 
// You can create constant values using the const keyword
// const is evaluated and inlined at compile time meaning they have no fixed memory location
// consts cannot be mutableo or be shadowed like you can with let variables either
// let is evaluated at run time and can be mutable if desired
// By placing the value here it is accessible to the entire program


static mut Z:i32 = 123;
// You can create static values using the static keyword
// Similar to const but are not inlined, instead they have a fixed memory location
// The value must be known at compile time (more flexible with arrays though)
// Immutable by default but can be set to mutable


fn main() {
    //let PI_VALUE_1 = 3.14;
    // Cannot do this as this shadows the global constant value (illegal)
    
    
    println!("PI: {}", PI_VALUE);
    // Can use global constant value within any part of the program
    
    
    unsafe {
        println!("Z: {}", Z);
    }
    // To use mutable static variables you need to use an unsafe function or 
    // block as rust tries to prevent this due to the unsafe nature in concurrency

}