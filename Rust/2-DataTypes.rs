/******************************************************************************
Data Type Example - 
Example program that demonstrates the different data types in Rust


*******************************************************************************/
#[allow(dead_code)]
#[allow(unused_variables)]

use std::mem; // For line 31

fn main() {
    let a: u8 = 123; 
    // u = unsigned, 8 bits, 0 - 255
    // By default all variables in rust are immutable
    println!("a = {}", a); 
    // ! means macro and {} let us print format
    
    
    let mut b: i8 = 0;
    // i = signed, 8 bits, -128 - 127
    // mut keyword makes variable immutable
    println!("b = {} before", b);
    b = 42;
    println!("b = {} after", b);
    // mut keyword allows us to change variables value


    let mut c = 123456789;
    // Rust includes type inference to determine best type
    // In this case, it will use i32
    println!("c = {}, takes up {} bytes", c, mem::size_of_val(&c));
    // Using this package, we can see it does take up 4 bytes of space
    c = -1;
    println!("c = {}, takes up {} bytes", c, mem::size_of_val(&c));
    // Changes the value to -1 but still takes up 4 bytes initially allocated
    
    
    // Overall, we have u8, u16, u32, u64, i8, i16, ....
    
    
    let z: isize = 123;
    let size_z = mem::size_of_val(&z);
    println!("z = {}, takes up {} bytes, {} bit OS", z, size_z, size_z * 8);
    // usize and isize are unsigned and signed values
    // Their sizes are dependent on the architecture of your OS (32 vs 64) bit
    
    
    let d: char = 'x';
    println!("d = {}, takes up {} bytes", d, mem::size_of_val(&d));
    // A single character uses single quotes
    // Strings use double quotes
    
    
    let e: f32 = 2.5;
    println!("e = {}, takes up {} bytes", e, mem::size_of_val(&e));
    let f: f64 = 2.5;
    println!("f = {}, takes up {} bytes", f, mem::size_of_val(&f));
    // The two floating-point types are f32 and f64
    // f64 is the default value if none is specified
    
    
    let g: bool = false // true
    println!("e = {}, takes up {} bytes", e, mem::size_of_val(&e));
    // Two options but will always take up a single byte (not bit)
}