/******************************************************************************
Operators Example - 
Example program that creates a main function and demonstrates different rust 
operators that are supported


*******************************************************************************/
#[allow(dead_code)]
#[allow(unused_variables)]


fn main() {
    // ARITHMETIC OPERATORS  -------------------------
    let mut a = 2 + 3 * 4; 
    println!("a = {}", a);
    // Standard arithmetic is supported in Rust
    // + - * / % Operators
    // Follows standard precedence and associativity
    
    
    a += 1; 
    println!("a = {}", a);
    // Rust does not support ++ or -- notation
    // Rust does support shorthand notation += or -=
    
    
    let a_cubed = i32::pow(a, 3);
    println!("a cubed = {}", a_cubed);
    // Rust does not have a power operator
    // Use pow function
    
    
    let b = 2.5;
    let b_cubed = f64::powi(b, 3);
    let b_to_pi = f64::powf(b, std::f64::consts::PI);
    println!("b cubed = {}", b_cubed);
    println!("b^pi = {}", b_to_pi);
    // powi and powf are power functions for floating point values
    // powi is for when the exponent is an integer (faster)
    // powf is for when the exponent is a float value (flexible but slower)
    
    
    let a_to_pi = f64::powf((a as f64), std::f64::consts::PI);
    println!("a^pi = {}", a_to_pi);
    // To raise integer to float value, must convert to float first
    
    
    // BITWISE OPERATORS -------------------------
    let c = 1 | 2;
    println!("1 | 2 = {}", c);
    // | OR, & AND, ^ XOR, ! NOR
    // 01 OR 10 = 11 === 3
    
    
    let two_to_ten = 1 << 10;
    println!("2^10 = {}", two_to_ten);
    // << and >> shift operators exist in Rust
    
    
    // LOGICAL OPERATORS -------------------------
    let pi_less_4 = std::f64::consts::PI < 4.0;
    println!("p < 4 = {}", pi_less_4);
    // < > <= >= == operators supported

}