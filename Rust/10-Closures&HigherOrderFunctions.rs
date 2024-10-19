/******************************************************************************
Closures and Higher Order Functions Example - 
Example program that creates a main function and demonstrates how to use 
closures and higher order functions in Rust

*******************************************************************************/
#[allow(dead_code)]
#[allow(unused_variables)]

fn closures()
{
    let add = |a: i32, b: i32| -> i32 {
        a + b
    };
    // We can create a basic closure like this
    let result = add(5, 10);
    println!("The sum is: {}", result); // Output: The sum is: 15
    
    
    let num = 10;
    // Closure captures `num` by borrowing
    let print_num = || println!("Number: {}", num);
    // Call the closure
    print_num(); // Output: Number: 10
    

    let mut count = 0;
    // Closure captures `count` by mutable borrowing
    let mut increment = || {
        count += 1;
        println!("Count: {}", count);
    };
    increment(); // Output: Count: 1
    increment(); // Output: Count: 2


    let text = String::from("Hello, Rust!");
    // Closure takes ownership of `text`
    let print_text = move || {
        println!("{}", text);
    };

    print_text(); // Output: Hello, Rust!
    //println!("{}", text);
    // `text` is no longer accessible here
}


// A function that takes a closure as a parameter
fn apply<F>(f: F) 
where
    F: Fn(), // F is a closure that takes no arguments and returns nothing
{
    f();
}


// A function that takes a closure and an integer, applies the closure to the integer
fn operate_on_number<F>(num: i32, operation: F) -> i32
where
    F: Fn(i32) -> i32,
{
    return operation(num);
}


// A function that returns a closure
fn make_adder(addend: i32) -> impl Fn(i32) -> i32 {
    move |x| x + addend
}


fn higher_order()
{
    let say_hello = || println!("Hello, Rust!");
    // Pass the closure to the higher-order function
    apply(say_hello); // Output: Hello, Rust!
    
    
    let square = |x: i32| x * x;
    // Pass the closure and a number to the HOF
    let result = operate_on_number(5, square);
    println!("Square of 5 is: {}", result); // Output: Square of 5 is: 25
    
    
    let add_five = make_adder(5);
    println!("5 + 10 = {}", add_five(10)); // Output: 5 + 10 = 15
    println!("5 + 20 = {}", add_five(20)); // Output: 5 + 20 = 25
}

fn main()
{
    closures();
    higher_order();
}