/******************************************************************************
Control Structure Example - 
Example program that creates a main function and demonstrates how to use if 
statments, while loops, loops, for loops, and match statements.

*******************************************************************************/
#[allow(dead_code)]
#[allow(unused_variables)]

fn if_statement(){
    let temp = 35;
    
    if temp > 30 {
        println!("It is hot outside!");
    } else if temp < 10 {
        println!("It is cold outside!");
    } else {
        println!("It is okay outside!");
    }
    // We can use if statements in rust just like any other language
    
    
    let day = if temp > 20 {"sunny"} else {"cloudy"};
    println!("{}", day);
    // We can use if statements to set variable values too
    // Very similar to ternary expressions but rust does not support ? : notation
    
    
    println!("{}", 
        if temp > 20 {"hot"} else if temp < 10 {"cold"} else {"OK"});
    // We can use if statements inside of the println macro as well
    
    
    // We can nest if statements just like any other language as well
}



fn while_and_loop(){
    let mut x = 1;
    
    while x < 1000 {
        x *= 2;
        
        if x == 64 {
            continue;
            // We can use continue to jump to next iteration of loops
            // This does NOT complete the current iteration of the loop
        }
        
        println!("x = {}", x);
    }
    // We can use while loops in rust just like any other language
    
    
    let mut y = 1;
    
    loop {
        y *= 2;
        println!("y = {}", y);
        if y == 1 << 10 {
            break;
            // We can use break to stop the running of the infinite loop
        }
    }
    // Rust supports making infinite loops without conditions using loop keyword
}



fn for_loops(){

    for x in 1..=11 {
        if x == 3 { continue; }
        if x == 8 { break; }
        // We can use continue and break here as well
        
        println!("x = {}", x);
    }
    // For loops in rust are a bit different from other languages
    // They use a range operators .. or ..= for inclusive ending
    
    
    for z in (1..=11).step_by(2) {
        
        println!("z = {}", z);
    }
    // We can adjust the step size using the step_by function
    
    
    for (pos, y) in (30..41).enumerate() {
        println!("{}: {}", pos, y);
    }
    // You can also get the position and value in a sequence
    // In this case we are going from 30 to 40 and returning the 
    // enumerated index as well.
}



fn match_statement() {
    let country_code = 44;
    
    let country = match country_code {
        44 => "UK",
        46 => "Sweden",
        7 => "Russia",
        1 ..= 1000 => "unknown",
        _ => "invalid"
    };
    // The match statement will look to match the value given
    // If no value is found, the _ default case at then end is used
    // Rust will force you to cover ever possible case and will stop you if you dont
    // This exhaustive coverage is based on the data type of the result variable
    
    println!("The country with code {} is {}", country_code, country);
}



fn main() {
    if_statement();
    while_and_loop();
    for_loops();
    match_statement();
}