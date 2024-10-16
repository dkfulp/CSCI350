/******************************************************************************
Scope and Shadowing Example - 
Example program that creates a main function and demonstrates rust scoping rules
and shadowing


*******************************************************************************/
#[allow(dead_code)]
#[allow(unused_variables)]

fn scope_and_shadowing(){
    let a = 123;
    
    {
        let b = 456;
        println!("b = {}", b);
        // b is only available within this scope block
        // The { } braces are what denote scope blocks in rust
        
        
        let a = 777;
        println!("a = {}", a);
        // a in the inner scope is set but has no effect on the outer a
    }
    
    println!("a = {}", a);
    // We can print a externally but we cannot print b as the scope has left
}

fn main() {
    scope_and_shadowing();
    
    println!("a = {}", a);
    // Cannot use a in this scope block as it is not declared here

}