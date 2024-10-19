/******************************************************************************
Strings and Formatting Example - 
Example program that creates a main function and demonstrates how to use strings.

*******************************************************************************/
#[allow(dead_code)]
#[allow(unused_variables)]


fn strings()
{
   let s = "Hello There!"; // String slice
   // Acts as a vector of characters
   
   let s:&'static str = "Hello World";
   // This is a static string that cannot change in the program

   for c in s.chars().rev()
   {
    println!("{}", c);
   }

   if let Some(first_char) = s.chars().nth(0)
   {
    println!("{}", first_char);
   }

   let mut letters = String::new();
   let mut a = 'a' as u8;
   while a <= ('z' as u8)
   {
    letters.push(a as char);
    letters.push_str(",");
    a += 1;
   }

   println!("{}", letters);
   
   // &str <> String 
   let u:&str = &letters;
   // Converts String to str
   
   let z = letters + "abc"; 
   
   let mut abc = "Hello World".to_string();
   abc.remove(0);
   abc.push_str("!!!");
   println!("{}", abc.replace("ello", "Goodbye"));
}

fn format()
{
    let name = "Jim";
    let greeting = format!("Hi, I am {}, nice to meet you!", name);
    println!("{}", greeting);
    // Can use format macro to format strings
    
    let run = "run";
    let forest = "forest";
    let rfr = format!("{0}, {1}, {0}!", run, forest);
    println!("{}", rfr);
    
    let rfr = format!("{r}, {f}, {r}!", r=run, f=forest);
    println!("{}", rfr);
}

fn main()
{
    strings();
    format();
}