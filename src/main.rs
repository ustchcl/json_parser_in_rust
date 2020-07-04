
mod json_parser;

fn main() {
    let json = "{ \"key1\" : true , \"key2\" : [7, false] }";
    let result = json_parser::parse(json);
    println!("the result is {:?}", result);
}
 