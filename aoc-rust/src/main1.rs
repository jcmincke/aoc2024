extern crate combine;
use combine::{Parser, EasyParser};
use combine::stream::position;
use combine::parser::char::{digit, letter};
const MSG: &'static str = r#"Parse error at line: 1, column: 1
Unexpected `|`
Expected digit or letter
"#;

fn main() {
    // Wrapping a `&str` with `State` provides automatic line and column tracking. If `State`
    // was not used the positions would instead only be pointers into the `&str`
    if let Err(err) = digit().or(letter()).easy_parse(position::Stream::new("|")) {
        assert_eq!(MSG, format!("{}", err));
    }

    println!("Hello, world!");
}


