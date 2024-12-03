use std::io;
extern crate combine;
use combine::parser::char::{spaces, digit, char};
use combine::{many1, sep_by, Parser, EasyParser};
use combine::stream::easy;
use combine::parser::sequence;
use combine::parser::repeat::{Many1};
use combine::parser::token::{Token};


use crate::lib1::read_lines;



pub fn parse(input: &str) -> (i32, i32) {
  let integer_1 = (many1(digit()).map(|string: String| string.parse::<i32>().unwrap()));
  let integer_2 = (many1(digit()).map(|string: String| string.parse::<i32>().unwrap()));
  //let chars: Many1<String, Token<easy::Stream<&str>>> = (many1(char('a')));

  let skip_spaces = spaces().silent();

  let mut p = (integer_1, skip_spaces, integer_2);

  //Parse integers separated by commas, skipping whitespace
  //let mut integer_list = sep_by(integer, spaces().skip(char(',')));

  //Call parse with the input to execute the parser
  let input = "1234 876";
  let result =
      p.easy_parse(input);
  match result {
      Ok((value, _remaining_input)) => (value.0, value.2),
      Err(err) => panic!("error")
  }
}




pub fn run() -> () {

  let lines: Vec<String> = read_lines("./src/p01/data"); //.unwrap();

  let input = "1234 876";
  let r = parse(input);
  println!("{:?}", lines.len())

}