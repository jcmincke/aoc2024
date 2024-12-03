

use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;

pub fn do_read_lines(filename: impl AsRef<Path>) -> io::Result<Vec<String>> {
    BufReader::new(File::open(filename)?).lines().collect()
}


pub fn read_lines(filename: impl AsRef<Path>) -> Vec<String> {

  let r = do_read_lines(filename);

  if let Ok(v) = r {
      v
    }
  else {
    panic!("error");
  }
}
