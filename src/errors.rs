use std::fmt;

/// Container for errors that can occur during lexing
#[derive(Debug)]
pub enum LexingError {}
#[derive(Debug)]
pub enum ParseError {
    Error(&'static str)
}

impl fmt::Display for LexingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LexingError::*;
        match self {
            _ => {
                write!(f, "")
            }
        }
    }
}

impl std::convert::From<&'static str> for ParseError {
    fn from(error: &'static str) -> Self {
        ParseError::Error(error)
    }
}
