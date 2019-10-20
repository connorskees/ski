use std::fmt;

/// Container for errors that can occur during lexing
#[derive(Debug)]
pub enum LexingError {
    InvalidIntegerLiteralCharacter,
    RegexError(regex::Error),
    ParseIntError(std::num::ParseIntError),
}

impl std::convert::From<regex::Error> for LexingError {
    fn from(error: regex::Error) -> Self {
        LexingError::RegexError(error)
    }
}

impl std::convert::From<std::num::ParseIntError> for LexingError {
    fn from(error: std::num::ParseIntError) -> Self {
        LexingError::ParseIntError(error)
    }
}

#[derive(Debug)]
pub enum ParseError {
    Eof,
    Error(&'static str),
}

impl std::convert::From<&'static str> for ParseError {
    fn from(error: &'static str) -> Self {
        ParseError::Error(error)
    }
}
