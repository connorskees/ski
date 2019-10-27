use std::fmt;

/// Container for errors that can occur during lexing
#[derive(Debug)]
pub enum LexingError {
    InvalidIntegerLiteralCharacter(u32),
    RegexError(regex::Error, u32),
    ParseIntError(std::num::ParseIntError, u32),
}

impl std::convert::From<regex::Error> for LexingError {
    fn from(error: regex::Error) -> Self {
        LexingError::RegexError(error, line!())
    }
}

impl std::convert::From<std::num::ParseIntError> for LexingError {
    fn from(error: std::num::ParseIntError) -> Self {
        LexingError::ParseIntError(error, line!())
    }
}

#[derive(Debug)]
pub enum ParseError {
    Eof,
    Error(&'static str, u32),
}

impl std::convert::From<&'static str> for ParseError {
    fn from(error: &'static str) -> Self {
        ParseError::Error(error, line!())
    }
}
