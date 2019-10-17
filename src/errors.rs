use std::fmt;

/// Container for errors that can occur during lexing
#[derive(Debug)]
pub enum LexingError {}

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

// impl std::convert::From<StringDecodeError> for PNGDecodingError {
//     fn from(error: StringDecodeError) -> Self {
//         PNGDecodingError::StringDecodeError(error)
//     }
// }