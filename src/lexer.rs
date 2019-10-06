#[derive(Debug, Eq, PartialEq, Hash)]
pub enum TypeKing<T> {
    Int,
    Float,
    Str,
    Bool,
    Array(T),
    Path_
}

