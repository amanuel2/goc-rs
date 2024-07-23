/// Reference of lexer taken from ANTLR
/// https://github.com/antlr/grammars-v4/blob/master/golang/GoLexer.g4
use std::{backtrace::BacktraceStatus, char, mem::ManuallyDrop};


#[derive(Clone, Debug, PartialEq, Eq)]
/// Hidden
///  - Whitespace
///  - Comment
///  - Terminator
///  - LineComment
pub enum Hidden {
    Whitespace,
    Comment,
    Terminator,
    LineComment
}

impl TokenRepr for Hidden {
    fn as_str(&self) -> &'static str {
        match self {
            Hidden::Whitespace => " ",
            Hidden::Comment => "//",
            Hidden::Terminator => "\r\n",
            Hidden::LineComment => "//"
        }
    }

    fn from(str: &'static str) -> Option<Self> {
        match str {
            " " => Some(Hidden::Whitespace),
            "/* */" => Some(Hidden::Comment),
            "\n" => Some(Hidden::Terminator),
            "//" => Some(Hidden::LineComment),
            _ => None
        }
    }
}

/// Literal
/// - Number
/// - String
#[repr(C)]
pub union Literal {
    number: std::mem::ManuallyDrop<NumberLiteral>,
    string: std::mem::ManuallyDrop<StringLiteral>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// StringLiteral
/// - RawString : : '`' ~'`'* '`'
/// - InterpretedString : '"' ~'"'* '"'
pub enum StringLiteral {
    RawString,
    InterpretedString
}

impl TokenRepr for StringLiteral {
    fn as_str(&self) -> &'static str {
        match self {
            StringLiteral::RawString=> "``",
            StringLiteral::InterpretedString => "\"\""
        }
    }

    fn from(str: &'static str) -> Option<Self> {
        match str {
            "``" => Some(StringLiteral::RawString),
            "\"\"" => Some(StringLiteral::InterpretedString),
            _ => None
        }
    }
}


#[derive(Clone, Debug, PartialEq, Eq)]
/// Float
///  - DECIMALS : '0'..'9'+;
///  - EXPONENT : ('e' | 'E') ('+' | '-')? DECIMALS;
pub enum Float {
    Decimals,
    Exponent
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// NumberLiteral
/// Decimal -> : ('0' | [1-9] ('_'? [0-9])*)   
/// Binary -> '0' [bB] ('_'? BIN_DIGIT)+    
/// Octal -> : '0' [oO]? ('_'? OCTAL_DIGIT)+ 
/// Hex -> '0' [xX] ('_'? HEX_DIGIT)+    
pub enum NumberLiteral {
    Decimal,
    Binary,
    Octal,
    Hex,
    Float(Float),
}

impl TokenRepr for NumberLiteral {
    fn as_str(&self) -> &'static str {
        match self {
            NumberLiteral::Decimal => "0-9",
            NumberLiteral::Binary => "0b1001",
            NumberLiteral::Octal => "0o123",
            NumberLiteral::Hex => "0x123",
            NumberLiteral::Float(Float::Decimals) => "1.23",
            NumberLiteral::Float(Float::Exponent) => "1.23e-10"
        }
    }

    fn from(str: &'static str) -> Option<Self> {
        match str {
            "0-9" => Some(NumberLiteral::Decimal),
            "0b1001" => Some(NumberLiteral::Binary),
            "0o123" => Some(NumberLiteral::Octal),
            "0x123" => Some(NumberLiteral::Hex),
            "1.23" => Some(NumberLiteral::Float(Float::Decimals)),
            "1.23e-10" => Some(NumberLiteral::Float(Float::Exponent)),
            _ => None
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Byte
///  - HexValue : '\\' ('x' | 'X') HexDigit HexDigit;
///  - OctalValue : '\\' OctalDigit OctalDigit OctalDigit;
pub enum Byte {
    HexValue(char, char, char, char),
    OctalValue(char, char, char, char),
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Rune
/// - Byte @Byte
/// - LittleUnicode : '\\' 'u' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit;
/// - BigUnicode : '\\' 'U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
///                 HexDigit HexDigit;
/// - HexByte : '\\' 'x' HexDigit HexDigit;
pub enum Rune {
    Byte(Byte),
    LittleUnicode(char, char, char, char, char, char),
    BigUnicode(char, char, char, char, char, char, char, char, char, char),
    HexByte(char, char),
}

impl TokenRepr for Rune {
    fn as_str(&self) -> &'static str {
        match self {
            Rune::Byte(Byte) => "\\b",
            Rune::LittleUnicode(char, char2, char3, char4, char5, char6) => Box::leak(format!("\\u{}{}{}{}{}{}", char, char2, char3, char4, char5, char6).into_boxed_str()),
            Rune::BigUnicode(char, char2, char3, char4, char5, char6, char7, char8, char9, char10) => Box::leak(format!("\\x{}{}{}{}{}{}{}{}{}{}", char,char2,char3,char4,char5,char6,char7,char8,char9,char10).into_boxed_str()),
            Rune::HexByte(char, char2) => Box::leak(format!("\\x{} {}", char, char2).into_boxed_str())
        }
    }

    fn from(str: &'static str) -> Option<Self> {
        unimplemented!()
        // match str {
        //     "\\x" => Some(Rune::Byte),
        //     "\\u" => Some(Rune::LittleUnicode),
        //     "\\U" => Some(Rune::BigUnicode),
        //     "\\x" => Some(Rune::HexByte),
        //     _ => None
        // }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Operator
/// Can be one of the following types
///  - logical
///  - relational
///  - arithmetic
///  - unary
///  - mixed
pub enum Operator {
    Logical(LogicalOp),
    Relation(RelationOp),
    Arithmetic(ArithmeticOp),
    Unary(UnaryOp),
    Mixed(MixedOp),
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Logical operators
///  - LOGICAL_OR  : '||';
///  - LOGICAL_AND : '&&';
pub enum LogicalOp {
    LogicalOr,
    LogicalAnd
}

impl TokenRepr for LogicalOp {
    fn as_str(&self) -> &'static str {
        match self {
            LogicalOp::LogicalOr => "||",
            LogicalOp::LogicalAnd => "&&"
        }
    }

    fn from(str: &'static str) -> Option<Self> {
        match str {
            "||" => Some(LogicalOp::LogicalOr),
            "&&" => Some(LogicalOp::LogicalAnd), 
            _ => None
        }
    
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Relation operators
pub enum RelationOp {
    Equals,
    NotEquals,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEquals
}

impl TokenRepr for RelationOp {
    fn as_str(&self) -> &'static str {
        match self {
            RelationOp::Equals => "==",
            RelationOp::NotEquals => "!=",
            RelationOp::Less => "<",
            RelationOp::LessOrEqual => "<=",
            RelationOp::Greater => ">",
            RelationOp::GreaterOrEquals => ">="
        }
    }
    
    fn from(str: &'static str) -> Option<Self> {
        match str {
            "==" => Some(RelationOp::Equals),
            "!=" => Some(RelationOp::NotEquals),
            "<" => Some(RelationOp::Less),
            "<=" => Some(RelationOp::LessOrEqual),
            ">" => Some(RelationOp::Greater),
            ">=" => Some(RelationOp::GreaterOrEquals),
            _ => None
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Arithmetic Operators
///	- OR         : '|';
///	- DIV        : '/';
///	- MOD        : '%';
///	- LSHIFT     : '<<';
///	- RSHIFT     : '>>';
///	- BIT_CLEAR  : '&^';
///	- UNDERLYING : '~';
pub enum ArithmeticOp {
    Or,
    Div,
    Mod,
    LShift,
    RShift,
    BitClear,
    Underlying
}

impl TokenRepr for ArithmeticOp {
    fn as_str(&self) -> &'static str {
        match self {
            ArithmeticOp::Or => "|",
            ArithmeticOp::Div => "/",
            ArithmeticOp::Mod => "%",
            ArithmeticOp::LShift => "<<",
            ArithmeticOp::RShift => ">>",
            ArithmeticOp::BitClear => "&^",
            ArithmeticOp::Underlying => "~"
        }
    }

    fn from(str: &'static str) -> Option<Self> {
        match str {
            "|" => Some(ArithmeticOp::Or),
            "/" => Some(ArithmeticOp::Div),
            "%" => Some(ArithmeticOp::Mod),
            "<<" => Some(ArithmeticOp::LShift),
            ">>" => Some(ArithmeticOp::RShift),
            "&^" => Some(ArithmeticOp::BitClear),
            "~" => Some(ArithmeticOp::Underlying),
            _ => None
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Unary Operator
pub enum UnaryOp {
    Exclamation
}

impl TokenRepr for UnaryOp {
    fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::Exclamation => "!"
        }
    }

    fn from(str: &'static str) -> Option<Self> {
        match str {
            "!" => Some(UnaryOp::Exclamation),
            _ => None
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Mixed Operators
/// - PLUS     : '+';
/// - MINUS    : '-';
/// - CARET    : '^';
/// - STAR     : '*';
/// - AMPERSAND: '&';
/// - RECIEVE  : '<-';
pub enum MixedOp {
    Plus,
    Minus,
    Caret,
    Star,
    Ampersand,
    Recieve
}

impl TokenRepr for MixedOp {
    fn as_str(&self) -> &'static str {
        match self {
            MixedOp::Plus => "+",
            MixedOp::Minus => "-",
            MixedOp::Caret => "^",
            MixedOp::Star => "*",
            MixedOp::Ampersand => "&",
            MixedOp::Recieve => "<-"
        }
    }

    fn from(str: &'static str) -> Option<Self> {
        match str {
            "+" => Some(MixedOp::Plus),
            "-" => Some(MixedOp::Minus),
            "^" => Some(MixedOp::Caret),
            "*" => Some(MixedOp::Star),
            "&" => Some(MixedOp::Ampersand),
            "<-" => Some(MixedOp::Recieve),
            _ => None
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Punctuation
/// - LParen : '(';
/// - RParen : ')';
/// - LCurly : '{';
/// - RCurly : '}';
/// - LBracket : '[';
/// - RBracket : ']';
/// - Assign : '=';
/// - Comma : ',';
/// - Semi : ';';
/// - Colon : ':';
/// - Dot : '.';
/// - PlusPlus : '++';
/// - MinusMinus : '--';
/// - DeclareAssign : ':=';
/// - Ellipsis : '...'
pub enum Punctuation {
    LParen,
	RParen,
	LCurly,
	RCurly,
	LBracket,
	RBracket,
	Assign,
	Comma,
	Semi,
	Colon,
	Dot,
	PlusPlus,
	MinusMinus,
	DeclareAssign,
	Ellipsis
}

impl TokenRepr for Punctuation {
    fn as_str(&self) -> &'static str {
        match self {
            Punctuation::LParen => "(",
            Punctuation::RParen => ")",
            Punctuation::LCurly => "{",
            Punctuation::RCurly => "}",
            Punctuation::LBracket => "[",
            Punctuation::RBracket => "]",
            Punctuation::Assign => "=",
            Punctuation::Comma => ",",
            Punctuation::Semi => ";",
            Punctuation::Colon => ":",
            Punctuation::Dot => ".",
            Punctuation::PlusPlus => "++",
            Punctuation::MinusMinus => "--",
            Punctuation::DeclareAssign => ":=",
            Punctuation::Ellipsis => "..."
        }
    }

    fn from(str: &'static str) -> Option<Self> {
        match str {
            "(" => Some(Punctuation::LParen),
            ")" => Some(Punctuation::RParen),
            "{" => Some(Punctuation::LCurly),
            "}" => Some(Punctuation::RCurly),
            "[" => Some(Punctuation::LBracket),
            "]" => Some(Punctuation::RBracket),
            "=" => Some(Punctuation::Assign),
            "," => Some(Punctuation::Comma),
            ";" => Some(Punctuation::Semi),
            ":" => Some(Punctuation::Colon),
            "." => Some(Punctuation::Dot),
            "++" => Some(Punctuation::PlusPlus),
            "--" => Some(Punctuation::MinusMinus),
            ":=" => Some(Punctuation::DeclareAssign),
            "..." => Some(Punctuation::Ellipsis),
            _ => None
        }
    }
}



#[derive(Clone, Debug, PartialEq, Eq)]
/// Keyword
/// All keywords used in golang
pub enum Keyword {
    Break,
    Default,
    Func,
    Interface,
    Select,
    Case,
    Defer,
    Go,
    Map,
    Struct,
    Chan,
    Else,
    Goto,
    Package,
    Switch,
    Const,
    Fallthrough,
    If,
    Range,
    Type,
    Continue,
    For,
    Import,
    Return,
    Var,
}

impl TokenRepr for Keyword {
    fn as_str(&self) -> &'static str {
        match self {
            Keyword::Break => "break",
            Keyword::Default => "default",
            Keyword::Func => "func",
            Keyword::Interface => "interface",
            Keyword::Select => "select",
            Keyword::Case => "case",
            Keyword::Defer => "defer",
            Keyword::Go => "go",
            Keyword::Map => "map",
            Keyword::Struct => "struct",
            Keyword::Chan => "chan",
            Keyword::Else => "else",
            Keyword::Goto => "goto",
            Keyword::Package => "package",
            Keyword::Switch => "switch",
            Keyword::Const => "const",
            Keyword::Fallthrough => "fallthrough",
            Keyword::If => "if",
            Keyword::Range => "range",
            Keyword::Type => "type",
            Keyword::Continue => "continue",
            Keyword::For => "for",
            Keyword::Import => "import",
            Keyword::Return => "return",
            Keyword::Var => "var"
        }
    }

    fn from(s: &'static str) -> Option<Self> {
        match s {
            "break" => Some(Keyword::Break),
            "default" => Some(Keyword::Default),
            "func" => Some(Keyword::Func),
            "interface" => Some(Keyword::Interface),
            "select" => Some(Keyword::Select),
            "case" => Some(Keyword::Case),
            "defer" => Some(Keyword::Defer),
            "go" => Some(Keyword::Go),
            "map" => Some(Keyword::Map),
            "struct" => Some(Keyword::Struct),
            "chan" => Some(Keyword::Chan),
            "else" => Some(Keyword::Else),
            "goto" => Some(Keyword::Goto),
            "package" => Some(Keyword::Package),
            "switch" => Some(Keyword::Switch),
            "const" => Some(Keyword::Const),
            "fallthrough" => Some(Keyword::Fallthrough),
            "if" => Some(Keyword::If),
            "range" => Some(Keyword::Range),
            "type" => Some(Keyword::Type),
            "continue" => Some(Keyword::Continue),
            "for" => Some(Keyword::For),
            "import" => Some(Keyword::Import),
            "return" => Some(Keyword::Return),
            "var" => Some(Keyword::Var),
            _ => None
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// TokenType
/// - Literal
/// - Number
/// - Float
/// - Rune
/// - String
pub enum TokenType {
    Number(NumberLiteral),
    String(StringLiteral),
    Float(Float),
    Rune(Rune),
    Keyword(Keyword),
    Punctuation(Punctuation),
    Operator(Operator),
    Hidden(Hidden)
}

/// trait implemented by all token variants
trait TokenRepr {
    fn from(s: &'static str) -> Option<Self> where Self: Sized;
    fn as_str(&self) -> &'static str;
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Token describes the generic interface for all types  
/// of tokens which include
///     - Keyword
///     - Literals
///     - Operators
///     - Brackets
struct Token {
    token_type: TokenType,
    value: &'static str,
    line: u64,
}

impl TokenType {
    fn as_str(&self) -> &'static str {
        match self {
            Literal => "Literal",
            Number => "Number",
            Float => "Float",
            Rune => "Rune",
            String => "String",
            Keyword => "Keyword",
            Punctuation => "Punctuation",
            Operator => "Operator",
            Hidden => "Hidden"
        }
    }

    fn from(s: &'static str) -> Option<Self> {
        unimplemented!()
        // match s {
        //     "Number" => Some(TokenType::Number),
        //     "Float" => Some(TokenType::Float),
        //     "Rune" => Some(TokenType::Rune),
        //     "String" => Some(TokenType::String),
        //     "Keyword" => Some(TokenType::Keyword),
        //     "Punctuation" => Some(TokenType::Punctuation),
        //     "Operator" => Some(TokenType::Operator),
        //     _ => Hidden::from(s).map(|h| TokenType::Hidden(h))
        // }
    }
}
