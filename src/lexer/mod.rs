use chumsky::span::SimpleSpan;
use logos::Logos;
use crate::lexer::tokens::{IJlToken, XIJContentToken, XIJToken};
mod tokens;

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, PartialEq, Eq)]
pub enum Token {
    Ident,
    NewLine,
    BlockOpen,
    BlockClose,
    ArrayOpen,
    ArrayClose,
    Dot,
    Plus,
    Minus,
    Slash,
    AngleBracketClose,
    AngleBracketOpen,
    Xor,
    Pipe,
    Ampersand,
    At,
    Hash,
    EqualSign,
    Underscore,
    Hex,
    Number,
    String,
    Comma,
    BracketOpen,
    BracketClose,
    Semicolon,
    Colon,
    Class,
    Trait,
    Interface,
    Define,
    Return,
    Yield,
    XIJText,
    XIJFragmentOpen,
    XIJFragmentClose,
    XIJElementStart,
    XIJElementEnd,
    XIJElementClose,
    XIJElementSelfClose,
    XIJEventAssign,
    InternalError,
}

pub struct Lexer<'a> {
    lexer: Lexer<'a, IJlToken>,
}

enum XIJLexer<'a> {
    Element(Lexer<'a, >)
}

pub fn lex(input: &str) -> impl Iterator<Item=(Token, SimpleSpan)> {
    IJlToken::lexer(input)
        .spanned()
        .flat_map(|(v, span)| {
            match v {
                Ok(_) => {}
                Err(e) => vec![(Token::InternalError, span.into())],
            }
        })
}

impl From<IJlToken> for Token {
    fn from(value: IJlToken) -> Self {
        match value {
            IJlToken::Ident => Token::Ident,
            IJlToken::NewLine => Token::NewLine,
            IJlToken::BlockOpen => Token::BlockOpen,
            IJlToken::BlockClose => Token::BlockClose,
            IJlToken::ArrayOpen => Token::ArrayOpen,
            IJlToken::ArrayClose => Token::ArrayClose,
            IJlToken::Dot => Token::Dot,
            IJlToken::Plus => Token::Plus,
            IJlToken::Minus => Token::Minus,
            IJlToken::Slash => Token::Slash,
            IJlToken::AngleBracketClose => Token::AngleBracketClose,
            IJlToken::AngleBracketOpen => Token::AngleBracketOpen,
            IJlToken::Xor => Token::Xor,
            IJlToken::Pipe => Token::Pipe,
            IJlToken::Ampersand => Token::Ampersand,
            IJlToken::At => Token::At,
            IJlToken::Hash => Token::Hash,
            IJlToken::EqualSign => Token::EqualSign,
            IJlToken::Underscore => Token::Underscore,
            IJlToken::Hex => Token::Hex,
            IJlToken::Number => Token::Number,
            IJlToken::String => Token::String,
            IJlToken::Comma => Token::Comma,
            IJlToken::BracketOpen => Token::BracketOpen,
            IJlToken::BracketClose => Token::BracketClose,
            IJlToken::Semicolon => Token::Semicolon,
            IJlToken::Colon => Token::Colon,
            IJlToken::Class => Token::Class,
            IJlToken::Trait => Token::Trait,
            IJlToken::Interface => Token::Interface,
            IJlToken::Define => Token::Define,
            IJlToken::Return => Token::Return,
            IJlToken::Yield => Token::Yield,
            IJlToken::XIJ(_) | IJlToken::XIJFragment(_) => panic!("unsupported"),
        }
    }
}

impl From<XIJContentToken> for Token {
    fn from(value: XIJContentToken) -> Self {
        match value {
            XIJContentToken::Text => Token::XIJText,
            XIJContentToken::XmlFragment => Token::XIJFragmentOpen,
            XIJContentToken::XmlClose => Token::XIJElementClose,
            XIJContentToken::XmlFragmentClose => Token::XIJFragmentClose,
            XIJContentToken::Block(_) | XIJContentToken::Xml(_) => panic!("unsupported"),
        }
    }
}

impl From<XIJToken> for Token {
    fn from(value: XIJToken) -> Self {
        match value {
            XIJToken::Open => Token::XIJElementStart,
            XIJToken::Close => Token::XIJElementEnd,
            XIJToken::SelfClose => Token::XIJElementSelfClose,
            XIJToken::Ident => Token::Ident,
            XIJToken::Assign => Token::EqualSign,
            XIJToken::AssignEvent => Token::XIJEventAssign,
            XIJToken::String => Token::String,
            XIJToken::Expression(_) => panic!("unsupported"),
        }
    }
}
