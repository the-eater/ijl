use std::fmt::Display;
use std::mem;
use chumsky::input::InputRef;
use chumsky::Parser;
use crate::lexer::tokens::{IJlToken, XIJContentToken, XIJToken};
use chumsky::span::SimpleSpan;
use logos::Logos;
mod tokens;

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, PartialEq, Eq)]
pub enum Token<'src> {
    Ident(&'src str),
    NewLine,
    BlockOpen,
    BlockClose,
    ArrayOpen,
    ArrayClose,
    Dot,
    Plus,
    Minus,
    Slash,
    Backslash,
    Percentage,
    DollarSign,
    AngleBracketClose,
    AngleBracketOpen,
    Xor,
    Pipe,
    Ampersand,
    At,
    Hash,
    EqualSign,
    Underscore,
    Spread,
    Hex(&'src str),
    Number(&'src str),
    String(&'src str),
    Comma,
    BracketOpen,
    BracketClose,
    Semicolon,
    Colon,
    Asterisk,
    Tilde,
    Backtick,
    Class,
    Trait,
    Interface,
    Define,
    Return,
    Yield,
    XIJText(&'src str),
    XIJFragmentOpen,
    XIJFragmentClose,
    XIJElementStart(&'src str),
    XIJElementEnd,
    XIJElementClose(&'src str),
    XIJElementSelfClose,
    XIJEventAssign,
    InternalError,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Token::Ident(v) => v,
            Token::At => "@",
            Token::Ampersand => "&",
            Token::Slash => "/",
            Token::Backslash => r"\",
            Token::DollarSign => "$",
            Token::Hash => "#",
            Token::Minus => "-",
            Token::Plus => "+",
            Token::BlockOpen => "{",
            Token::BlockClose => "}",
            Token::Percentage => "%",
            Token::ArrayOpen => "[",
            Token::ArrayClose => "]",
            Token::BracketClose => ")",
            Token::BracketOpen => "(",
            Token::Xor => "^",
            Token::AngleBracketOpen => "<",
            Token::AngleBracketClose => ">",
            Token::Asterisk => "*",
            Token::Colon => ":",
            Token::Semicolon => ";",
            Token::NewLine => "\\n",
            Token::Pipe => "|",
            Token::EqualSign => "=",
            Token::Dot => ".",
            Token::Comma => ",",
            Token::Underscore => "_",
            Token::Hex(v) => v,
            Token::Number(v) => v,
            Token::String(v) => v,
            Token::Tilde => "~",
            Token::Backtick => "`",
            Token::Spread => "...",
            _ => return write!(f, "{self:?}")
        };

        write!(f, "{s}")
    }
}

pub struct Lexer<'src, const WITH_SPAN: bool = false> {
    lexer: IJlLexer<'src>,
    depth: usize,
}

enum IJlLexer<'src> {
    IJl(logos::Lexer<'src, IJlToken<'src>>),
    XIJ(logos::Lexer<'src, XIJToken<'src>>),
    XIJContent(logos::Lexer<'src, XIJContentToken<'src>>),
    Poisoned
}

impl<'src, const WITH_SPAN: bool> Lexer<'src, WITH_SPAN> {
    pub fn span(&self) -> SimpleSpan {
        match &self.lexer {
            IJlLexer::IJl(l) => l.span(),
            IJlLexer::XIJ(l) => l.span(),
            IJlLexer::XIJContent(l) => l.span(),
            IJlLexer::Poisoned => unreachable!(),
        }
        .into()
    }
    pub fn next_token(&mut self) -> Option<Token<'src>> {
        let mut ret = None;
        let lexer = mem::replace(&mut self.lexer, IJlLexer::Poisoned);

        self.lexer = match lexer {
            IJlLexer::IJl(mut l) => {
                if let Some(token) = l.next() {
                    let token = token.map_or(Token::InternalError, Into::into);
                    ret = Some(token);
                    match token {
                        Token::XIJFragmentOpen => {
                            self.depth += 1;
                            IJlLexer::XIJContent(l.morph())
                        }
                        Token::XIJElementStart(_) => {
                            self.depth += 1;
                            IJlLexer::XIJ(l.morph())
                        }
                        _ => IJlLexer::IJl(l),
                    }
                } else {
                    IJlLexer::IJl(l)
                }
            }
            IJlLexer::XIJ(mut l) => {
                if let Some(token) = l.next() {
                    let token = token.map_or(Token::InternalError, Into::into);
                    ret = Some(token);
                    match token {
                        Token::XIJElementEnd => {
                            IJlLexer::XIJContent(l.morph())
                        }
                        Token::XIJElementSelfClose => {
                            self.depth -= 1;
                            if self.depth == 0 {
                                IJlLexer::IJl(l.morph())
                            } else {
                                IJlLexer::XIJContent(l.morph())
                            }
                        }

                        _ => IJlLexer::XIJ(l),
                    }
                } else {
                    IJlLexer::XIJ(l)
                }
            }
            IJlLexer::XIJContent(mut l) => {
                if let Some(token) = l.next() {
                    let token = token.map_or(Token::InternalError, Into::into);
                    ret = Some(token);
                    match token {
                        Token::XIJElementClose(_) | Token::XIJFragmentClose => {
                            self.depth -= 1;
                            if self.depth == 0 {
                                IJlLexer::IJl(l.morph())
                            } else {
                                IJlLexer::XIJContent(l.morph())
                            }
                        }

                        _ => IJlLexer::XIJContent(l),
                    }
                } else {
                    IJlLexer::XIJContent(l)
                }
            }
            IJlLexer::Poisoned => {
                unreachable!();
            }
        };

        ret
    }

    pub fn spanned(self) -> Lexer<'src, true> {
        Lexer {
            lexer: self.lexer,
            depth: 0,
        }
    }
}

impl<'src> Iterator for Lexer<'src, true> {
    type Item = (Token<'src>, SimpleSpan);

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token()?;
        Some((tok, self.span()))
    }
}

impl<'src> Iterator for Lexer<'src, false> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

pub fn lexer(input: &str) -> Lexer<false> {
    Lexer::<false> {
        lexer: IJlLexer::IJl(IJlToken::lexer(input)),
        depth: 0,
    }
}

impl<'src> From<IJlToken<'src>> for Token<'src> {
    fn from(value: IJlToken<'src>) -> Self {
        match value {
            IJlToken::Ident(i) => Token::Ident(i),
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
            IJlToken::Hex(v) => Token::Hex(v),
            IJlToken::Number(v) => Token::Number(v),
            IJlToken::String(v) => Token::String(v),
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
            IJlToken::XIJ (v)=> Token::XIJElementStart(v),
            IJlToken::XIJFragment => Token::XIJFragmentOpen,
            IJlToken::Backslash => Token::Backslash,
            IJlToken::DollarSign => Token::DollarSign,
            IJlToken::Percentage => Token::Percentage,
            IJlToken::Asterisk => Token::Asterisk,
            IJlToken::Tilde => Token::Tilde,
            IJlToken::Backtick => Token::Backtick,
            IJlToken::Spread => Token::Spread,
        }
    }
}

impl<'src> From<XIJContentToken<'src>> for Token<'src> {
    fn from(value: XIJContentToken<'src>) -> Self {
        match value {
            XIJContentToken::Text(v) => Token::XIJText(v),
            XIJContentToken::XmlFragment => Token::XIJFragmentOpen,
            XIJContentToken::XmlClose(v) => Token::XIJElementClose(v),
            XIJContentToken::XmlFragmentClose => Token::XIJFragmentClose,
            XIJContentToken::Block => Token::BlockOpen,
            XIJContentToken::Xml(v) => Token::XIJElementStart(v),
        }
    }
}

impl<'src> From<XIJToken<'src>> for Token<'src> {
    fn from(value: XIJToken<'src>) -> Self {
        match value {
            XIJToken::Close => Token::XIJElementEnd,
            XIJToken::SelfClose => Token::XIJElementSelfClose,
            XIJToken::Ident(v) => Token::Ident(v),
            XIJToken::Assign => Token::EqualSign,
            XIJToken::AssignEvent => Token::XIJEventAssign,
            XIJToken::String(v) => Token::String(v),
            XIJToken::Expression => Token::BlockOpen,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Token, lexer};

    #[test]
    fn test_simple() {
        let input = "let x = <>1</>; wow";
        let output = vec![
            Token::Ident("let"),
            Token::Ident("x"),
            Token::EqualSign,
            Token::XIJFragmentOpen,
            Token::XIJText("1"),
            Token::XIJFragmentClose,
            Token::Semicolon,
            Token::Ident("wow"),
        ];

        assert_eq!(output, lexer(input).collect::<Vec<_>>());
    }
}
