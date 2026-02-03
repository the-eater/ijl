use std::collections::VecDeque;
use crate::lexer::tokens::{IJlToken, XIJContentToken, XIJToken};
use chumsky::span::SimpleSpan;
use logos::Logos;
use std::fmt::Display;
use std::mem;
use chumsky::container::Container;

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
    DoubleAmpersand,
    At,
    Hash,
    QuestionMark,
    ExclamationMark,
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
    DoubleAsterisk,
    Tilde,
    Backtick,
    Class,
    Trait,
    Interface,
    Define,
    Return,
    Yield,
    As,
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
            Token::DoubleAmpersand => "&&",
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
            Token::DoubleAsterisk => "**",
            Token::ExclamationMark => "!",
            Token::QuestionMark => "?",
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
            Token::As => "as",
            Token::XIJFragmentClose => "</>",
            Token::XIJElementSelfClose => "/>",
            Token::XIJElementEnd => ">",
            Token::XIJFragmentOpen => "<>",
            _ => return write!(f, "{self:?}"),
        };

        write!(f, "{s}")
    }
}


#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LexerType {
    IJl,
    XIJ,
    XIJContent,
}

enum IJlLexer<'src> {
    IJl(logos::Lexer<'src, IJlToken<'src>>),
    XIJ(logos::Lexer<'src, XIJToken<'src>>),
    XIJContent(logos::Lexer<'src, XIJContentToken<'src>>),
    Poisoned,
}

impl<'src> IJlLexer<'src> {
    pub fn lexer_type(&self) -> LexerType {
        match self {
            IJlLexer::IJl(_) => LexerType::IJl,
            IJlLexer::XIJ(_) => LexerType::XIJ,
            IJlLexer::XIJContent(_) => LexerType::XIJContent,
            IJlLexer::Poisoned => unreachable!(),
        }
    }

    fn morph_into<T: Logos<'src, Source=str, Extras=()>>(self) -> logos::Lexer<'src, T> {
        match self {
            IJlLexer::IJl(l) => l.morph(),
            IJlLexer::XIJ(l) => l.morph(),
            IJlLexer::XIJContent(l) => l.morph(),
            IJlLexer::Poisoned => unreachable!(),
        }
    }

    pub fn morph(self, lexer_type: LexerType) -> Self {
        if lexer_type == self.lexer_type() {
            return self;
        }
        match lexer_type {
            LexerType::IJl => IJlLexer::IJl(self.morph_into()),
            LexerType::XIJ => IJlLexer::XIJ(self.morph_into()),
            LexerType::XIJContent => IJlLexer::XIJContent(self.morph_into()),
        }
    }
}

pub struct Lexer<'src, const WITH_SPAN: bool = false> {
    lexer: IJlLexer<'src>,
    stack: Vec<LexerType>,
    transform_stack: VecDeque<Token<'src>>,
    last_token: Option<Token<'src>>,
}

impl<'src, const WITH_SPAN: bool> Lexer<'src, WITH_SPAN> {
    pub fn with_logos<L: Logos<'src, Source = str>>(lexer: logos::Lexer<'src, L>) -> Self
        where (): From<<L as Logos<'src>>::Extras> {
        Lexer {
            lexer: IJlLexer::IJl(lexer.morph()),
            stack: vec![],
            transform_stack: Default::default(),
            last_token: None,
        }
    }

    #[inline]
    pub fn new(input: &'src str) -> Self {
        Self::with_logos(logos::Lexer::<IJlToken<'src>>::new(input))
    }

    pub fn span(&self) -> SimpleSpan {
        match &self.lexer {
            IJlLexer::IJl(l) => l.span(),
            IJlLexer::XIJ(l) => l.span(),
            IJlLexer::XIJContent(l) => l.span(),
            IJlLexer::Poisoned => unreachable!(),
        }
            .into()
    }

    fn last_token_is_before_start_expression(&self) -> bool {
        let Some(tok) = self.last_token else { return true; };

        !matches!(tok, Token::Ident(_) | Token::ArrayClose | Token::BlockClose | Token::BracketClose)
    }

    pub fn next_token(&mut self) -> Option<Token<'src>> {
        if let Some(tok) = self.transform_stack.pop_front() {
            return Some(tok);
        }

        let mut ret = None;
        let lexer = mem::replace(&mut self.lexer, IJlLexer::Poisoned);

        self.lexer = match lexer {
            IJlLexer::IJl(mut l) => {
                if let Some(token) = l.next() {
                    let token = token.map_or(Token::InternalError, Into::into);
                    ret = Some(token);
                    match token {
                        Token::XIJFragmentOpen => {
                            if self.last_token_is_before_start_expression() {
                                self.stack.push(LexerType::IJl);
                                IJlLexer::XIJContent(l.morph())
                            } else {
                                ret = Some(Token::AngleBracketOpen);
                                self.transform_stack.push(Token::AngleBracketClose);
                                IJlLexer::IJl(l)
                            }
                        }
                        Token::XIJElementStart(name) => {
                            if self.last_token_is_before_start_expression() {
                                self.stack.push(LexerType::IJl);
                                IJlLexer::XIJ(l.morph())
                            } else {
                                ret = Some(Token::AngleBracketOpen);
                                self.transform_stack.push(Token::Ident(name));
                                IJlLexer::IJl(l)
                            }
                        }
                        Token::BlockOpen => {
                            self.stack.push(LexerType::IJl);
                            IJlLexer::IJl(l)
                        }
                        Token::BlockClose => {
                            let Some(v) = self.stack.pop() else {
                                unreachable!();
                            };

                            IJlLexer::IJl(l).morph(v)
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
                        Token::BlockOpen => {
                            self.stack.push(LexerType::XIJ);
                            IJlLexer::IJl(l.morph())
                        }
                        Token::XIJElementSelfClose => {
                            let Some(v) = self.stack.pop() else {
                                unreachable!();
                            };

                            IJlLexer::XIJ(l).morph(v)
                        }

                        Token::XIJElementEnd => {
                            IJlLexer::XIJContent(l.morph())
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
                            let Some(v) = self.stack.pop() else {
                                unreachable!();
                            };

                            IJlLexer::XIJContent(l).morph(v)
                        }
                        Token::XIJFragmentOpen => {
                            self.stack.push(LexerType::XIJContent);
                            IJlLexer::XIJContent(l)
                        }
                        Token::XIJElementStart(_) => {
                            self.stack.push(LexerType::XIJContent);
                            IJlLexer::XIJ(l.morph())
                        }
                        Token::BlockOpen => {
                            self.stack.push(LexerType::XIJContent);
                            IJlLexer::IJl(l.morph())
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

        self.last_token = ret;
        ret
    }

    pub fn spanned(self) -> Lexer<'src, true> {
        Lexer {
            lexer: self.lexer,
            stack: self.stack,
            transform_stack: self.transform_stack,
            last_token: self.last_token,
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

pub fn lexer<'src>(input: &'src str) -> Lexer<'src, false> {
    Lexer::<false>::new(input)
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
            IJlToken::QuestionMark => Token::QuestionMark,
            IJlToken::ExclamationMark => Token::ExclamationMark,
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
            IJlToken::XIJ(v) => Token::XIJElementStart(v),
            IJlToken::XIJFragment => Token::XIJFragmentOpen,
            IJlToken::Backslash => Token::Backslash,
            IJlToken::DollarSign => Token::DollarSign,
            IJlToken::Percentage => Token::Percentage,
            IJlToken::Asterisk => Token::Asterisk,
            IJlToken::Tilde => Token::Tilde,
            IJlToken::Backtick => Token::Backtick,
            IJlToken::Spread => Token::Spread,
            IJlToken::As => Token::As,
            IJlToken::DoubleAsterisk => Token::DoubleAsterisk,
            IJlToken::DoubleAmpersand => Token::DoubleAmpersand,
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
            XIJToken::Hex(h) => Token::Hex(h),
            XIJToken::Number(n) => Token::Number(n),
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

    #[test]
    fn test_spread() {
        let input = "...";
        let output = vec![Token::Spread];

        assert_eq!(output, lexer(input).collect::<Vec<_>>());
    }

    #[test]
    fn test_xij() {
        let input = r#"<>{1}</>"#;
        let output = vec![
            Token::XIJFragmentOpen,
            Token::BlockOpen,
            Token::Number("1"),
            Token::BlockClose,
            Token::XIJFragmentClose,
        ];

        assert_eq!(output, lexer(input).collect::<Vec<_>>());
    }

    #[test]
    fn test_example_xij() {
        let input = r#"<hello world=42 5 "yes" {...wow}>item</hello>"#;
        let output = vec![
            Token::XIJElementStart("hello"),
            Token::Ident("world"),
            Token::EqualSign,
            Token::Number("42"),
            Token::Number("5"),
            Token::String(r#""yes""#),
            Token::BlockOpen,
            Token::Spread,
            Token::Ident("wow"),
            Token::BlockClose,
            Token::XIJElementEnd,
            Token::XIJText("item"),
            Token::XIJElementClose("hello")
        ];

        assert_eq!(output, lexer(input).collect::<Vec<_>>());
    }

    #[test]
    fn test_simple_html() {
        let input = "<div>\n  <Counter />\n</div>";
        let output = vec![
            Token::XIJElementStart("div"),
            Token::XIJElementEnd,
            Token::XIJText("\n  "),
            Token::XIJElementStart("Counter"),
            Token::XIJElementSelfClose,
            Token::XIJText("\n"),
            Token::XIJElementClose("div")
        ];

        assert_eq!(output, lexer(input).collect::<Vec<_>>());
    }

    #[test]
    fn test_transform() {
        let input_a = "(<>Hello</>)";
        let input_b = "Hello<>";
        let input_c = "<>Hello</>";

        let output_a = vec![Token::BracketOpen, Token::XIJFragmentOpen, Token::XIJText("Hello"), Token::XIJFragmentClose, Token::BracketClose];
        let output_b = vec![Token::Ident("Hello"), Token::AngleBracketOpen, Token::AngleBracketClose];
        let output_c = vec![Token::XIJFragmentOpen, Token::XIJText("Hello"), Token::XIJFragmentClose];

        assert_eq!(output_a, lexer(input_a).collect::<Vec<_>>());
        assert_eq!(output_b, lexer(input_b).collect::<Vec<_>>());
        assert_eq!(output_c, lexer(input_c).collect::<Vec<_>>());
    }
}
