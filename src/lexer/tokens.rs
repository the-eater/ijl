use logos::{Lexer, Logos, Span};

#[derive(Debug, Clone, Default)]
pub(super) struct IJlExtras {
    pub(super) xml_depth: usize,
}

#[derive(Debug, Clone, Logos)]
#[logos(skip r"[ \r\t]+", extras = IJlExtras)]
pub(super) enum IJlToken<'src> {
    #[regex(r"<\p{XID_START}\p{XID_CONTINUE}*")]
    XIJ(&'src str),
    #[token(r"<>")]
    XIJFragment,
    #[regex(r"\p{XID_START}\p{XID_CONTINUE}*")]
    Ident(&'src str),
    #[token("\n")]
    NewLine,
    #[token("{")]
    BlockOpen,
    #[token("}")]
    BlockClose,
    #[token("[")]
    ArrayOpen,
    #[token("]")]
    ArrayClose,
    #[token(".")]
    Dot,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token(r"\")]
    Backslash,
    #[token("$")]
    DollarSign,
    #[token("*")]
    Asterisk,
    #[token("%")]
    Percentage,
    #[token(">")]
    AngleBracketClose,
    #[token("<")]
    AngleBracketOpen,
    #[token("^")]
    Xor,
    #[token("|")]
    Pipe,
    #[token("&")]
    Ampersand,
    #[token("@")]
    At,
    #[token("#")]
    Hash,
    #[token("=")]
    EqualSign,
    #[token("_")]
    Underscore,
    #[token("~")]
    Tilde,
    #[token("`")]
    Backtick,
    #[token("...")]
    Spread,
    #[regex(r"(?i)0[a-f]+")]
    Hex(&'src str),
    #[regex(r"[0-9]+(_[0-9]+)*(.[0-9]+(_[0-9]+)*)?")]
    Number(&'src str),
    #[regex(r#""[^"]*(\\"[^"]*)*""#)]
    String(&'src str),
    #[token(",")]
    Comma,
    #[token("(")]
    BracketOpen,
    #[token(")")]
    BracketClose,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("class")]
    Class,
    #[token("trait")]
    Trait,
    #[token("interface")]
    Interface,
    #[token("define")]
    Define,
    #[token("return")]
    Return,
    #[token("yield")]
    Yield,
}

#[derive(Debug, Clone, Logos)]
#[logos(skip r"\s+", extras = IJlExtras)]
pub(super) enum XIJToken<'src> {
    #[token("/>")]
    SelfClose,
    #[token(">")]
    Close,
    #[regex(r"\p{XID_START}\p{XID_CONTINUE}*")]
    Ident(&'src str),
    #[token("=")]
    Assign,
    #[token("=>")]
    AssignEvent,
    #[token("{")]
    Expression,
    #[regex(r#""[^"]*(\\"[^"]*)*""#)]
    String(&'src str),
}

#[derive(Debug, Clone, Logos)]
#[logos(extras = IJlExtras)]
pub(super) enum XIJContentToken<'src> {
    #[token("{")]
    Block,
    #[regex(r"<>")]
    XmlFragment,
    #[regex(r"<\p{XID_START}\p{XID_CONTINUE}*")]
    Xml(&'src str),
    #[regex(r"</\p{XID_START}\p{XID_CONTINUE}*>")]
    XmlClose(&'src str),
    #[token("</>")]
    XmlFragmentClose,
    #[regex(r"[^{<]+", priority = 1)]
    Text(&'src str),
}

#[cfg(test)]
mod tests {
    use crate::lexer::{IJlToken};
    use logos::Logos;

    #[test]
    fn test_xij() {
        let x = r#"wow.let { it + 1 } + 4"#;
        let c = IJlToken::lexer(x);
        let v = c.collect::<Result<Vec<_>, _>>().unwrap();
        println!("{v:?}");
    }

    #[test]
    fn test_simple_example() {
        let source = include_str!("../../examples/simple.ijl");
        let _tokens = IJlToken::lexer(source).collect::<Result<Vec<_>, _>>().unwrap();
    }
}
