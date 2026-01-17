use logos::{Lexer, Logos};


#[derive(Debug, Clone, Logos, PartialEq, Eq)]
#[logos(skip r"[ \r\t]+")]
pub(super) enum IJlToken<'src> {
    #[regex(r"<\p{XID_START}\p{XID_CONTINUE}*", strip_xml_prefix)]
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
    #[token("...", priority = 100)]
    Spread,
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
    #[token("?")]
    QuestionMark,
    #[token("!")]
    ExclamationMark,
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
    #[regex(r"(?i)0x[a-f]+")]
    Hex(&'src str),
    #[regex(r"[0-9]+(_[0-9]+)*(\.[0-9]+(_[0-9]+)*)?")]
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
    #[token("as")]
    As,
}

#[derive(Debug, Clone, Logos)]
#[logos(skip r"\s+")]
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
    #[regex(r"(?i)0x[a-f]+")]
    Hex(&'src str),
    #[regex(r"[0-9]+(_[0-9]+)*(\.[0-9]+(_[0-9]+)*)?")]
    Number(&'src str),
    #[regex(r#""[^"]*(\\"[^"]*)*""#)]
    String(&'src str),
}

#[derive(Debug, Clone, Logos)]
#[logos()]
pub(super) enum XIJContentToken<'src> {
    #[token("{")]
    Block,
    #[regex(r"<>")]
    XmlFragment,
    #[regex(r"<\p{XID_START}\p{XID_CONTINUE}*", strip_xml_prefix)]
    Xml(&'src str),
    #[regex(r"</\p{XID_START}\p{XID_CONTINUE}*>", strip_xml_prefix)]
    XmlClose(&'src str),
    #[token("</>")]
    XmlFragmentClose,
    #[regex(r"[^{<]+", priority = 1)]
    Text(&'src str),
}

fn strip_xml_prefix<'src, T: Logos<'src, Source = str>>(logos: &mut Lexer<'src, T>) -> &'src str {
    let mut input = logos.slice();
    if let Some(v) = input.strip_suffix(">") {
        input = v;
    }

    if input.starts_with("</") {
        return &input[2..];
    }
    if input.starts_with("<") {
        return &input[1..];
    }
    input
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
        let source = include_str!("../../../examples/simple.ijl");
        let _tokens = IJlToken::lexer(source).collect::<Result<Vec<_>, _>>().unwrap();
    }

    #[test]
    fn test_number() {
        let input = "1 2 3 4 5";
        let expected = vec![
            IJlToken::Number("1"),
            IJlToken::Number("2"),
            IJlToken::Number("3"),
            IJlToken::Number("4"),
            IJlToken::Number("5"),
        ];


        let output = IJlToken::lexer(input).collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(expected, output);
    }
}
