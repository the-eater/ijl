use logos::{Lexer, Logos, Span};

#[derive(Debug, Clone, Default)]
pub struct IJlExtras {
    xml_depth: usize,
}

#[derive(Debug, Clone, Logos)]
#[logos(skip r"[ \r\t]+", extras = IJlExtras)]
pub(super) enum IJlToken {
    #[regex(r"<\p{XID_START}\p{XID_CONTINUE}*", enter_ijl_xml)]
    XIJ(Vec<(Span, XIJContentToken)>),
    #[token(r"<>", enter_ijl_xml)]
    XIJFragment(Vec<(Span, XIJContentToken)>),
    #[regex(r"\p{XID_START}\p{XID_CONTINUE}*")]
    Ident,
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
    #[regex(r"(?i)0[a-f]+")]
    Hex,
    #[regex(r"[0-9]+(_[0-9]+)*(.[0-9]+(_[0-9]+)*)?")]
    Number,
    #[regex(r#""[^"]*(\\"[^"]*)*""#)]
    String,
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
pub(super) enum XIJToken {
    Open,
    #[token("/>")]
    SelfClose,
    #[token(">")]
    Close,
    #[regex(r"\p{XID_START}\p{XID_CONTINUE}*")]
    Ident,
    #[token("=")]
    Assign,
    #[token("=>")]
    AssignEvent,
    #[token("{", enter_ijl)]
    Expression(Vec<(Span, IJlToken)>),
    #[regex(r#""[^"]*(\\"[^"]*)*""#)]
    String,
}

#[derive(Debug, Clone, Logos)]
#[logos(extras = IJlExtras)]
pub(super) enum XIJContentToken {
    Block(Vec<IJlToken>),
    #[regex(r"<>", enter_xml_fragment)]
    XmlFragment,
    #[regex(r"<\p{XID_START}\p{XID_CONTINUE}*", enter_xml)]
    Xml(Vec<(Span, XIJToken)>),
    #[regex(r"</\p{XID_START}\p{XID_CONTINUE}*>", enter_xml_close)]
    XmlClose,
    #[token("</>", enter_xml_close)]
    XmlFragmentClose,
    #[regex(r"[^{<]+", priority = 1)]
    Text,
}

fn enter_ijl<'a, T: Logos<'a, Source = str, Extras = IJlExtras> + Clone>(
    lexer: &mut Lexer<'a, T>,
) -> Result<Vec<(Span, IJlToken)>, ()>
{
    let mut morphed = lexer.clone().morph::<IJlToken>();
    let mut evs = vec![(lexer.span(), IJlToken::BlockOpen)];
    let mut depth = 1;
    while let Some(ev) = morphed.next().transpose()? {
        match &ev {
            IJlToken::BlockClose => {
                depth -= 1;
            }

            _ => {}
        }
        evs.push((morphed.span(),ev));
        if depth == 0 {
            break;
        }
    }

    *lexer = morphed.morph();
    Ok(evs)
}

fn enter_ijl_xml(lexer: &mut Lexer<IJlToken>) -> Result<Vec<(Span, XIJContentToken)>, ()> {
    let sp = lexer.span();
    let depth = lexer.extras.xml_depth;
    let mut morphed:Lexer<XIJContentToken> = lexer.clone().morph();
    let mut xml_content = if lexer.slice() == "<>" {
        morphed.extras.xml_depth += 1;
        vec![(lexer.span(), XIJContentToken::XmlFragment)]
    } else {
        vec![(sp, XIJContentToken::Xml(enter_xml(&mut morphed)?))]
    };
    while depth < morphed.extras.xml_depth {
        let Some(ev) = morphed.next().transpose()? else {
            break;
        };
        xml_content.push((morphed.span(), ev));
    }

    *lexer = morphed.morph();

    Ok(xml_content)
}

fn enter_xml_fragment(lexer: &mut Lexer<XIJContentToken>) {
    lexer.extras.xml_depth += 1;
}

fn enter_xml_close(lexer: &mut Lexer<XIJContentToken>) {
    lexer.extras.xml_depth -= 1;
}

fn enter_xml(lexer: &mut Lexer<XIJContentToken>) -> Result<Vec<(Span, XIJToken)>, ()> {
    let mut token = lexer.clone().morph::<XIJToken>();

    let mut tokens = vec![(lexer.span(), XIJToken::Open)];
    while let Some(v) = token.next().transpose()? {
        let must_break = matches!(v, XIJToken::Close | XIJToken::SelfClose);
        if matches!(v, XIJToken::Close) {
            token.extras.xml_depth += 1;
        }
        tokens.push((token.span(), v));
        if must_break {
            break;
        }
    }

    *lexer = token.morph();

    Ok(tokens)
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
