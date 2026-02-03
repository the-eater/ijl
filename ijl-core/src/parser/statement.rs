use crate::lexer::Token;
use crate::parser::utils::sp;
use crate::parser::{Statement, Untyped};
use chumsky::error::Rich;
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use chumsky::{Parser, extra};

//
// pub(crate) fn parser<'tokens, 'src: 'tokens, I>()
// -> impl Parser<'tokens, I, Statement<Untyped>, extra::Err<Rich<'tokens, Token<'src>>>>
// where
//     I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
// {
//     todo!()
// }

#[derive(Debug, Clone)]
pub struct TypeName {
    name: Spanned<String>,
    arguments: Vec<Spanned<TypeName>>,
}

const TYPE_ENCLOSURE: [Token; 2] = [Token::AngleBracketOpen, Token::AngleBracketClose];

pub(crate) fn type_name<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<TypeName>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select! {
        Token::Ident(ident) => ident.to_string()
    }
    .labelled("type identifier");

    let mut type_name = Recursive::declare();

    type_name.define(
        ident
            .map_with(sp)
            .then(
                type_name
                    .clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(
                        just(TYPE_ENCLOSURE[0]),
                        just(TYPE_ENCLOSURE[1]),
                    )
                    .or_not(),
            )
            .map_with(|(name, args), x| {
                sp(
                    TypeName {
                        name,
                        arguments: args.unwrap_or_default(),
                    },
                    x,
                )
            }),
    );

    type_name
}
//
// pub(crate) fn generics<'tokens, 'src: 'tokens, I>()
// -> impl Parser<'tokens, I, Statement<Untyped>, extra::Err<Rich<'tokens, Token<'src>>>>
// where
//     I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
// {
//     let ident = select! {
//         Token::Ident(ident) => ident
//     }
//     .labelled("type identifier");
//
//     just(ident).then(just(Token::Colon).ignore_then(type_name()));
//
//     todo!();
// }

#[cfg(test)]
mod tests {
    use crate::lexer::{lexer, Token};
    use crate::parser::statement::type_name;
    use chumsky::{Parser, input::{Input, Stream}};

    #[test]
    fn test_type_name() {
        let x = type_name().parse(&[Token::Ident("Hello")]).unwrap();
        assert_eq!(x.name.as_str(), "Hello");

        let x = r"String<>";
        let tokens = lexer(x).spanned();
        let stream = Stream::from_iter(tokens).map((0..x.len()).into(), |(t, s): (_, _)| (t, s));
        let x = type_name().parse(stream).unwrap();
        println!("{x:?}");
    }
}
