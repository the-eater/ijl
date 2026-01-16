pub mod utils;
pub mod xij;

use crate::lexer::Token;
use crate::parser::utils::{Spanned, sp, ts};
use chumsky::input::ValueInput;
use chumsky::pratt::{infix, none, postfix, prefix};
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use std::fmt::Debug;
use utils::{Carrier, TypedSpan, Untyped};
use crate::parser::xij::{XIJElement, XIJFragment};

#[derive(Debug, Clone)]
pub enum Statement<C: Carrier> {
    Expression(TypedSpan<C, Expression<C>>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Number(String),
    Hex(String),
}

#[derive(Debug, Clone)]
pub enum Expression<C: Carrier> {
    Ident(Spanned<C, String>),
    Expression(Box<TypedSpan<C, Expression<C>>>),
    Block(Block<C>),
    Literal(Literal),
    Infix(Infix<C>),
    ArrayAccess(ArrayAccess<C>),
    Property(Property<C>),
    Call(Call<C>),
    Unary(Unary<C>),
    XIJFragment(XIJFragment<C>),
    XIJElement(XIJElement<C>)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnarySymbolLocation {
    Postfix,
    Prefix,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnarySymbol {
    Question(UnarySymbolLocation),
    Exclamation(UnarySymbolLocation),
    Minus,
    Plus,
    Asterisk,
    Tilde,
    Ampersand,
}

#[derive(Debug, Clone)]
pub struct Unary<C: Carrier> {
    pub symbol: Spanned<C, UnarySymbol>,
    pub source: Box<TypedSpan<C, Expression<C>>>,
}

#[derive(Debug, Clone)]
pub struct Infix<C: Carrier> {
    pub lhs: Box<TypedSpan<C, Expression<C>>>,
    pub rhs: Box<TypedSpan<C, Expression<C>>>,
    pub infix: Spanned<C, String>,
}

#[derive(Debug, Clone)]
pub struct Call<C: Carrier> {
    pub source: Box<TypedSpan<C, Expression<C>>>,
    pub arguments: Vec<TypedSpan<C, CallArgument<C>>>,
    pub block: Option<TypedSpan<C, Block<C>>>,
}

#[derive(Debug, Clone)]
pub struct CallArgument<C: Carrier> {
    pub r#type: CallArgumentType<C>,
    pub value: CallArgumentValue<C>,
}

#[derive(Debug, Clone)]
pub enum CallArgumentType<C: Carrier> {
    Named(Spanned<C, String>),
    Spread(C::Span),
    Positional,
}

#[derive(Debug, Clone)]
pub enum CallArgumentValue<C: Carrier> {
    Template,
    Value(TypedSpan<C, Expression<C>>),
}

#[derive(Debug, Clone)]
pub struct Property<C: Carrier> {
    pub source: Box<TypedSpan<C, Expression<C>>>,
    pub name: Spanned<C, String>,
}

#[derive(Debug, Clone)]
pub struct ArrayAccess<C: Carrier> {
    pub source: Box<TypedSpan<C, Expression<C>>>,
    pub indexes: Vec<Box<TypedSpan<C, Expression<C>>>>,
}

#[derive(Debug, Clone)]
pub struct Block<C: Carrier> {
    pub items: Vec<TypedSpan<C, Statement<C>>>,
}

pub(crate) fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Statement<Untyped>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select! {
        Token::Ident(ident) => ident
    }
    .labelled("identifier");

    let mut stmt = Recursive::declare();
    let mut expr = Recursive::declare();

    let block = just(Token::BlockOpen)
        .ignore_then(
            stmt.clone()
                .map_with(ts)
                .separated_by(just(Token::Semicolon).or(just(Token::NewLine)))
                .collect(),
        )
        .then_ignore(just(Token::BlockClose))
        .map(|v| Block { items: v });

    let block_expr = block.clone().map(Expression::Block);

    let embedded_expr = just(Token::BracketOpen)
        .ignore_then(expr.clone())
        .then_ignore(just(Token::BracketClose))
        .map(|v: TypedSpan<Untyped, Expression<Untyped>>| Expression::Expression(Box::new(v)));

    let variable = ident.map_with(|v, x| {
        Expression::Ident(Spanned::<Untyped, _> {
            inner: v.to_string(),
            span: x.span(),
        })
    });

    let string =
        select! { Token::String(v) => Literal::String(v.to_string()) }.labelled("string literal");
    let number =
        select! { Token::Number(v) => Literal::Number(v.to_string()) }.labelled("number literal");
    let hex = select! { Token::Hex(v) => Literal::Hex(v.to_string()) }.labelled("hex literal");

    let literal = string.or(number).or(hex).map(Expression::Literal);

    let access = embedded_expr
        .or(block_expr)
        .or(variable)
        .or(literal)
        .map_with(ts);

    let access = access.or(xij::parser(expr.clone()));

    let array_access = just(Token::ArrayOpen)
        .ignore_then(
            expr.clone()
                .map(Box::new)
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::ArrayClose));
    let property = just(Token::Dot)
        .ignore_then(ident)
        .map_with(|v, x| sp(v.to_string(), x));

    let arg_value = expr
        .clone()
        .map(CallArgumentValue::Value)
        .or(just(Token::DollarSign).map(|_| CallArgumentValue::Template));

    let arg_positional = just(Token::Spread)
        .spanned()
        .or_not()
        .then(arg_value.clone())
        .map_with(|(spread, value): (Option<Spanned<Untyped, Token>>, _), x| {
            ts(
                CallArgument {
                    r#type: if let Some(v) = spread {
                        CallArgumentType::Spread(v.span)
                    } else {
                        CallArgumentType::Positional
                    },
                    value,
                },
                x,
            )
        });

    let arg_named = ident
        .clone()
        .map_with(|v, x| sp(v.to_string(), x))
        .then_ignore(just(Token::EqualSign))
        .then(arg_value.clone())
        .map_with(|(name, value), x| {
            ts(
                CallArgument {
                    r#type: CallArgumentType::Named(name),
                    value,
                },
                x,
            )
        });

    let arg = arg_named.or(arg_positional);

    let call =
        arg.separated_by(just(Token::Comma)).collect::<Vec<_>>().delimited_by(just(Token::BracketOpen), just(Token::BracketClose));
    let block_postfix = block.map_with(ts);

    let unary_postfix = one_of([
        Token::ExclamationMark,
        Token::QuestionMark,
    ]).map_with(|v, x| sp(match v {
        Token::ExclamationMark => UnarySymbol::Exclamation(UnarySymbolLocation::Postfix),
        Token::QuestionMark => UnarySymbol::Question(UnarySymbolLocation::Postfix),
        _ => unreachable!(),
    }, x));
    let unary_prefix = one_of([
        Token::Tilde,
        Token::Minus,
        Token::Plus,
        Token::Asterisk,
        Token::QuestionMark,
        Token::ExclamationMark,
        Token::Ampersand,
    ])
        .map_with(|v, x| sp(match v {
            Token::ExclamationMark => UnarySymbol::Exclamation(UnarySymbolLocation::Prefix),
            Token::QuestionMark => UnarySymbol::Question(UnarySymbolLocation::Prefix),
            Token::Tilde => UnarySymbol::Tilde,
            Token::Minus => UnarySymbol::Minus,
            Token::Plus => UnarySymbol::Plus,
            Token::Asterisk => UnarySymbol::Asterisk,
            Token::Ampersand => UnarySymbol::Ampersand,
            _ => unreachable!(),
        }, x));

    let infix_symbol = one_of(&[
        Token::Pipe,
        Token::EqualSign,
        Token::AngleBracketClose,
        Token::AngleBracketOpen,
        Token::Plus,
        Token::Minus,
        Token::At,
        Token::Ampersand,
        Token::Slash,
        Token::Colon,
        Token::Asterisk,
        Token::Xor,
        Token::Slash,
        Token::Backslash,
        Token::DollarSign,
        Token::Underscore,
    ])
    .repeated()
    .at_least(2)
    .collect()
    .map_with(|v: Vec<Token>, x| {
        use std::fmt::Write;
        let mut symbol = String::new();
        for i in v {
            write!(symbol, "{i}").unwrap();
        }

        sp(symbol, x)
    });

    let ident_symbol = ident.map_with(|v, x| sp(v.to_string(), x));
    // let op = |t| just(t).map_with(ts);

    let full_expr = access.pratt((
        postfix(10, array_access, |lhs, rhs, x| {
            ts(
                Expression::ArrayAccess(ArrayAccess {
                    source: Box::new(lhs),
                    indexes: rhs,
                }),
                x,
            )
        }),
        postfix(10, property, |lhs, rhs, x| {
            ts(
                Expression::Property(Property {
                    source: Box::new(lhs),
                    name: rhs,
                }),
                x,
            )
        }),
        postfix(9, call, |lhs, rhs, x| {
            ts(
                Expression::Call(Call {
                    source: Box::new(lhs),
                    block: None,
                    arguments: rhs,
                }),
                x,
            )
        }),
        postfix(
            8,
            block_postfix,
            |mut lhs: TypedSpan<Untyped, Expression<Untyped>>,
             rhs: TypedSpan<Untyped, Block<Untyped>>,
             x| {
                let rhs_span = rhs.span;
                if let Expression::Call(v) = &mut lhs.inner
                    && v.block.is_none()
                {
                    v.block = Some(rhs);
                } else {
                    return ts(
                        Expression::Call(Call {
                            source: Box::new(lhs),
                            block: Some(rhs),
                            arguments: vec![],
                        }),
                        x,
                    );
                };
                lhs.span = lhs.span.union(rhs_span);
                return lhs;
            },
        ),
        postfix(7, unary_postfix, |lhs, rhs, x| {
            ts(Expression::Unary(Unary {
                symbol: rhs,
                source: Box::new(lhs),
            }), x)
        }),
        prefix(6, unary_prefix, |lhs, rhs, x| {
            ts(Expression::Unary(Unary {
                symbol: lhs,
                source: Box::new(rhs),
            }), x)
        }),
        infix(none(2), infix_symbol, |lhs, infix, rhs, x| {
            ts(
                Expression::Infix(Infix {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    infix,
                }),
                x,
            )
        }),
        infix(none(1), ident_symbol, |lhs, infix, rhs, x| {
            ts(
                Expression::Infix(Infix {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    infix,
                }),
                x,
            )
        }),
    ));

    expr.define(full_expr);
    stmt.define(expr.map(Statement::Expression));
    stmt
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer;
    use crate::parser::parser;
    use ariadne::{Color, Label, Report, ReportKind, sources};
    use chumsky::Parser;
    use chumsky::input::Input;
    use chumsky::input::Stream;
    use crate::parse;

    #[test]
    fn test_expr() {
        let x = include_str!("../../examples/expr.ijl");
        let x = parse(x);
    }

    #[test]
    fn test_simple() {
        let x = r#"<>hello {"world"}</>"#;
        let tokens = lexer(x).spanned();
        let stream = Stream::from_iter(tokens).map((0..x.len()).into(), |(t, s): (_, _)| (t, s));

        let (src, errs) = parser().parse(stream).into_output_errors();
        println!("{src:#?}");

        errs.into_iter()
            .map(|e| e.map_token(|c| c.to_string()))
            .for_each(|e| {
                Report::build(ReportKind::Error, ("<embed>", e.span().into_range()))
                    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                    .with_message(e.to_string())
                    .with_label(
                        Label::new(("<embed>", e.span().into_range()))
                            .with_message(e.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .with_labels(e.contexts().map(|(label, span)| {
                        Label::new(("<embed>", span.into_range()))
                            .with_message(format!("while parsing this {label}"))
                            .with_color(Color::Yellow)
                    }))
                    .finish()
                    .print(sources([("<embed>", x)]))
                    .unwrap()
            });
    }
}
