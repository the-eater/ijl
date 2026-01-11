mod utils;

use crate::lexer::Token;
use crate::parser::utils::{Spanned, sp, ts};
use chumsky::extra::ParserExtra;
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use chumsky::primitive::select;
use chumsky::span::SimpleSpan;
use std::fmt::Debug;
use std::marker::PhantomData;
use chumsky::pratt::{infix, none, postfix, prefix};
use utils::{Carrier, TypedSpan, Untyped};

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
    Postfix {
        source: Box<Expression<C>>,
        postfixes: Postfix<C>,
    },
    Block(Block<C>),
    Literal(Literal),
    Infix(Infix<C>),
    ArrayAccess(ArrayAccess<C>),
    Property(Property<C>),
    Call(Call<C>),
    Unary(Unary<C>),
}

#[derive(Debug, Clone)]
pub enum Unary<C: Carrier> {
    Negate(Box<TypedSpan<C, Expression<C>>>)
}

#[derive(Debug, Clone)]
pub struct Infix<C: Carrier> {
    lhs: Box<TypedSpan<C, Expression<C>>>,
    rhs: Box<TypedSpan<C, Expression<C>>>,
    infix: Spanned<C, String>,
}

#[derive(Debug, Clone)]
pub struct Call<C: Carrier> {
    source: Box<TypedSpan<C, Expression<C>>>,
    arguments: Vec<TypedSpan<C, CallArgument<C>>>,
    block: Option<TypedSpan<C, Block<C>>>,
}

#[derive(Debug, Clone)]
pub struct CallArgument<C: Carrier> {
    name: Option<Spanned<C, String>>,
    value: CallArgumentValue<C>,
    spread: bool,
}

#[derive(Debug, Clone)]
pub enum CallArgumentValue<C: Carrier> {
    Template,
    Value(TypedSpan<C, Expression<C>>),
}

#[derive(Debug, Clone)]
pub struct Property<C: Carrier> {
    source: Box<TypedSpan<C, Expression<C>>>,
    name: Spanned<C, String>,
}

#[derive(Debug, Clone)]
pub struct ArrayAccess<C: Carrier> {
    source: Box<TypedSpan<C, Expression<C>>>,
    indexes: Vec<Box<TypedSpan<C, Expression<C>>>>,
}

#[derive(Debug, Clone)]
pub struct Assign<C: Carrier> {
    lhs: Box<Expression<C>>,
    rhs: Box<Expression<C>>,
}

#[derive(Debug, Clone)]
pub struct Block<C: Carrier> {
    items: Vec<TypedSpan<C, Statement<C>>>,
}

#[derive(Debug, Clone)]
enum Postfix<C: Carrier> {
    Array(Box<Expression<C>>),
    Property(Spanned<C, String>),
    Call(Vec<Expression<C>>),
    Block(Spanned<C, Block<C>>),
    Infix {
        infix: Spanned<C, String>,
        target: Box<Spanned<C, Expression<C>>>,
    },
}

fn parser<'tokens, 'src: 'tokens, I>()
    -> impl Parser<'tokens, I, Statement<Untyped>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token=Token<'src>, Span=SimpleSpan>,
{
    let ident = select! {
        Token::Ident(ident) => ident
    }
        .labelled("identifier");

    recursive(|stmt| {
        let expr = recursive(|expr| {
            let block = just(Token::BlockOpen)
                .ignore_then(
                    stmt.map_with(ts)
                        .clone()
                        .separated_by(just(Token::Semicolon).or(just(Token::NewLine)))
                        .collect(),
                )
                .then_ignore(just(Token::BlockClose))
                .map(|v| Block { items: v });

            let block_expr = block.clone().map(Expression::Block);

            let embedded_expr = just(Token::BracketOpen)
                .ignore_then(expr.clone())
                .then_ignore(just(Token::BracketClose))
                .map(|v: TypedSpan<Untyped, Expression<Untyped>>| {
                    Expression::Expression(Box::new(v))
                });

            let variable = ident.map_with(|v, x| {
                Expression::Ident(Spanned::<Untyped, _> {
                    inner: v.to_string(),
                    span: x.span(),
                })
            });

            let string = select! { Token::String(v) => Literal::String(v.to_string()) }
                .labelled("string literal");
            let number = select! { Token::Number(v) => Literal::Number(v.to_string()) }
                .labelled("number literal");
            let hex =
                select! { Token::Hex(v) => Literal::Hex(v.to_string()) }.labelled("hex literal");

            let literal = string.or(number).or(hex).map(Expression::Literal);

            let access = embedded_expr.or(block_expr).or(variable).or(literal).map_with(ts);

            let array_access = just(Token::ArrayOpen)
                .ignore_then(expr.clone().map(Box::new).separated_by(just(Token::Comma)).collect::<Vec<_>>())
                .then_ignore(just(Token::ArrayClose));
            let property = just(Token::Dot)
                .ignore_then(ident)
                .map_with(|v, x| sp(v.to_string(), x));

            let arg_value = expr.clone().map(CallArgumentValue::Value).or(just(Token::DollarSign).map(|_| CallArgumentValue::Template));

            let arg_positional = arg_value.clone().map_with(|value, x| ts(CallArgument {
                name: None,
                spread: false,
                value,
            }, x));

            let arg_named = ident.clone()
                .map_with(|v, x| sp(v.to_string(), x)).then_ignore(just(Token::EqualSign)).then(arg_value.clone()).map_with(|(name, value), x| ts(CallArgument {
                name: Some(name),
                value,
                spread: false,
            }, x));

            let arg = arg_named.or(arg_positional);

            let call = just(Token::BracketOpen)
                .ignore_then(arg.separated_by(just(Token::Comma)).collect::<Vec<_>>())
                .then_ignore(just(Token::BracketClose));
            let block_postfix = block.map_with(ts);

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
            let op = |t| just(t).map_with(ts);

            return access.pratt(
                (
                    postfix(10, array_access, |lhs, rhs, x| {
                        ts(Expression::ArrayAccess(ArrayAccess {
                            source: Box::new(lhs),
                            indexes: rhs,
                        }), x)
                    }),
                    postfix(10, property, |lhs, rhs, x| {
                        ts(Expression::Property(Property {
                            source: Box::new(lhs),
                            name: rhs,
                        }), x)
                    }),
                    postfix(9, call, |lhs, rhs, x| {
                        ts(Expression::Call(Call {
                            source: Box::new(lhs),
                            block: None,
                            arguments: rhs,
                        }), x)
                    }),
                    postfix(8, block_postfix, |mut lhs: TypedSpan<Untyped, Expression<Untyped>>, rhs: TypedSpan<Untyped, Block<Untyped>>, x| {
                        let rhs_span = rhs.span;
                        if let Expression::Call(v) = &mut lhs.inner && v.block.is_none() {
                            v.block = Some(rhs);
                        } else {
                            return ts(Expression::Call(Call {
                                source: Box::new(lhs),
                                block: Some(rhs),
                                arguments: vec![],
                            }), x)
                        };
                        lhs.span = lhs.span.union(rhs_span);
                        return lhs;
                    }),
                    prefix(6, op(Token::Minus), |_, rhs, x| {
                        ts(Expression::Unary(Unary::Negate(Box::new(rhs))), x)
                    }),
                    infix(none(2), infix_symbol, |lhs, infix, rhs, x| {
                        ts(Expression::Infix(Infix {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            infix,
                        }), x)
                    }),
                    infix(none(1), ident_symbol, |lhs, infix, rhs, x| {
                        ts(Expression::Infix(Infix {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            infix,
                        }), x)
                    })
                )
            );

            // access.foldl_with(
            //     (block_postfix
            //         .or(array_access)
            //         .or(property)
            //         .or(call)
            //         .or(infix))
            //     .repeated(),
            //     |l, r, x| Expression::Postfix {
            //         source: Box::new(l),
            //         postfixes: r,
            //     },
            // )
        });

        expr.map(Statement::Expression)
    })
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer;
    use crate::parser::parser;
    use ariadne::{Color, Label, Report, ReportKind, sources};
    use chumsky::Parser;
    use chumsky::input::Input;
    use chumsky::input::Stream;

    #[test]
    fn test_simple() {
        let x = r#"if {} else {}"#;
        let tokens = lexer(x).spanned();
        let mut stream =
            Stream::from_iter(tokens).map((0..x.len()).into(), |(t, s): (_, _)| (t, s));

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
