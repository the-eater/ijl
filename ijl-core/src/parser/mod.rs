pub mod utils;
pub mod xij;
mod traits;

use chumsky::extra::{ParserExtra};
use crate::lexer::Token;
use crate::parser::utils::{Spanned, sp, ts};
use chumsky::input::{MapExtra, ValueInput};
use chumsky::pratt::{infix, left, none, postfix, prefix, right};
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use std::fmt::Debug;
use chumsky::container::{OrderedSeq};
use crate::parser::xij::{XIJElement, XIJFragment};

pub use utils::{Untyped, InProgress, Typed, Carrier, TypedSpan};
pub use traits::*;

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
    Assign(Assign<C>),
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum InfixType {
    // 0
    As,
    // 1
    Multiply,
    Divide,
    Modulo,
    // 2
    Add,
    Subtract,
    // 3
    ShiftLeft,
    ShiftRight,
    // 4
    BitAnd,
    // 5
    BitXor,
    // 6
    BitOr,
    // 7
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,
    // 6
    LogicalAnd,
    LogicalOr,
    // 7
    Symbol(String),
    Word(String)
}

#[derive(Debug, Clone)]
pub struct Assign<C: Carrier> {
    pub lhs: Box<TypedSpan<C, Expression<C>>>,
    pub rhs: Box<TypedSpan<C, Expression<C>>>,
    pub modifier: Spanned<C, Option<InfixType>>,
}

#[derive(Debug, Clone)]
pub struct Infix<C: Carrier> {
    pub lhs: Box<TypedSpan<C, Expression<C>>>,
    pub rhs: Box<TypedSpan<C, Expression<C>>>,
    pub infix: Spanned<C, InfixType>,
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
    pub indexes: Vec<TypedSpan<C, Expression<C>>>,
}

#[derive(Debug, Clone)]
pub struct Block<C: Carrier> {
    pub return_type: C::Type,
    pub yield_type: C::Type,
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
        .map(|v| Block { return_type: (), yield_type: (), items: v });

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

        sp(InfixType::Symbol(symbol), x)
    });

    let ident_symbol = ident.map_with(|v, x| sp(InfixType::Word(v.to_string()), x));
    // let op = |t| just(t).map_with(ts);

    pub fn make_infix< 'src, 'b, I: Input<'src, Span = SimpleSpan>, E: ParserExtra<'src, I>>(lhs: TypedSpan<Untyped, Expression<Untyped>>, infix: Spanned<Untyped, InfixType>, rhs: TypedSpan<Untyped, Expression<Untyped>>, x: &mut MapExtra<'src, 'b, I, E>) -> TypedSpan<Untyped, Expression<Untyped>> {
        ts(
            Expression::Infix(Infix {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                infix,
            }),
            x,
        )
    }

    pub fn make_assign< 'src, 'b, I: Input<'src, Span = SimpleSpan>, E: ParserExtra<'src, I>>(lhs: TypedSpan<Untyped, Expression<Untyped>>, modifier: Spanned<Untyped, Option<InfixType>>, rhs: TypedSpan<Untyped, Expression<Untyped>>, x: &mut MapExtra<'src, 'b, I, E>) -> TypedSpan<Untyped, Expression<Untyped>> {
        ts(
            Expression::Assign(Assign {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                modifier
            }),
            x,
        )
    }

    fn if_as<'tokens, 'src: 'tokens, I, O: Clone>(i: impl OrderedSeq<'tokens, Token<'src>> + Clone, output: O) -> impl Parser<'tokens, I, Spanned<Untyped, O>, extra::Err<Rich<'tokens, Token<'src>>>> + Clone
    where
        I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan> {
        just(i).map_with(move |_, x| sp(output.clone(), x))
    }
    // let if_as = |t, o: InfixType| just(t).map_with(move |_, x| sp(o.clone(), x));

    let eq_infix = choice(
        (
            if_as([Token::EqualSign; 2], InfixType::Equals),
            if_as([Token::ExclamationMark, Token::EqualSign], InfixType::NotEquals),
            if_as(Token::AngleBracketOpen, InfixType::LessThan),
            if_as(Token::AngleBracketClose, InfixType::GreaterThan),
            if_as([Token::AngleBracketOpen, Token::EqualSign], InfixType::LessThanOrEquals),
            if_as([Token::AngleBracketClose, Token::EqualSign], InfixType::GreaterThanOrEquals),
        )
    );

    let assign = choice(
        (
            if_as(Token::EqualSign, None),
            if_as([Token::Plus, Token::EqualSign], Some(InfixType::Add)),
            if_as([Token::Minus, Token::EqualSign], Some(InfixType::Subtract)),
            if_as([Token::Asterisk, Token::EqualSign], Some(InfixType::Multiply)),
            if_as([Token::Slash, Token::EqualSign], Some(InfixType::Divide)),
            if_as([Token::Percentage, Token::EqualSign], Some(InfixType::Modulo)),
            if_as([Token::Ampersand, Token::EqualSign], Some(InfixType::BitAnd)),
            if_as([Token::Pipe, Token::EqualSign], Some(InfixType::BitOr)),
            if_as([Token::Xor, Token::EqualSign], Some(InfixType::BitXor)),
            if_as([Token::AngleBracketOpen, Token::AngleBracketOpen, Token::EqualSign], Some(InfixType::ShiftLeft)),
            if_as([Token::AngleBracketClose, Token::AngleBracketClose, Token::EqualSign], Some(InfixType::ShiftRight)),
        )
    );

    let full_expr = access.pratt((
        postfix(100, array_access, |lhs, rhs, x| {
            ts(
                Expression::ArrayAccess(ArrayAccess {
                    source: Box::new(lhs),
                    indexes: rhs,
                }),
                x,
            )
        }),
        postfix(100, property, |lhs, rhs, x| {
            ts(
                Expression::Property(Property {
                    source: Box::new(lhs),
                    name: rhs,
                }),
                x,
            )
        }),
        postfix(90, call, |lhs, rhs, x| {
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
            80,
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
        postfix(70, unary_postfix, |lhs, rhs, x| {
            ts(Expression::Unary(Unary {
                symbol: rhs,
                source: Box::new(lhs),
            }), x)
        }),
        prefix(60, unary_prefix, |lhs, rhs, x| {
            ts(Expression::Unary(Unary {
                symbol: lhs,
                source: Box::new(rhs),
            }), x)
        }),
        infix(left(50), if_as(Token::As, InfixType::As), make_infix),
        infix(left(49), if_as(Token::Asterisk, InfixType::Multiply).or(if_as(Token::Slash, InfixType::Divide)).or(if_as(Token::Percentage, InfixType::Modulo)), make_infix),
        infix(left(48), if_as(Token::Plus, InfixType::Add).or(if_as(Token::Minus, InfixType::Subtract)), make_infix),
        infix(left(47), if_as([Token::AngleBracketOpen, Token::AngleBracketOpen], InfixType::ShiftLeft).or(if_as([Token::AngleBracketClose; 2], InfixType::ShiftRight)), make_infix),
        infix(left(46), if_as(Token::Ampersand, InfixType::BitAnd), make_infix),
        infix(left(45), if_as(Token::Xor, InfixType::BitXor), make_infix),
        infix(left(44), if_as(Token::Pipe, InfixType::BitOr), make_infix),
        infix(left(43), eq_infix, make_infix),
        infix(left(42), if_as([Token::Ampersand; 2], InfixType::LogicalAnd), make_infix),
        infix(left(41), if_as([Token::Pipe; 2], InfixType::LogicalOr), make_infix),
        infix(right(40), assign, make_assign),
        infix(none(2), infix_symbol, make_infix),
        infix(none(1), ident_symbol, make_infix),
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
        let x = include_str!("../../../examples/expr.ijl");
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
