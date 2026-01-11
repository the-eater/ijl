use std::fmt::Debug;
use chumsky::input::{Input, MapExtra};
use chumsky::prelude::SimpleSpan;
use chumsky::extra::ParserExtra;
use chumsky::span::SimpleSpanned;

pub trait Carrier: Debug + Clone {
    type Type: Debug + Clone;
    type Span: Debug + Clone;
}

#[derive(Debug, Clone)]
pub struct Untyped;

pub type Spanned<C, V> = chumsky::span::Spanned<V, <C as Carrier>::Span>;

impl Carrier for Untyped {
    type Type = ();
    type Span = SimpleSpan;
}

#[derive(Debug, Clone, Copy)]
pub struct TypedSpan<C: Carrier, V> {
    resolved_type: C::Type,
    span: C::Span,
    pub(crate) inner: V,
}

impl<V> From<Spanned<Untyped, V>> for TypedSpan<Untyped, V> {
    fn from(value: Spanned<Untyped, V>) -> Self {
        TypedSpan {
            resolved_type: (),
            span: value.span,
            inner: value.inner,
        }
    }
}

pub fn sp< 'src, 'b, V, I: Input<'src, Span = SimpleSpan>, E: ParserExtra<'src, I>>(value: V, x: &mut MapExtra<'src, 'b, I, E>) -> SimpleSpanned<V> {
    SimpleSpanned {
        inner: value,
        span: x.span(),
    }
}

pub fn ts< 'src, 'b, V, I: Input<'src, Span = SimpleSpan>, E: ParserExtra<'src, I>>(value: V, x: &mut MapExtra<'src, 'b, I, E>) -> TypedSpan<Untyped, V> {
    TypedSpan {
        resolved_type: (),
        span: x.span(),
        inner: value,
    }
}