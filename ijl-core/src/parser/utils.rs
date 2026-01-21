use crate::parser::Expression;
use chumsky::extra::ParserExtra;
use chumsky::input::{Input, MapExtra};
pub use chumsky::prelude::SimpleSpan;
use chumsky::span::SimpleSpanned;
use std::fmt::Debug;
use std::marker::PhantomData;
use ijl_type::Type;

pub trait Carrier: Debug + Clone {
    type Type: Debug + Clone + Eq;
    type Span: Debug + Clone;
}


#[derive(Debug, Copy, Clone)]
pub struct InProgress<C: Carrier>(PhantomData<C>);

impl<C: Carrier> Carrier for InProgress<C> {
    type Type = Option<C::Type>;
    type Span = C::Span;
}

#[derive(Debug, Clone, Copy)]
pub struct Untyped;

#[derive(Debug, Clone, Copy)]
pub struct Untracked;

pub type Spanned<C, V> = chumsky::span::Spanned<V, <C as Carrier>::Span>;

impl From<Expression<Untracked>> for Spanned<Untracked, Expression<Untracked>> {
    fn from(value: Expression<Untracked>) -> Self {
        Spanned::<Untracked, _> {
            inner: value,
            span: (),
        }
    }
}

impl From<Expression<Untracked>> for TypedSpan<Untracked, Expression<Untracked>> {
    fn from(value: Expression<Untracked>) -> Self {
        TypedSpan {
            resolved_type: (),
            span: (),
            inner: value,
        }
    }
}

impl Carrier for Untracked {
    type Type = ();
    type Span = ();
}

impl Carrier for Untyped {
    type Type = ();
    type Span = SimpleSpan;
}

#[derive(Debug, Copy, Clone)]
pub struct Typed;

impl Carrier for Typed {
    type Type = Type;
    type Span = SimpleSpan;
}

#[derive(Debug, Clone, Copy)]
pub struct TypedSpan<C: Carrier, V> {
    pub resolved_type: C::Type,
    pub span: C::Span,
    pub inner: V,
}

impl<C: Carrier, V> TypedSpan<C, V> {
    pub fn map<R, F: FnOnce(V) -> R>(self, f: F) -> TypedSpan<C, R> {
        TypedSpan {
            resolved_type: self.resolved_type,
            span: self.span,
            inner: f(self.inner),
        }
    }
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

pub fn sp<'src, 'b, V, I: Input<'src, Span = SimpleSpan>, E: ParserExtra<'src, I>>(
    value: V,
    x: &mut MapExtra<'src, 'b, I, E>,
) -> SimpleSpanned<V> {
    SimpleSpanned {
        inner: value,
        span: x.span(),
    }
}

pub fn ts<'src, 'b, V, I: Input<'src, Span = SimpleSpan>, E: ParserExtra<'src, I>>(
    value: V,
    x: &mut MapExtra<'src, 'b, I, E>,
) -> TypedSpan<Untyped, V> {
    TypedSpan {
        resolved_type: (),
        span: x.span(),
        inner: value,
    }
}
