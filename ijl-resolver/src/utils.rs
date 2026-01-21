use std::fmt::Debug;
use ijl_core::parser::{IntoProgressCarrier, Typed};
use ijl_core::parser::utils::{Carrier, InProgress, SimpleSpan, Untyped};
use ijl_type::Type;

pub trait Named {
    fn name(&self) -> &str;
}

pub trait Resolvable: Debug + Clone + IntoProgressCarrier {
    type Output: Carrier<Type = Type, Span = SimpleSpan>;
}

impl<C: Resolvable> Resolvable for InProgress<C> {
    type Output = C::Output;
}


pub trait Resolving: Debug + Clone + Carrier<Type = Type> {}

impl<C: Carrier<Type = Type>> Resolving for C {

}

impl Resolvable for Untyped {
    type Output = Typed;
}