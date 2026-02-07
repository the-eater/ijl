use allocator_api2::{boxed, vec};
use gc_arena::Mutation;
use gc_arena::allocator_api::MetricsAlloc;

pub type MetricVec<'gc, T> = vec::Vec<T, MetricsAlloc<'gc>>;
pub type MetricBox<'gc, T> = boxed::Box<T, MetricsAlloc<'gc>>;

pub trait MetricNew<'gc> {
    fn new(mc: &Mutation<'gc>) -> Self;
}

impl<'gc, T> MetricNew<'gc> for MetricVec<'gc, T> {
    fn new(mc: &Mutation<'gc>) -> Self {
        vec::Vec::new_in(MetricsAlloc::new(mc))
    }
}

#[macro_export]
macro_rules! mvec {
    [$mc:expr$(; $($item:expr),* $(,)?)?] => {
        {
            let mut item: $crate::t::MetricVec<_> = $crate::t::MetricNew::new($mc);
            $(
            $(item.push($item);)*;
            )?
            item
        }
    };
}