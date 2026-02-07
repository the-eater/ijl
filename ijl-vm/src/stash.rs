use crate::fiber::Executor;
use gc_arena::{DynamicRoot, DynamicRootSet, Mutation, Rootable};

pub trait Stashable<'gc> {
    type Stashed;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed;
}

pub trait Fetchable {
    type Fetched<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc>;
}

pub struct StashedExecutor(DynamicRoot<Rootable![Executor<'_>]>);

impl<'gc> Stashable<'gc> for Executor<'gc> {
    type Stashed = StashedExecutor;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedExecutor(roots.stash::<Rootable![Executor<'_>]>(mc, self))
    }
}
