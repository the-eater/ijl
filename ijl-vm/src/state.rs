use crate::fiber::{Executor, ExecutorState};
use gc_arena::{Collect, DynamicRoot, DynamicRootSet, Mutation, Rootable};

#[derive(Collect)]
#[collect(no_drop)]
pub struct State<'gc> {
    pub roots: DynamicRootSet<'gc>,
}

impl<'gc> State<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>) -> State<'gc> {
        let roots = DynamicRootSet::new(mc);

        State { roots }
    }
}

