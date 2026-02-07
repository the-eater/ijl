#![allow(unused)]

use crate::fiber::Code;
use crate::stash::Stashable;
use crate::state::State;
use gc_arena::lock::GcRefLock;
use gc_arena::{Arena, Collect, Mutation, Rootable};
use std::fmt::Debug;
use std::ops::Deref;

mod callback;
mod fiber;
mod fuel;
mod opcode;
mod stash;
mod state;
mod t;

#[derive(Copy, Clone)]
pub struct Context<'gc> {
    mc: &'gc Mutation<'gc>,
    state: &'gc State<'gc>,
}

impl<'gc> Context<'gc> {
    pub fn stash<S: Stashable<'gc>>(&self, item: S) -> S::Stashed {
        item.stash(&self, self.state.roots)
    }
}

impl<'gc> Deref for Context<'gc> {
    type Target = Mutation<'gc>;

    fn deref(&self) -> &Self::Target {
        self.mc
    }
}

#[derive(Collect, Debug, Copy, Clone)]
#[collect(no_drop)]
pub enum Value<'gc> {
    Int(i64),
    Bool(bool),
    Function(Function<'gc>),
    Container(GcRefLock<'gc, Value<'gc>>),
}

impl<'gc> Value<'gc> {
    fn as_bool(self) -> bool {
        match self {
            Value::Int(v) => v > 0,
            Value::Bool(v) => v,
            _ => true,
        }
    }
}

#[derive(Collect, Debug, Copy, Clone)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Code<'gc>),
}

pub struct IJlVM {
    arena: Arena<Rootable![State<'_>]>,
}

impl IJlVM {
    pub fn new() -> Self {
        let arena = Arena::new(|mc| State::new(mc));

        IJlVM { arena }
    }

    pub fn enter<F: for<'gc> FnOnce(Context<'gc>) -> R, R>(&self, f: F) -> R {
        self.arena.mutate(|mc, state| f(Context { mc, state }))
    }
}

#[cfg(test)]
mod tests {
    use crate::IJlVM;
    use crate::fiber::Executor;
    use std::ops::Deref;

    fn test_simple() {
        let x = IJlVM::new();

        x.enter(|ctx| {
            let exec = Executor::new(&ctx);
        })
    }
}
