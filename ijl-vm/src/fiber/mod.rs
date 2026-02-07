use crate::fiber::vm::{FiberFrame, run_vm};
use crate::fuel::Fuel;
use crate::opcode::Operation;
use crate::t::{MetricBox, MetricNew, MetricVec};
use crate::{Context, Value, mvec};
use gc_arena::allocator_api::MetricsAlloc;
use gc_arena::lock::{GcRefLock, RefLock};
use gc_arena::{Collect, Gc, Mutation};
use std::ops::Deref;

mod vm;

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct Executor<'gc>(GcRefLock<'gc, ExecutorState<'gc>>);

impl<'gc> Executor<'gc> {
    const FUEL_PER_INSTRUCTION: i32 = 5;

    pub fn new(mc: &Mutation<'gc>) {
        let mut fiber_stack = mvec![mc; Fiber::new(mc)];
        Executor(Gc::new(mc, RefLock::new(ExecutorState { fiber_stack })));
    }

    pub fn run(&self, ctx: Context<'gc>, fuel: &mut Fuel) -> Result<bool, ()> {
        let state = self.0.unlock(&ctx);
        let state_m = state.borrow_mut();
        let top_fiber = *state_m.fiber_stack.last().unwrap();

        let f = top_fiber.0.unlock(&ctx);
        let mut y = f.borrow_mut();

        while let Some(last_frame) = y.frames.last() {
            match last_frame {
                Frame::Waiting => {}
                Frame::Code {
                    pc,
                    stack_offset,
                    code,
                    ..
                } => {
                    let so = *stack_offset;
                    let fiber_frame = FiberFrame {
                        fiber: top_fiber,
                        fiber_state: &mut *y,
                    };
                    assert!(
                        so + fiber_frame.code().0.stack_size == fiber_frame.fiber_state.stack.len()
                    );
                    let v = run_vm(ctx, fiber_frame, 64).unwrap();
                    fuel.consume(v as i32 * Self::FUEL_PER_INSTRUCTION);
                    if !fuel.should_continue() {
                        return Ok(false);
                    }
                }
                Frame::Start(code) => {
                    let code = *code;
                    debug_assert!(y.frames.len() == 1);
                    y.frames[0] = Frame::Code {
                        code,
                        pc: 0,
                        stack_offset: 0,
                        stack_return: 0,
                        stack_end: code.stack_size,
                    };
                    y.stack.resize(code.0.stack_size, Value::Int(0));
                }
                Frame::Yielded => {}
            }
        }

        Ok(true)
    }
}

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct ExecutorState<'gc> {
    fiber_stack: MetricVec<'gc, Fiber<'gc>>,
}

impl<'gc> ExecutorState<'gc> {}

#[derive(Collect, Debug, Copy, Clone)]
#[collect(no_drop)]
pub struct Fiber<'gc>(GcRefLock<'gc, FiberState<'gc>>);

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct FiberState<'gc> {
    frames: MetricVec<'gc, Frame<'gc>>,
    stack: MetricVec<'gc, Value<'gc>>,
}

impl<'gc> Fiber<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Fiber<'gc> {
        let frames = mvec![mc; Frame::Waiting];
        let mut stack = mvec![mc];
        stack.reserve(48);

        let state = FiberState { frames, stack };

        Fiber(Gc::new(mc, RefLock::new(state)))
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub enum Frame<'gc> {
    Waiting,
    Code {
        code: Code<'gc>,
        pc: usize,
        stack_offset: usize,
        stack_end: usize,
        stack_return: usize,

    },
    Start(Code<'gc>),
    Yielded,
}

#[derive(Collect, Copy, Clone, Debug)]
#[collect(no_drop)]
pub struct Code<'gc>(Gc<'gc, CodeBlock<'gc>>);

impl<'gc> Deref for Code<'gc> {
    type Target = CodeBlock<'gc>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct CodeBlock<'gc> {
    opcodes: MetricBox<'gc, [Operation]>,
    functions: MetricBox<'gc, [Code<'gc>]>,
    stack_size: usize,
}

#[cfg(test)]
mod tests {
    use crate::fiber::{Code, CodeBlock, Executor, ExecutorState, Fiber, FiberState, Frame};
    use crate::fuel::Fuel;
    use crate::opcode::{r, Operation};
    use crate::t::{MetricBox, MetricVec};
    use crate::{IJlVM, mvec};
    use gc_arena::allocator_api::MetricsAlloc;
    use gc_arena::lock::{GcRefLock, RefLock};
    use gc_arena::{Arena, Gc, Rootable};
    use std::cell::Ref;

    #[test]
    pub fn test_fibonacci() {
        let ijlvm = IJlVM::new();
        ijlvm.enter(|ctx| {
            let fibonacci = mvec![&ctx;
                Operation::lte(r(1), r(0), 1),
                Operation::test(r(1), false),
                Operation::jmp(2),
                Operation::load_literal(r(2), 1, 0),
                Operation::ret(r(2), 1),
                Operation::eq(r(1), r(0), 2),
                Operation::test(r(4), false),
                Operation::jmp(2),
                Operation::load_literal(r(2), 1, 1),
                Operation::ret(r(2), 1),
                Operation::sub(r(2), r(0), 1),
                Operation::self_call(r(2), 1),
                Operation::sub(r(3), r(0), 2),
                Operation::self_call(r(3), 1),
                Operation::add(r(0), r(2), r(3)),
                Operation::ret(r(0), 1)
            ]
                .into_boxed_slice();
            let code = CodeBlock {
                opcodes: fibonacci,
                functions: mvec![&ctx].into_boxed_slice(),
                stack_size: 6,
            };

            let prog_2 = mvec![&ctx;
                Operation::load_function(r(1), 0),
                Operation::load_literal(r(2), 1, 2),
                Operation::call(r(1), 1),
                Operation::ret(r(1), 1)
            ]
            .into_boxed_slice();
            let mut progs = mvec![&ctx; Code(Gc::new(&ctx, code))];
            let code_2 = CodeBlock {
                opcodes: prog_2,
                functions: progs.into_boxed_slice(),
                stack_size: 3,
            };

            let frame = Frame::Start(Code(Gc::new(&ctx, code_2)));

            let mut fiber_state = FiberState {
                frames: mvec![&ctx; frame],
                stack: mvec![&ctx],
            };

            let mut executor_state = ExecutorState {
                fiber_stack: mvec![&ctx; Fiber(GcRefLock::new(&ctx, RefLock::new(fiber_state)))]
            };

            let ex = Executor(Gc::new(&ctx, RefLock::new(executor_state)));

            ex.run(ctx, &mut Fuel::unlimited()).unwrap();
            let x = ex.0.unlock(&ctx).borrow().fiber_stack[0]
                .0
                .unlock(&ctx)
                .borrow()
                .stack[0];
            println!("{x:?}");
        });
    }
}
