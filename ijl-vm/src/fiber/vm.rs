use crate::fiber::{Code, Fiber, FiberState, Frame};
use crate::opcode::{FunctionIndex, InputValue, Operation, RegisterIndex};
use crate::{Context, Function, Value};
use std::ops::{Index, IndexMut};

pub struct FiberFrame<'gc, 'a> {
    pub(super) fiber: Fiber<'gc>,
    pub(super) fiber_state: &'a mut FiberState<'gc>,
}

impl<'gc, 'a> FiberFrame<'gc, 'a> {
    pub fn registers<'b>(&'b mut self) -> Registers<'gc, 'b> {
        let Frame::Code {
            stack_offset, pc, ..
        } = self.fiber_state.frames.last_mut().unwrap()
        else {
            unreachable!();
        };

        let (top_stack, window) = self.fiber_state.stack[..].split_at_mut(*stack_offset);

        Registers {
            pc,
            window,
            top_stack,
            stack_offset: *stack_offset,
        }
    }

    pub fn code(&self) -> Code<'gc> {
        let Frame::Code { code, .. } = self.fiber_state.frames.last().unwrap() else {
            unreachable!();
        };
        *code
    }

    pub fn call_self(self, arg_index: RegisterIndex, arg_count: usize) {
        let Frame::Code {
            stack_offset,
            stack_end,
            code,
            ..
        } = self.fiber_state.frames.last().unwrap()
        else {
            unreachable!();
        };

        let arg_offset = stack_offset + arg_index.0 as usize;
        let stack = code.stack_size;
        let required_len = stack_end + stack;
        self.fiber_state.stack.resize(required_len, Value::Int(0));
        self.fiber_state
            .stack
            .copy_within(arg_offset..arg_offset + arg_count, *stack_end);

        self.fiber_state.frames.push(Frame::Code {
            code: *code,
            pc: 0,
            stack_offset: *stack_end,
            stack_end: required_len,
            stack_return: arg_offset,
        });
    }

    pub fn call_tail_self(self, arg_index: RegisterIndex, arg_count: usize) {
        let Frame::Code {
            stack_offset,
            stack_end,
            code,
            stack_return,
            ..
        } = self.fiber_state.frames.pop().unwrap()
        else {
            unreachable!();
        };

        let arg_offset = arg_index.0 as usize + stack_offset;
        self.fiber_state
            .stack
            .copy_within(arg_offset..arg_offset + arg_count, stack_offset);

        self.fiber_state.frames.push(Frame::Code {
            code,
            pc: 0,
            stack_offset,
            stack_end,
            stack_return,
        });
    }

    pub fn call_function(self, index: RegisterIndex, arg_count: usize) {
        let Frame::Code {
            stack_offset,
            stack_end,
            ..
        } = self.fiber_state.frames.last().unwrap()
        else {
            unreachable!();
        };

        let call_offset = stack_offset + index.0 as usize;
        let arg_offset = call_offset + 1;
        let Value::Function(v) = self.fiber_state.stack[call_offset] else {
            todo!(
                "only allows function instead found {:?}",
                self.fiber_state.stack[call_offset]
            );
        };

        match v {
            Function::Closure(code) => {
                let stack = code.0.stack_size;
                let required_len = stack_end + stack;
                self.fiber_state.stack.resize(required_len, Value::Int(0));
                self.fiber_state
                    .stack
                    .copy_within(arg_offset..arg_offset + arg_count, *stack_end);

                self.fiber_state.frames.push(Frame::Code {
                    code,
                    pc: 0,
                    stack_offset: *stack_end,
                    stack_end: required_len,
                    stack_return: call_offset,
                });
            }
        }
    }

    pub fn do_return(self, index: RegisterIndex, arg_count: usize) {
        let Some(Frame::Code {
            stack_offset,
            stack_return,
            code,
            ..
        }) = self.fiber_state.frames.pop()
        else {
            unreachable!();
        };

        let start = stack_offset + index.0 as usize;
        self.fiber_state
            .stack
            .copy_within(start..start + arg_count, stack_return);

        if self.fiber_state.frames.len() > 0 {
            self.fiber_state.stack.truncate(stack_offset);
        }
    }
}

pub struct Registers<'gc, 'a> {
    pc: &'a mut usize,
    window: &'a mut [Value<'gc>],
    top_stack: &'a mut [Value<'gc>],
    stack_offset: usize,
}

impl<'gc, 'a> Index<RegisterIndex> for Registers<'gc, 'a> {
    type Output = Value<'gc>;

    fn index(&self, index: RegisterIndex) -> &Self::Output {
        &self.window[index.0 as usize]
    }
}

impl<'gc, 'a> IndexMut<RegisterIndex> for Registers<'gc, 'a> {
    fn index_mut(&mut self, index: RegisterIndex) -> &mut Self::Output {
        &mut self.window[index.0 as usize]
    }
}


pub fn run_vm<'gc>(
    ctx: Context<'gc>,
    mut frame: FiberFrame,
    max_instructions: u32,
) -> Result<u32, ()> {
    let code = frame.code();
    let mut registers = frame.registers();
    let mut instructions_run = 0;

    fn gip<'gc, 'a>(i: InputValue, r: &Registers<'gc, 'a>) -> Value<'gc> {
        match i {
            InputValue::Register(ri) => r[ri],
            InputValue::Literal(v) => Value::Int(v as _),
        }
    }

    loop {
        let op = code.0.opcodes[*registers.pc];
        *registers.pc += 1;

        match op {
            Operation::Move { dest, source } => {
                registers[dest] = registers[source];
                instructions_run += 1;
            }
            Operation::LoadLiteral { dest, value, count } => {
                let mut count = count as usize - 1;
                loop {
                    registers.window[dest + count] = Value::Int(value as _);

                    if count == 0 {
                        break;
                    }

                    count -= 1;
                }

                instructions_run += 1;
            }
            Operation::Add { dest, a, b } => {
                match (gip(a, &registers), gip(b, &registers)) {
                    (Value::Int(a), Value::Int(b)) => {
                        registers[dest] = Value::Int(a + b);
                    }

                    _ => todo!(),
                }

                instructions_run += 1
            }
            Operation::LoadFunction { dest, index } => {
                let func = code.0.functions[index.0 as usize];
                registers[dest] = Value::Function(Function::Closure(func));
                instructions_run += 1;
            }
            Operation::Call { index, arg_count } => {
                frame.call_function(index, arg_count.0 as usize);
                break;
            }
            Operation::Return { index, arg_count } => {
                frame.do_return(index, arg_count.0 as usize);
                break;
            }
            Operation::SelfCall {
                arg_count,
                arg_source,
            } => {
                frame.call_self(arg_source, arg_count.0 as usize);
                break;
            }
            Operation::Test { value, is_true } => {
                if registers[value].as_bool() != is_true {
                    *registers.pc += 1;
                }

                instructions_run += 1;
            }
            Operation::Jump { offset } => {
                if offset > 0 {
                    *registers.pc += offset.abs() as usize;
                }
                if offset < 0 {
                    *registers.pc -= offset.abs() as usize;
                }
                instructions_run += 1;
            }
            Operation::Eq { dest, a, b } => {
                let res = match (gip(a, &registers), gip(b, &registers)) {
                    (Value::Int(a), Value::Int(b)) => a == b,
                    (Value::Bool(a), Value::Bool(b)) => a == b,
                    _ => false,
                };

                registers[dest] = Value::Bool(res);
            }
            Operation::Less { dest, a, b } => {
                let res = match (gip(a, &registers), gip(b, &registers)) {
                    (Value::Int(a), Value::Int(b)) => a < b,
                    (Value::Bool(a), Value::Bool(b)) => a < b,
                    _ => false,
                };

                registers[dest] = Value::Bool(res);
            }
            Operation::LessEq { dest, a, b } => {
                let res = match (gip(a, &registers), gip(b, &registers)) {
                    (Value::Int(a), Value::Int(b)) => a <= b,
                    (Value::Bool(a), Value::Bool(b)) => a <= b,
                    _ => false,
                };

                registers[dest] = Value::Bool(res);
            }
            Operation::Sub { dest, a, b } => {
                match  (gip(a, &registers), gip(b, &registers)) {
                    (Value::Int(a), Value::Int(b)) => {
                        registers[dest] = Value::Int(a - b);
                    }

                    _ => todo!(),
                }

                instructions_run += 1
            }
        }

        if max_instructions <= instructions_run {
            break;
        }
    }

    Ok(instructions_run)
}
