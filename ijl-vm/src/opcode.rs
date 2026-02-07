use gc_arena::Collect;
use std::ops::{Add, Deref, DerefMut, Index, IndexMut};

#[derive(Debug, Copy, Clone)]
pub enum InputValue {
    Register(RegisterIndex),
    Literal(i16),
}

impl From<RegisterIndex> for InputValue {
    fn from(value: RegisterIndex) -> Self {
        InputValue::Register(value)
    }
}

impl From<i16> for InputValue {
    fn from(value: i16) -> Self {
        InputValue::Literal(value)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct RegisterIndex(pub u8);

pub fn r(index: u8) -> RegisterIndex {
    return RegisterIndex(index);
}

impl Add<usize> for RegisterIndex {
    type Output = usize;

    fn add(self, rhs: usize) -> Self::Output {
        self.0 as usize + rhs
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FunctionIndex(pub u8);

impl From<u8> for FunctionIndex {
    fn from(value: u8) -> Self {
        FunctionIndex(value)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Constant(pub u16);

#[derive(Debug, Copy, Clone)]
pub struct Constant8(pub u8);

impl From<u8> for Constant8 {
    fn from(value: u8) -> Self {
        Constant8(value)
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[repr(u64)]
#[collect(require_static)]
pub enum Operation {
    Move {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    LoadLiteral {
        dest: RegisterIndex,
        count: u8,
        value: i32,
    },
    LoadFunction {
        dest: RegisterIndex,
        index: FunctionIndex,
    },
    Call {
        index: RegisterIndex,
        arg_count: Constant8,
    },
    SelfCall {
        arg_source: RegisterIndex,
        arg_count: Constant8,
    },
    Return {
        index: RegisterIndex,
        arg_count: Constant8,
    },
    Test {
        value: RegisterIndex,
        is_true: bool,
    },
    Jump {
        offset: i16,
    },
    // Cmp
    Eq {
        dest: RegisterIndex,
        a: InputValue,
        b: InputValue,
    },
    Less {
        dest: RegisterIndex,
        a: InputValue,
        b: InputValue,
    },
    LessEq {
        dest: RegisterIndex,
        a: InputValue,
        b: InputValue,
    },
    // Math
    Add {
        dest: RegisterIndex,
        a: InputValue,
        b: InputValue,
    },
    Sub {
        dest: RegisterIndex,
        a: InputValue,
        b: InputValue,
    },
}

impl Operation {
    pub fn mov<D: Into<RegisterIndex>, S: Into<RegisterIndex>>(dest: D, source: S) -> Self {
        Operation::Move {
            dest: dest.into(),
            source: source.into(),
        }
    }

    pub fn load_literal<D: Into<RegisterIndex>>(dest: D, count: u8, value: i32) -> Self {
        Operation::LoadLiteral {
            dest: dest.into(),
            count,
            value,
        }
    }

    pub fn add<D: Into<RegisterIndex>, A: Into<InputValue>, B: Into<InputValue>>(
        dest: D,
        a: A,
        b: B,
    ) -> Self {
        Operation::Add {
            dest: dest.into(),
            a: a.into(),
            b: b.into(),
        }
    }

    pub fn sub<D: Into<RegisterIndex>, A: Into<InputValue>, B: Into<InputValue>>(
        dest: D,
        a: A,
        b: B,
    ) -> Self {
        Operation::Sub {
            dest: dest.into(),
            a: a.into(),
            b: b.into(),
        }
    }

    pub fn eq<D: Into<RegisterIndex>, A: Into<InputValue>, B: Into<InputValue>>(
        dest: D,
        a: A,
        b: B,
    ) -> Self {
        Operation::Eq {
            dest: dest.into(),
            a: a.into(),
            b: b.into(),
        }
    }

    pub fn lt<D: Into<RegisterIndex>, A: Into<InputValue>, B: Into<InputValue>>(
        dest: D,
        a: A,
        b: B,
    ) -> Self {
        Operation::Less {
            dest: dest.into(),
            a: a.into(),
            b: b.into(),
        }
    }

    pub fn lte<D: Into<RegisterIndex>, A: Into<InputValue>, B: Into<InputValue>>(
        dest: D,
        a: A,
        b: B,
    ) -> Self {
        Operation::LessEq {
            dest: dest.into(),
            a: a.into(),
            b: b.into(),
        }
    }

    pub fn load_function<D: Into<RegisterIndex>, F: Into<FunctionIndex>>(
        dest: D,
        index: F,
    ) -> Operation {
        Operation::LoadFunction {
            dest: dest.into(),
            index: index.into(),
        }
    }

    pub fn call<I: Into<RegisterIndex>, A: Into<Constant8>>(index: I, arg_count: A) -> Self {
        Operation::Call {
            index: index.into(),
            arg_count: arg_count.into(),
        }
    }

    pub fn self_call<I: Into<RegisterIndex>, A: Into<Constant8>>(
        arg_source: I,
        arg_count: A,
    ) -> Self {
        Operation::SelfCall {
            arg_source: arg_source.into(),
            arg_count: arg_count.into(),
        }
    }

    pub fn test<V: Into<RegisterIndex>>(value: V, is_true: bool) -> Self {
        Operation::Test {
            value: value.into(),
            is_true,
        }
    }

    pub fn ret<I: Into<RegisterIndex>, A: Into<Constant8>>(index: I, arg_count: A) -> Self {
        Operation::Return {
            index: index.into(),
            arg_count: arg_count.into(),
        }
    }

    pub fn jmp(offset: i16) -> Self {
        Operation::Jump { offset }
    }
}
