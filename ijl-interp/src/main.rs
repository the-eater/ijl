use gc_arena::{Arena, Collect, Gc, Mutation, Rootable};
use ijl_core::parse;
use ijl_core::parser::utils::Spanned;
use ijl_core::parser::{Carrier, Expression, Infix, InfixType, Literal, Statement, TypedSpan};
use std::collections::HashMap;

fn main() {
    let input = r#"(4 ** 4) > 4 && 0"#;
    let Statement::Expression(expr) = parse(input);

    let y = Literal::Number("1337".to_string());

    let ijl = IJl::new();
    ijl.enter(|ctx| {
        let x = ctx.eval(&expr);
        println!("{x:?}");
    })
}

type ValueArena = Arena<Rootable![ValueScopes<'_>]>;

pub struct IJl {
    arena: ValueArena,
}

impl IJl {
    pub fn new() -> Self {
        IJl {
            arena: ValueArena::new(|m| ValueScopes { scopes: vec![] }),
        }
    }

    pub fn enter<F: for<'gc> FnOnce(Context<'gc>) -> R, R>(&self, f: F) -> R {
        self.arena.mutate(|mc, root| {
            f(Context {
                mutation: mc,
                scopes: root,
            })
        })
    }
}

pub struct Context<'gc> {
    mutation: &'gc Mutation<'gc>,
    scopes: &'gc ValueScopes<'gc>,
}

impl<'gc> Context<'gc> {
    pub fn new<T: Collect>(&self, value: T) -> Gc<'gc, T> {
        Gc::new(self.mutation, value)
    }

    pub fn new_string<T: ToString>(&self, value: T) -> Value<'gc> {
        Value::String(self.new(value.to_string()))
    }

    pub fn add_string(&self, left: Value<'gc>, right: Value<'gc>) -> Value<'gc> {
        let (Value::String(l), Value::String(r)) = (left, right) else {
            unreachable!()
        };

        if l.len() == 0 {
            return Value::String(r);
        }

        if r.len() == 0 {
            return Value::String(l);
        }

        self.new_string(format!("{l}{r}"))
    }

    #[inline]
    pub fn eval<E: EvalExpression>(&self, expression: &E) -> EvalResult<'gc> {
        expression.eval(self)
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct ValueScopes<'gc> {
    scopes: Vec<Scope<'gc>>,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct Scope<'gc> {
    variables: HashMap<String, Value<'gc>>,
}

#[derive(Collect)]
#[collect(no_drop)]
#[derive(Debug, Clone, Copy)]
pub enum Value<'gc> {
    None,
    Int(isize),
    Float(f64),
    Bool(bool),
    String(Gc<'gc, String>),
}

impl<'gc> Value<'gc> {
    fn as_bool(&self) -> bool {
        match self {
            Value::None => false,
            Value::Int(i) => *i > 0,
            Value::Float(f) => *f > 0.0,
            Value::Bool(b) => *b,
            Value::String(b) => !b.is_empty()
        }
    }
}

type EvalResult<'gc> = Result<Value<'gc>, Value<'gc>>;

pub trait EvalExpression {
    fn eval<'gc>(&self, context: &Context<'gc>) -> EvalResult<'gc>;
}

impl EvalExpression for Literal {
    fn eval<'gc>(&self, context: &Context<'gc>) -> EvalResult<'gc> {
        match self {
            Literal::String(v) => Ok(Value::String(context.new(v.clone()))),
            Literal::Number(v) => isize::from_str_radix(v, 10)
                .map(Value::Int)
                .map_err(|_| Value::String(context.new("invalid number".to_string()))),
            _ => todo!(),
        }
    }
}

impl<C: Carrier> EvalExpression for Expression<C> {
    fn eval<'gc>(&self, context: &Context<'gc>) -> EvalResult<'gc> {
        match self {
            Expression::Infix(i) => i.eval(context),
            Expression::Literal(l) => l.eval(context),
            Expression::Expression(e) => e.eval(context),
            _ => todo!("eval not implemented yet for {self:?}"),
        }
    }
}

impl<C: Carrier, T: EvalExpression> EvalExpression for TypedSpan<C, T> {
    fn eval<'gc>(&self, context: &Context<'gc>) -> EvalResult<'gc> {
        self.inner.eval(context)
    }
}

impl<C: Carrier> EvalExpression for Infix<C> {
    fn eval<'gc>(&self, context: &Context<'gc>) -> EvalResult<'gc> {
        match &self.infix.inner {
            InfixType::Add => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
                    (Value::String(_), Value::String(_)) => Ok(context.add_string(left, right)),
                    _ => todo!(),
                }
            }
            InfixType::Multiply => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
                    _ => todo!(),
                }
            }
            InfixType::Subtract => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
                    _ => todo!(),
                }
            }
            InfixType::Divide => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l / r)),
                    _ => todo!(),
                }
            }
            InfixType::BitAnd => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l & r)),
                    _ => todo!(),
                }
            }
            InfixType::BitOr => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l | r)),
                    _ => todo!(),
                }
            }
            InfixType::BitXor => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l ^ r)),
                    _ => todo!(),
                }
            }
            InfixType::Modulo => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l % r)),
                    _ => todo!(),
                }
            }
            InfixType::Equals => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l == r)),
                    _ => todo!(),
                }
            }
            InfixType::LessThan => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l < r)),
                    _ => todo!(),
                }
            }
            InfixType::GreaterThan => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l > r)),
                    _ => todo!(),
                }
            }
            InfixType::GreaterThanOrEquals => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l >= r)),
                    _ => todo!(),
                }
            }
            InfixType::LessThanOrEquals => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l <= r)),
                    _ => todo!(),
                }
            }
            InfixType::ShiftLeft => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l << r)),
                    _ => todo!(),
                }
            }
            InfixType::ShiftRight => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l >> r)),
                    _ => todo!(),
                }
            }
            InfixType::NotEquals => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l != r)),
                    _ => todo!(),
                }
            }
            InfixType::Power => {
                let left = self.lhs.eval(context)?;
                let right = self.rhs.eval(context)?;

                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l.pow(r as _))),
                    _ => todo!(),
                }
            }
            InfixType::LogicalAnd => {
                let left = self.lhs.eval(context)?;
                if !left.as_bool() {
                    return Ok(Value::Bool(false))
                }

                let right = self.rhs.eval(context)?;
                Ok(Value::Bool(right.as_bool()))
            },
            InfixType::LogicalOr => {
                let left = self.lhs.eval(context)?;
                if left.as_bool() {
                    return Ok(Value::Bool(true))
                }

                let right = self.rhs.eval(context)?;
                Ok(Value::Bool(right.as_bool()))
            },
            InfixType::As | InfixType::Symbol(_) | InfixType::Word(_) => todo!(),
        }
    }
}
