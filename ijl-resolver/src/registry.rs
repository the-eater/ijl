use crate::builtin::{INT_TYPE, STRING_TYPE, any_shape, int_shape, none_shape, string_shape, NONE_TYPE};
use crate::utils::Resolvable;
use crate::{Method, Shape};
use ijl_core::parser::utils::{Carrier, TypedSpan, Untyped};
use ijl_core::parser::{Assign, Block, Call, Expression, FinalizeProgress, Literal, MissingTypes, Property, Statement, Typed};
use ijl_core::parser::{InProgress, IntoProgress};
use std::collections::HashMap;
use std::task::Context;
use thiserror::Error;
use ijl_type::Type;

#[derive(Debug, Clone)]
pub struct Registry {
    shapes: HashMap<String, Shape>,
}

impl Default for Registry {
    fn default() -> Self {
        let mut r = Registry::new();
        r.add_default();
        r
    }
}

impl Registry {
    pub fn new() -> Registry {
        Registry {
            shapes: Default::default(),
        }
    }

    pub fn add_default(&mut self) {
        self.add_shape(any_shape()).unwrap();
        self.add_shape(string_shape()).unwrap();
        self.add_shape(int_shape()).unwrap();
        self.add_shape(none_shape()).unwrap();
    }

    pub fn add_shape(&mut self, shape: Shape) -> Result<(), RegisterError> {
        if shape.parent_shape != shape.name && !self.shapes.contains_key(&shape.parent_shape) {
            return Err(RegisterError::ParentShapeNotFound {
                given_shape: shape.name,
                parent_shape: shape.parent_shape,
            });
        }

        self.shapes.insert(shape.name.clone(), shape);

        Ok(())
    }

    fn get_class_stack(&self, _type: &Type) -> impl Iterator<Item = &Shape> {
        ClassStackIterator {
            registry: self,
            top: self.shapes.get(_type.name()),
        }
    }

    fn get_methods(&self, _type: &Type, name: &str) -> impl Iterator<Item = &Method> {
        self.get_class_stack(_type)
            .flat_map(|v| v.methods.get(name))
            .flat_map(|m| m.iter())
    }

    fn get_property(&self, _type: &Type, name: &str) -> Option<&crate::Property> {
        self.shapes.get(_type.name())?.properties.get(name)
    }

    fn progress_resolve_statement(
        &self,
        input: &mut Statement<InProgress<Typed>>,
        context: &mut Context,
    ) -> Result<StatementResult<Type>, ResolutionError> {
        match input {
            Statement::Expression(expr) => {
                if let Some(v) = expr.resolved_type.clone() {
                    return Ok(StatementResult::QuietReturn(v));
                }
                let _t = self.progress_resolve_expression(&mut expr.inner, context)?;
                expr.resolved_type = Some(_t.clone());
                Ok(StatementResult::QuietReturn(_t))
            }
        }
    }

    fn progress_resolve_block(
        &self,
        input: &mut Block<InProgress<Typed>>,
        context: &mut Context,
    ) -> Result<Type, ResolutionError> {
        if let Some(v) = &input.return_type {
            return Ok(v.clone());
        }

        let mut block_return = None;
        let mut block_return_hard = false;

        for stmt in &mut input.items {
            let _t = self.progress_resolve_statement(&mut stmt.inner, context)?;
            match (_t, &block_return, block_return_hard) {
                (StatementResult::Return(_t), _, false) => {
                    block_return = Some((_t, stmt.span));
                    block_return_hard = true;
                }
                (StatementResult::Return(_t), Some((_ot, _sp)), true) => {
                    if &_t != _ot {
                        todo!();
                    }

                    continue;
                }
                (StatementResult::QuietReturn(v), _, false) => block_return = Some((v, stmt.span)),
                _ => {}
            }
        }

        let ret_type = if let Some((ret, _)) = block_return {
            ret
        } else {
            NONE_TYPE
        };

        input.return_type = Some(ret_type.clone());

        Ok(ret_type)
    }

    fn progress_resolve_typed_span_expression(
        &self,
        input: &mut TypedSpan<InProgress<Typed>, Expression<InProgress<Typed>>>,
        context: &mut Context,
    ) -> Result<Type, ResolutionError> {
        if let Some(v) = input.resolved_type.clone() {
            return Ok(v);
        }

        let _t = self.progress_resolve_expression(&mut input.inner, context)?;
        input.resolved_type = Some(_t.clone());
        Ok(_t)
    }

    fn progress_resolve_expression(
        &self,
        input: &mut Expression<InProgress<Typed>>,
        context: &mut Context,
    ) -> Result<Type, ResolutionError> {
        match input {
            Expression::Ident(_id) => {
                todo!()
            }
            Expression::Expression(expr) => {
                if let Some(v) = expr.resolved_type.clone() {
                    return Ok(v);
                }

                let _t = self.progress_resolve_expression(&mut expr.inner, context)?;
                expr.resolved_type = Some(_t.clone());

                Ok(_t)
            }
            Expression::Block(block) => self.progress_resolve_block(block, context),
            Expression::Literal(Literal::Number(_) | Literal::Hex(_)) => Ok(INT_TYPE.clone()),
            Expression::Infix(infix) => {
                let left = self.progress_resolve_typed_span_expression(&mut infix.lhs, context)?;
                let right = self.progress_resolve_typed_span_expression(&mut infix.rhs, context)?;

                let s = self.shapes.get(left.name()).unwrap();
                if let Some(v) = s.operator.get(&(infix.infix.inner.clone(), right)) {
                    return Ok(v.return_type.clone());
                }

                Err(ResolutionError::TypeNotFound)
            }
            Expression::Assign(Assign { rhs, lhs, modifier }) => {
                let left = self.progress_resolve_typed_span_expression(lhs, context)?;
                let right = self.progress_resolve_typed_span_expression(rhs, context)?;

                match &modifier.inner {
                    None => {
                        if left != right {
                            todo!();
                        }

                        Ok(left)
                    }
                    Some(operator) => {
                        let s = self.shapes.get(left.name()).unwrap();
                        if s.assign_operator.get(&(operator.clone(), right)).is_some() {
                            Ok(left)
                        } else {
                            Err(ResolutionError::TypeNotFound)
                        }
                    }
                }
            }
            Expression::Property(Property { source, name }) => {
                let _t = self.progress_resolve_typed_span_expression(source, context)?;
                let t = self.get_property(&_t, name.as_str()).ok_or_else(|| {
                    ResolutionError::PropertyNotFound {
                        name: name.to_string(),
                        _type: _t,
                    }
                })?;

                Ok(t.return_type.clone())
            }
            Expression::Call(Call {
                source,
                arguments,
                block: _,
            }) => {
                if let Expression::Property(Property { source, name }) = &mut source.inner {
                    // method
                    let t = self.progress_resolve_typed_span_expression(source, context)?;

                    for i in self.get_methods(&t, name.as_str()) {
                        if i.arguments.len() == arguments.len() {
                            return Ok(i.return_type.clone());
                        }
                    }

                    if let Some(_shape) = self.get_property(&t, name.as_str()) {
                        todo!();
                    }

                    todo!()
                } else {
                    // random
                    todo!()
                }
            }
            _ => todo!(),
        }
    }

    fn into_resolved_block<C: Resolvable>(
        &self,
        input: Block<Untyped>,
        context: &mut Context,
    ) -> Result<Block<Typed>, ResolutionError> {
        let mut input: Block<InProgress<Typed>> = input.into_progress::<Typed>();
        self.progress_resolve_block(&mut input, context)?;
        let mut missing_types = Default::default();
        Ok(input.finalize(&mut missing_types).map_err(|_e| missing_types)?)
    }

    fn into_resolved_statement(
        &self,
        input: Statement<Untyped>,
        context: &mut Context,
    ) -> Result<Statement<Typed>, ResolutionError> {
        let mut input: Statement<InProgress<Typed>> = input.into_progress::<Typed>();
        self.progress_resolve_statement(&mut input, context)?;
        let mut missing_types = Default::default();
        Ok(input.finalize(&mut missing_types).map_err(|_e| missing_types)?)
    }

    fn into_resolved(
        &self,
        input: Expression<Untyped>,
        context: &mut Context,
    ) -> Result<Expression<Typed>, ResolutionError> {
        let mut input: Expression<InProgress<Typed>> = input.into_progress::<Typed>();
        self.progress_resolve_expression(&mut input, context)?;
        let mut missing_types = Default::default();
        Ok(input.finalize(&mut missing_types).map_err(|_e| missing_types)?)
    }

    fn resolve<C: Carrier>(&self, input: &Expression<C>) -> Result<Type, ResolutionError> {
        match input {
            Expression::Ident(_) => {
                todo!()
            }
            Expression::Expression(v) => self.resolve(&v.inner),
            Expression::Block(_b) => {
                todo!()
            }
            Expression::Literal(Literal::String(_)) => Ok(STRING_TYPE),
            Expression::Literal(Literal::Number(_) | Literal::Hex(_)) => Ok(INT_TYPE),
            Expression::Infix(infix) => {
                let left = self.resolve(&infix.lhs.inner)?;
                let right = self.resolve(&infix.rhs.inner)?;

                let s = self.shapes.get(left.name()).unwrap();
                if let Some(v) = s.operator.get(&(infix.infix.inner.clone(), right)) {
                    return Ok(v.return_type.clone());
                }

                Err(ResolutionError::TypeNotFound)
            }
            Expression::Assign(ass) => self.resolve(&ass.rhs.inner),
            Expression::ArrayAccess(_) => {
                todo!()
            }
            Expression::Property(Property { source, name }) => {
                let t = self.resolve(&source.inner)?;

                let prop = self.get_property(&t, name.as_str()).ok_or_else(|| {
                    ResolutionError::PropertyNotFound {
                        name: name.inner.clone(),
                        _type: t,
                    }
                })?;

                Ok(prop.return_type.clone())
            }
            Expression::Call(Call {
                source,
                arguments,
                block: _,
            }) => {
                if let Expression::Property(Property { source, name }) = &source.inner {
                    // method
                    let t = self.resolve(&source.inner)?;

                    for i in self.get_methods(&t, name.as_str()) {
                        if i.arguments.len() == arguments.len() {
                            return Ok(i.return_type.clone());
                        }
                    }

                    if let Some(_shape) = self.get_property(&t, name.as_str()) {
                        todo!();
                    }

                    todo!()
                } else {
                    // random
                    todo!()
                }
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ClassStackIterator<'a> {
    registry: &'a Registry,
    top: Option<&'a Shape>,
}

impl<'a> Iterator for ClassStackIterator<'a> {
    type Item = &'a Shape;

    fn next(&mut self) -> Option<Self::Item> {
        let shape = self.top?;

        if shape.name == shape.parent_shape {
            self.top = None;
            return Some(shape);
        }

        self.top = Some(self.registry.shapes.get(&shape.parent_shape).unwrap());
        Some(shape)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ClassNameStackIterator<'a>(ClassStackIterator<'a>);

impl<'a> Iterator for ClassNameStackIterator<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.0.next()?.name.as_str())
    }
}

pub enum StatementResult<T> {
    Return(T),
    NoType,
    QuietReturn(T),
}

#[derive(Debug, Error)]
pub enum RegisterError {
    #[error(
        "tried to register shape {given_shape} but its parent shape {parent_shape} was not found"
    )]
    ParentShapeNotFound {
        given_shape: String,
        parent_shape: String,
    },
}

#[derive(Debug, Error)]
pub enum ResolutionError {
    #[error("type not found")]
    TypeNotFound,
    #[error("property {name} not found on type {_type}")]
    PropertyNotFound { name: String, _type: Type },
    #[error("failed to resolve types: {0}")]
    FailedResolution(#[from] MissingTypes)
}

#[cfg(test)]
mod tests {
    use crate::builtin::{INT_TYPE, STRING_TYPE};
    use crate::registry::Registry;
    use ijl_core::parse;
    use ijl_core::parser::utils::{Spanned, TypedSpan, Untracked, Untyped};
    use ijl_core::parser::{Call, Expression, Literal, Property, Statement};

    #[test]
    fn test_simple() {
        let exp: Expression<Untyped> = Expression::Literal(Literal::String("hello".to_string()));
        let reg = Registry::default();

        assert_eq!(reg.resolve(&exp).unwrap(), STRING_TYPE);
    }

    #[test]
    fn test_method() {
        let expr = parse("1 + 2");
        let Statement::Expression(expr) = expr else {
            panic!();
        };

        let mut reg = Registry::default();
        assert_eq!(reg.resolve(&expr.inner).unwrap(), INT_TYPE);

        let expr = parse("3.toString()");
        let Statement::Expression(expr) = expr else {
            panic!();
        };
        assert_eq!(reg.resolve(&expr.inner).unwrap(), STRING_TYPE);
    }

    #[test]
    fn test_expr() {
        let expr = parse(r#"("hello".length + 3).toString()"#);
        let Statement::Expression(expr) = expr;

        let reg = Registry::default();
        assert_eq!(reg.resolve(&expr.inner).unwrap(), STRING_TYPE);
    }
}
