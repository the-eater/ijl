use crate::parser::utils::{Carrier, InProgress, TypedSpan, Untyped};
use crate::parser::xij::{XIJArgument, XIJElement, XIJFragment, XIJNode, XIJProperty};
use crate::parser::{
    ArrayAccess, Assign, Block, Call, CallArgument, CallArgumentType, CallArgumentValue,
    Expression, Infix, Property, Statement, Typed, Unary,
};
use chumsky::prelude::SimpleSpan;
use thiserror::Error;

pub trait IntoProgressCarrier: Carrier<Span = SimpleSpan> {}

impl<C: Carrier<Span = SimpleSpan>> IntoProgressCarrier for C {}

pub trait IntoProgress {
    type Output<C: IntoProgressCarrier>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C>;
}

pub trait FinalizeProgress {
    type Output;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()>;
}

#[derive(Debug, Default, Clone, Error)]
#[error("unable to resolve all types")]
pub struct MissingTypes {
    items: Vec<()>,
}

impl IntoProgress for Statement<Untyped> {
    type Output<C: IntoProgressCarrier> = Statement<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        match self {
            Statement::Expression(expr) => Statement::Expression(expr.into_progress()),
        }
    }
}

impl<I: IntoProgress> IntoProgress for Vec<I> {
    type Output<C: IntoProgressCarrier> = Vec<I::Output<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        self.into_iter().map(|e| e.into_progress()).collect()
    }
}

impl<I: IntoProgress> IntoProgress for Box<I> {
    type Output<C: IntoProgressCarrier> = Box<I::Output<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        Box::new((*self).into_progress())
    }
}

impl IntoProgress for Expression<Untyped> {
    type Output<C: IntoProgressCarrier> = Expression<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        match self {
            Expression::Ident(id) => Expression::Ident(id),
            Expression::Expression(expr) => Expression::Expression(expr.into_progress()),
            Expression::Block(block) => Expression::Block(block.into_progress()),
            Expression::Literal(lit) => Expression::Literal(lit),
            Expression::Infix(infix) => Expression::Infix(infix.into_progress()),
            Expression::Assign(assign) => Expression::Assign(assign.into_progress()),
            Expression::ArrayAccess(array_access) => {
                Expression::ArrayAccess(array_access.into_progress())
            }
            Expression::Property(property) => Expression::Property(property.into_progress()),
            Expression::Call(call) => Expression::Call(call.into_progress()),
            Expression::Unary(unary) => Expression::Unary(unary.into_progress()),
            Expression::XIJFragment(xij_fragment) => {
                Expression::XIJFragment(xij_fragment.into_progress())
            }
            Expression::XIJElement(xij_element) => {
                Expression::XIJElement(xij_element.into_progress())
            }
        }
    }
}

impl<P: IntoProgress> IntoProgress for TypedSpan<Untyped, P> {
    type Output<C: IntoProgressCarrier> = TypedSpan<InProgress<C>, P::Output<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        TypedSpan {
            resolved_type: None,
            span: self.span,
            inner: self.inner.into_progress(),
        }
    }
}

impl IntoProgress for Block<Untyped> {
    type Output<C: IntoProgressCarrier> = Block<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        Block {
            return_type: None,
            yield_type: None,
            items: self.items.into_iter().map(|e| e.into_progress()).collect(),
        }
    }
}

impl IntoProgress for Infix<Untyped> {
    type Output<C: IntoProgressCarrier> = Infix<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        Infix {
            lhs: self.lhs.into_progress(),
            rhs: self.rhs.into_progress(),
            infix: self.infix,
        }
    }
}

impl IntoProgress for Assign<Untyped> {
    type Output<C: IntoProgressCarrier> = Assign<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        Assign {
            lhs: self.lhs.into_progress(),
            rhs: self.rhs.into_progress(),
            modifier: self.modifier,
        }
    }
}

impl IntoProgress for ArrayAccess<Untyped> {
    type Output<C: IntoProgressCarrier> = ArrayAccess<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        ArrayAccess {
            source: self.source.into_progress(),
            indexes: self.indexes.into_progress(),
        }
    }
}

impl IntoProgress for Property<Untyped> {
    type Output<C: IntoProgressCarrier> = Property<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        Property {
            source: self.source.into_progress(),
            name: self.name,
        }
    }
}

impl IntoProgress for Call<Untyped> {
    type Output<C: IntoProgressCarrier> = Call<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        Call {
            source: self.source.into_progress(),
            arguments: self
                .arguments
                .into_iter()
                .map(|m| m.into_progress())
                .collect(),
            block: self.block.map(|e| e.into_progress()),
        }
    }
}

impl IntoProgress for CallArgument<Untyped> {
    type Output<C: IntoProgressCarrier> = CallArgument<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        CallArgument {
            r#type: match self.r#type {
                CallArgumentType::Named(n) => CallArgumentType::Named(n),
                CallArgumentType::Spread(n) => CallArgumentType::Spread(n),
                CallArgumentType::Positional => CallArgumentType::Positional,
            },
            value: match self.value {
                CallArgumentValue::Template => CallArgumentValue::Template,
                CallArgumentValue::Value(v) => CallArgumentValue::Value(v.into_progress()),
            },
        }
    }
}

impl IntoProgress for Unary<Untyped> {
    type Output<C: IntoProgressCarrier> = Unary<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        Unary {
            symbol: self.symbol,
            source: self.source.into_progress(),
        }
    }
}

impl IntoProgress for XIJFragment<Untyped> {
    type Output<C: IntoProgressCarrier> = XIJFragment<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        XIJFragment {
            nodes: self.nodes.into_progress(),
        }
    }
}

impl IntoProgress for XIJElement<Untyped> {
    type Output<C: IntoProgressCarrier> = XIJElement<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        XIJElement {
            name: self.name,
            properties: self.properties.into_progress(),
            arguments: self.arguments.into_progress(),
            nodes: self.nodes.into_progress(),
            self_closed: self.self_closed,
        }
    }
}

impl IntoProgress for XIJNode<Untyped> {
    type Output<C: IntoProgressCarrier> = XIJNode<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        match self {
            XIJNode::Element(element) => XIJNode::Element(element.into_progress()),
            XIJNode::Fragment(fragment) => XIJNode::Fragment(fragment.into_progress()),
            XIJNode::Text(text) => XIJNode::Text(text),
            XIJNode::Expression(expr) => XIJNode::Expression(expr.into_progress()),
        }
    }
}

impl IntoProgress for XIJProperty<Untyped> {
    type Output<C: IntoProgressCarrier> = XIJProperty<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        XIJProperty {
            property_type: self.property_type,
            name: self.name,
            value: self.value.into_progress(),
        }
    }
}

impl IntoProgress for XIJArgument<Untyped> {
    type Output<C: IntoProgressCarrier> = XIJArgument<InProgress<C>>;

    fn into_progress<C: IntoProgressCarrier>(self) -> Self::Output<C> {
        match self {
            XIJArgument::Spread { spread, expression } => XIJArgument::Spread {
                spread,
                expression: expression.into_progress(),
            },
            XIJArgument::Argument(args) => XIJArgument::Argument(args.into_progress()),
        }
    }
}

impl FinalizeProgress for Statement<InProgress<Typed>> {
    type Output = Statement<Typed>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        match self {
            Statement::Expression(expr) => Ok(Statement::Expression(expr.finalize(missing_types)?)),
        }
    }
}

impl<F: FinalizeProgress> FinalizeProgress for Box<F> {
    type Output = Box<F::Output>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        Ok(Box::new((*self).finalize(missing_types)?))
    }
}

impl<F: FinalizeProgress> FinalizeProgress for Option<F> {
    type Output = Option<F::Output>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        match self {
            None => Ok(None),
            Some(f) => Ok(Some(f.finalize(missing_types)?))
        }
    }
}

impl<F: FinalizeProgress> FinalizeProgress for Vec<F> {
    type Output = Vec<F::Output>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        let mut output = vec![];
        let mut failed = false;
        for i in self.into_iter() {
            if let Ok(v) = i.finalize(missing_types) {
                output.push(v);
            } else {
                failed = true;
            }
        }

        if failed {
            return Err(())
        }

        Ok(output)
    }
}

impl<F: FinalizeProgress> FinalizeProgress for TypedSpan<InProgress<Typed>, F> {
    type Output = TypedSpan<Typed, F::Output>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        let output = self.inner.finalize(missing_types)?;
        if let Some(v) = self.resolved_type {
            Ok(TypedSpan {
                resolved_type: v,
                span: self.span,
                inner: output,
            })
        } else {
            missing_types.items.push(());
            Err(())
        }
    }
}

impl FinalizeProgress for Expression<InProgress<Typed>> {
    type Output = Expression<Typed>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        Ok(match self {
            Expression::Ident(id) => Expression::Ident(id),
            Expression::Expression(expr) => Expression::Expression(expr.finalize(missing_types)?),
            Expression::Block(b) => {
                Expression::Block(b.finalize(missing_types)?)
            }
            Expression::Literal(l) => Expression::Literal(l),
            Expression::Infix(i) => Expression::Infix(i.finalize(missing_types)?),
            Expression::Assign(a) => Expression::Assign(a.finalize(missing_types)?),
            Expression::ArrayAccess(aa) => Expression::ArrayAccess(aa.finalize(missing_types)?),
            Expression::Property(p) => Expression::Property(p.finalize(missing_types)?),
            Expression::Call(c) => Expression::Call(c.finalize(missing_types)?),
            Expression::Unary(u) => Expression::Unary(u.finalize(missing_types)?),
            Expression::XIJFragment(_) => todo!(),
            Expression::XIJElement(_) => todo!(),
        })
    }
}

impl FinalizeProgress for Block<InProgress<Typed>> {
    type Output = Block<Typed>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        let return_type = self.return_type.unwrap();
        let yield_type = self.yield_type.unwrap();
        Ok(Block {
            return_type,
            yield_type,
            items: self.items.finalize(missing_types)?,
        })
    }
}

impl FinalizeProgress for Infix<InProgress<Typed>> {
    type Output = Infix<Typed>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        let left = self.lhs.finalize(missing_types);
        let right = self.rhs.finalize(missing_types);

        Ok(Infix {
            lhs: left?,
            rhs: right?,
            infix: self.infix,
        })
    }
}

impl FinalizeProgress for Assign<InProgress<Typed>> {
    type Output = Assign<Typed>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        let left = self.lhs.finalize(missing_types);
        let right = self.rhs.finalize(missing_types);

        Ok(Assign {
            lhs: left?,
            rhs: right?,
            modifier: self.modifier,
        })
    }
}

impl FinalizeProgress for ArrayAccess<InProgress<Typed>> {
    type Output = ArrayAccess<Typed>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        let v = self.indexes.finalize(missing_types);

        Ok(ArrayAccess {
            source: self.source.finalize(missing_types)?,
            indexes: v?,
        })
    }
}

impl FinalizeProgress for Property<InProgress<Typed>> {
    type Output = Property<Typed>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
       Ok(Property {
           source: self.source.finalize(missing_types)?,
           name: self.name,
       })
    }
}

impl FinalizeProgress for Call<InProgress<Typed>> {
    type Output = Call<Typed>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        let source = self.source.finalize(missing_types);
        let arguments = self.arguments.finalize(missing_types);
        let block = self.block.finalize(missing_types);

        Ok(Call {
            source: source?,
            arguments: arguments?,
            block: block?,
        })
    }
}

impl FinalizeProgress for CallArgument<InProgress<Typed>> {
    type Output = CallArgument<Typed>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        let v = match self.value {
            CallArgumentValue::Template => CallArgumentValue::Template,
            CallArgumentValue::Value(v) => CallArgumentValue::Value(v.finalize(missing_types)?)
        };

        let r = match self.r#type {
            CallArgumentType::Named(v) => CallArgumentType::Named(v),
            CallArgumentType::Spread(v) => CallArgumentType::Spread(v),
            CallArgumentType::Positional => CallArgumentType::Positional,
        };

        Ok(CallArgument {
            r#type: r,
            value: v,
        })
    }
}

impl FinalizeProgress for Unary<InProgress<Typed>> {
    type Output = Unary<Typed>;

    fn finalize(self, missing_types: &mut MissingTypes) -> Result<Self::Output, ()> {
        Ok(Unary {
            symbol: self.symbol,
            source: self.source.finalize(missing_types)?,
        })
    }
}

