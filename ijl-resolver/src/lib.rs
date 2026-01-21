mod builtin;
mod registry;
mod utils;

use crate::utils::Named;
use ijl_core::parser::InfixType;
use ijl_type::Type;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Registry {
    shapes: HashMap<String, Shape>,
}

#[derive(Debug, Clone)]
pub struct Shape {
    name: String,
    parent_shape: String,
    properties: HashMap<String, Property>,
    methods: HashMap<String, Vec<Method>>,
    operator: HashMap<(InfixType, Type), Operator>,
    assign_operator: HashMap<(InfixType, Type), Operator>,
}

impl Named for Shape {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, Clone)]
pub struct Property {
    name: String,
    return_type: Type,
}

impl Property {
    pub fn new(name: impl ToString, return_type: impl Into<Type>) -> Property {
        Property {
            name: name.to_string(),
            return_type: return_type.into(),
        }
    }
}

impl Named for Property {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    name: String,
    arguments: Vec<Type>,
    return_type: Type,
}

impl Named for Method {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, Clone)]
pub struct Operator {
    infix_type: InfixType,
    lhs: Type,
    rhs: Type,
    return_type: Type,
}

#[cfg(test)]
pub mod tests {
    use crate::builtin::ANY_TYPE;
    use ijl_type::{DynamicType, Type};
    use std::collections::HashMap;
    use std::sync::Arc;

    #[test]
    fn test_dynamic_eq() {
        let lhs = ANY_TYPE;
        let rhs = Type::Dynamic(Arc::new(DynamicType {
            name: "ij.Any".to_string(),
            arguments: vec![],
        }));

        assert_eq!(lhs, rhs);
        assert_eq!(rhs, lhs);

        let mut m = HashMap::new();
        m.insert(lhs.clone(), "hello");
        assert_eq!(Some(&"hello"), m.get(&rhs));
    }
}
