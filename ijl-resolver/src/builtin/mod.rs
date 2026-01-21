use std::collections::HashMap;
use ijl_core::parser::InfixType;
use crate::{Method, Operator, Property, Shape};
use ijl_type::{StaticType, Type};
use crate::utils::Named;

pub const ANY_TYPE: Type = Type::Static(&StaticType {
    name: "ij.Any",
    arguments: &[]
});

pub const STRING_TYPE: Type = Type::Static(&StaticType {
    name: "ij.String",
    arguments: &[],
});

pub const INT_TYPE: Type = Type::Static(&StaticType {
    name: "ij.Int",
    arguments: &[],
});

pub const NONE_TYPE: Type = Type::Static(&StaticType {
    name: "ij.None",
    arguments: &[]
});

pub fn any_shape() -> Shape {
    Shape {
        name: "ij.Any".to_string(),
        parent_shape: "ij.Any".to_string(),
        properties: Default::default(),
        methods: {
            let mut m =HashMap::new();
            m.insert("toString".to_string(), vec![Method {
                name: "toString".to_string(),
                arguments: vec![],
                return_type: STRING_TYPE,
            }]);
            m
        },
        operator: Default::default(),
        assign_operator: Default::default(),
    }
}

pub fn string_shape() -> Shape {
    Shape {
        name: "ij.String".to_string(),
        parent_shape: "ij.Any".to_string(),
        properties: index(vec![
            Property::new("length", INT_TYPE),
        ]),
        methods: Default::default(),
        operator: Default::default(),
        assign_operator: Default::default(),
    }
}

pub fn int_shape() -> Shape {
    Shape {
        name: "ij.Int".to_string(),
        parent_shape: "ij.Any".to_string(),
        properties: Default::default(),
        methods: Default::default(),
        operator: {
            let mut m = HashMap::new();
            m.insert((InfixType::Add, INT_TYPE), Operator {
                infix_type: InfixType::Add,
                lhs: INT_TYPE,
                rhs: INT_TYPE,
                return_type: INT_TYPE,
            });
            m
        },
        assign_operator: {
            let mut m = HashMap::new();
            m.insert((InfixType::Add, INT_TYPE), Operator {
                infix_type: InfixType::Add,
                lhs: INT_TYPE,
                rhs: INT_TYPE,
                return_type: INT_TYPE,
            });
            m
        },
    }
}

pub fn none_shape() -> Shape {
    Shape {
        name: "ij.None".to_string(),
        parent_shape: "ij.None".to_string(),
        properties: Default::default(),
        methods: Default::default(),
        operator: Default::default(),
        assign_operator: Default::default(),
    }
}

fn index<N: Named>(input: impl IntoIterator<Item = N>) -> HashMap<String, N> {
    input.into_iter().map(|e| (e.name().to_string(), e)).collect::<HashMap<String, N>>()
}