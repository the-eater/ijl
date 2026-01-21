use std::cmp::{max, Ordering};
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::sync::Arc;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct StaticType {
    pub name: &'static str,
    pub arguments: &'static [StaticType],
}

impl Display for StaticType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if self.arguments.len() > 0 {
            write!(f, "<")?;
            let mut first = true;
            for a in self.arguments {
                if !first {
                    write!(f, ", ")?;
                }

                first = false;

                write!(f, "{a}")?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}

impl PartialEq<DynamicType> for StaticType {
    fn eq(&self, other: &DynamicType) -> bool {
        other.eq(self)
    }
}

impl PartialEq<Type> for StaticType {
    fn eq(&self, other: &Type) -> bool {
        other.eq(self)
    }
}

impl PartialOrd<Type> for StaticType {
    fn partial_cmp(&self, other: &Type) -> Option<Ordering> {
        Some(other.partial_cmp(self)?.reverse())
    }
}

impl PartialOrd<DynamicType> for StaticType {
    fn partial_cmp(&self, other: &DynamicType) -> Option<Ordering> {
        let res = self.name.partial_cmp(&other.name)?;
        if res != Ordering::Equal {
            return Some(res);
        }
        let mut i = 0;
        let m = max(self.arguments.len(), other.arguments.len());
        while i < m {
            if i >= self.arguments.len() {
                return Some(Ordering::Less);
            }

            if i >= other.arguments.len() {
                return Some(Ordering::Greater);
            }

            let cmp = self.arguments[i].partial_cmp(&other.arguments[i])?;
            if cmp != Ordering::Equal {
                return Some(cmp);
            }

            i += 1;
        }

        Some(Ordering::Equal)
    }
}

impl Hash for StaticType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.arguments.hash(state);
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct DynamicType {
    pub name: String,
    pub arguments: Vec<Type>,
}

impl Display for DynamicType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if self.arguments.len() > 0 {
            write!(f, "<")?;
            let mut first = true;
            for a in &self.arguments {
                if !first {
                    write!(f, ", ")?;
                }

                first = false;

                write!(f, "{a}")?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}

impl PartialOrd<StaticType> for DynamicType {
    fn partial_cmp(&self, other: &StaticType) -> Option<Ordering> {
        other.partial_cmp(self).map(|e| e.reverse())
    }
}

impl PartialEq<StaticType> for DynamicType {
    fn eq(&self, other: &StaticType) -> bool {
        self.name == other.name && self.arguments.len() == other.arguments.len() &&
            self.arguments.iter().zip(other.arguments).all(|(l, r)| l == r)
    }
}

impl Hash for DynamicType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.arguments.hash(state);
    }
}


impl PartialEq<StaticType> for Arc<DynamicType> {
    fn eq(&self, other: &StaticType) -> bool {
        self.deref().eq(other)
    }
}

#[derive(Debug, Clone, Ord)]
pub enum Type {
    Static(&'static StaticType),
    Dynamic(Arc<DynamicType>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Static(s) => Display::fmt(s, f),
            Type::Dynamic(s) => Display::fmt(s, f)
        }
    }
}

impl PartialEq<StaticType> for Type {
    fn eq(&self, other: &StaticType) -> bool {
        match self {
            Type::Static(s) => **s == *other,
            Type::Dynamic(s) => **s == *other
        }
    }
}

impl PartialEq<DynamicType> for Type {
    fn eq(&self, other: &DynamicType) -> bool {
        match self {
            Type::Static(s) => **s == *other,
            Type::Dynamic(s) => **s == *other
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Type::Static(lhs), Type::Static(rhs)) => lhs.partial_cmp(rhs),
            (Type::Dynamic(lhs), Type::Dynamic(rhs)) => lhs.partial_cmp(rhs),
            (Type::Static(lhs), Type::Dynamic(rhs)) => (*lhs).partial_cmp(rhs.deref()),
            (Type::Dynamic(lhs), Type::Static(rhs)) => lhs.deref().partial_cmp(*rhs),
        }
    }
}

impl PartialOrd<StaticType> for Type {
    fn partial_cmp(&self, other: &StaticType) -> Option<Ordering> {
        match self {
            Type::Static(s) => (*s).partial_cmp(other),
            Type::Dynamic(s) => s.deref().partial_cmp(other),
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Static(s) => s.hash(state),
            Type::Dynamic(s) => s.hash(state),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Dynamic(l), Type::Dynamic(r)) => l == r,
            (Type::Static(l), Type::Static(r)) => l == r,
            (Type::Static(l), Type::Dynamic(r)) => **l == **r,
            (Type::Dynamic(l), Type::Static(r)) => **l == **r,
        }
    }
}

impl Eq for Type {}

impl From<DynamicType> for Type {
    fn from(value: DynamicType) -> Self {
        Type::Dynamic(Arc::new(value))
    }
}

impl From<&'static StaticType> for Type {
    fn from(value: &'static StaticType) -> Self {
        Type::Static(value)
    }
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Static(s) => s.name,
            Type::Dynamic(s) => s.name.as_str(),
        }
    }
}