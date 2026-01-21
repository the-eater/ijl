use crate::lexer::Token;
use crate::lexer::Token::XIJFragmentClose;
use crate::parser::{Expression, Literal};
use crate::parser::utils::{Carrier, Spanned, TypedSpan, Untyped, sp, ts};
use chumsky::error::Rich;
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use chumsky::{Parser, extra};

#[derive(Debug, Clone)]
pub enum XIJNode<C: Carrier> {
    Element(TypedSpan<C, XIJElement<C>>),
    Fragment(TypedSpan<C, XIJFragment<C>>),
    Text(Spanned<C, String>),
    Expression(TypedSpan<C, Expression<C>>),
}

#[derive(Debug, Clone)]
pub struct XIJElement<C: Carrier> {
    pub name: Spanned<C, String>,
    pub properties: Vec<XIJProperty<C>>,
    pub arguments: Vec<XIJArgument<C>>,
    pub nodes: Vec<XIJNode<C>>,
    pub self_closed: bool,
}

#[derive(Debug, Clone)]
pub struct XIJFragment<C: Carrier> {
    pub nodes: Vec<XIJNode<C>>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum XIJPropertyType {
    Assign,
    Event,
    Bind,
}

#[derive(Debug, Clone)]
pub enum XIJArgument<C: Carrier> {
    Spread {
        spread: C::Span,
        expression: TypedSpan<C, Expression<C>>,
    },
    Argument(TypedSpan<C, Expression<C>>),
}

#[derive(Debug, Clone)]
pub struct XIJProperty<C: Carrier> {
    pub property_type: Spanned<C, XIJPropertyType>,
    pub name: Spanned<C, String>,
    pub value: TypedSpan<C, Expression<C>>,
}

#[derive(Debug, Clone)]
pub enum XIJElementItem<C: Carrier> {
    Property(XIJProperty<C>),
    Argument(XIJArgument<C>),
}

pub(crate) fn parser<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<
        'tokens,
        I,
        TypedSpan<Untyped, Expression<Untyped>>,
        extra::Err<Rich<'tokens, Token<'src>>>,
    > + Clone + 'tokens,
) -> impl Parser<
    'tokens,
    I,
    TypedSpan<Untyped, Expression<Untyped>>,
    extra::Err<Rich<'tokens, Token<'src>>>,
> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token=Token<'src>, Span=SimpleSpan>,
{
    let mut fragment_decl = Recursive::declare();
    let mut element_decl = Recursive::declare();
    let expr_decl = expr
        .clone()
        .delimited_by(just(Token::BlockOpen), just(Token::BlockClose))
        .labelled("xml expression");
    let text_fragment = select! { Token::XIJText(txt) => txt.to_string() }
        .labelled("xml text")
        .map_with(sp);

    let xij_content = choice((
        text_fragment.map(XIJNode::Text),
        expr_decl.map(XIJNode::Expression),
        element_decl.clone().map(XIJNode::Element),
        fragment_decl.clone().map(XIJNode::Fragment),
    ));

    let xij_nodes = xij_content.clone().repeated().collect::<Vec<_>>();

    fragment_decl.define(
        xij_nodes
            .clone()
            .map_with(|nodes, x| ts(XIJFragment { nodes }, x))
            .delimited_by(just(Token::XIJFragmentOpen), just(XIJFragmentClose)),
    );

    let xml_start =
        select! { Token::XIJElementStart(tag) => tag.to_string() }.labelled("xml element start");

    let lit_num = select! { Token::Number(v) => v.to_string() }.labelled("number value");
    let str = select! { Token::String(v) => v.to_string() }.labelled("string value");
    let xml_key_value_value = expr.clone()
        .delimited_by(just(Token::BlockOpen), just(Token::BlockClose))
        .or(lit_num.map_with(|v, x| ts(Expression::Literal(Literal::Number(v.to_string())), x)))
        .or(str.map_with(|v, x| ts(Expression::Literal(Literal::String(v.to_string())), x)));

    let xml_element_value = just(Token::Spread)
        .spanned()
        .or_not()
        .then(expr.clone())
        .delimited_by(just(Token::BlockOpen), just(Token::BlockClose))
        .map_with(|(spr, expr), _| {
            if let Some(spr) = spr {
                XIJArgument::Spread {
                    spread: spr.span,
                    expression: expr,
                }
            } else {
                XIJArgument::Argument(expr)
            }
        })
        .or(lit_num.map_with(|v, x| ts(Expression::Literal(Literal::Number(v.to_string())), x))
            .or(str.map_with(|v, x| ts(Expression::Literal(Literal::String(v.to_string())), x))).map(XIJArgument::Argument));


    let name = select! { Token::Ident(id) => id.to_string() }.labelled("name");

    let xml_key_value = name.map_with(sp).then(just(Token::EqualSign).map_with(sp)).then(xml_key_value_value).map(|((lhs, b), rhs)| {
        XIJProperty::<Untyped> {
            property_type: Spanned::<Untyped, _> {
                inner: XIJPropertyType::Assign,
                span: b.span,
            },
            name: lhs,
            value: rhs,
        }
    });

    let xml_element_item = xml_key_value.map(XIJElementItem::Property).or(xml_element_value.map(XIJElementItem::Argument));

    let xij_close_name =
        select! { Token::XIJElementClose(c) => c.to_string() }.labelled("xml close tag");

    let xml_end = just(Token::XIJElementSelfClose).map(|_| (true, vec![]))
        .or(just(Token::XIJElementEnd).ignore_then(xij_nodes).then_ignore(xij_close_name).map(|nodes| (false, nodes)));

    element_decl.define(
        xml_start
            .spanned()
            .then(xml_element_item.repeated().collect::<Vec<XIJElementItem<_>>>())
            .then(xml_end)
            .map_with(|((name, items), (self_closed, nodes)), x| {
                let mut properties = vec![];
                let mut arguments = vec![];

                for item in items {
                    match item {
                        XIJElementItem::Argument(arg) => {
                            arguments.push(arg);
                        }
                        XIJElementItem::Property(prop) => {
                            properties.push(prop);
                        }
                    }
                }

                ts(
                    XIJElement {
                        name,
                        properties,
                        arguments,
                        nodes,
                        self_closed,
                    },
                    x,
                )
            }),
    );

    element_decl
        .map(|x| x.map(Expression::XIJElement))
        .or(fragment_decl.map(|x| x.map(Expression::XIJFragment)))
}

#[cfg(test)]
pub mod tests {
    use crate::parse;

    #[test]
    fn test_xij() {
        let input = r#"<hello world=42 5 "yes" {...wow}>item</hello>"#;
        let p = parse(input);

        println!("{p:?}");
    }
}