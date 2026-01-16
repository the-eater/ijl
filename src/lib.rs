use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::input::{Stream, Input};
use chumsky::Parser;
use crate::lexer::lexer;
use crate::parser::Statement;
use crate::parser::utils::Untyped;

pub mod lexer;
pub mod parser;

pub fn parse<'src>(input: impl AsRef<str> + 'src) -> Statement<Untyped> {
    let v = lexer(input.as_ref()).spanned();
    let stream = Stream::from_iter(v).map((0..input.as_ref().len()).into(), |(t, s): (_, _)| (t, s));

    let (src, errs) = crate::parser::parser().parse(stream).into_output_errors();

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .for_each(|e| {
            Report::build(ReportKind::Error, ("<embed>", e.span().into_range()))
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                .with_message(e.to_string())
                .with_label(
                    Label::new(("<embed>", e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new(("<embed>", span.into_range()))
                        .with_message(format!("while parsing this {label}"))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .print(sources([("<embed>", input.as_ref())]))
                .unwrap()
        });

    src.unwrap()
}