use crate::ast::{BinaryOperator, CodeExpression, Expression, Pattern, Template, TypePath};

/// A parser for Alcova templates.
/// Supports a minimal subset of Rust-like syntax.
///
/// Based on parser combinators from: https://bodil.lol/parser-combinators/

// These are special keywords used in the templating languages.
// Keep track of them here so we don't parse an identifier that is a keyword.
const KEYWORDS: &'static [&'static str] =
    &["if", "else", "for", "match", "end", "let", "true", "false"];

macro_rules! any_of {
    ($($parser:expr),*) => {
        move |input| {
            let err = Err(input);
            $(
                if let Ok(result) = ($parser).parse(input) {
                    return Ok(result);
                }
            )*
            err
        }
    };
}

macro_rules! all_of {
    ($($parser:expr),*) => {
        #[allow(unused_assignments)]
        move |mut input| {
            $(
                let (new_input, _) = ($parser).parse(input)?;
                input = new_input;
            )*
            Ok((input, ()))
        }
    };
}

pub fn parse_template<'a>() -> impl Parser<'a, Template> {
    zero_or_more(expression()).map(|expressions| Template { expressions })
}

// TODO: Possibly rename this to something else
// These "expressions" output code that generates a slot in the template.
fn expression<'a>() -> impl Parser<'a, Expression> {
    either(code_block(), text())
}

fn text<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a str| {
        let end = match input.find("{{") {
            Some(pos) => pos,
            None => input.len(),
        };

        if end == 0 {
            return Err(input);
        }

        let expression = Expression::Literal(input[..end].to_string());
        Ok((&input[end..], expression))
    }
}

fn code_block<'a>() -> impl Parser<'a, Expression> {
    let symbol_or_call = move |input| {
        right(literal("{{"), left(code(), literal("}}")))
            .map(|code| Expression::CodeBlock(code))
            .parse(input)
    };

    any_of!(
        for_expression(),
        if_let_expression(),
        if_expression(),
        match_expression(),
        symbol_or_call
    )
}

fn type_path<'a>() -> impl Parser<'a, TypePath> {
    pair(
        zero_or_more(left(non_keyword_ident(), literal("::"))),
        non_keyword_ident(),
    )
    .map(|(mut segments, trailing)| {
        segments.push(trailing);
        TypePath { segments }
    })
}

fn pattern<'a>() -> impl Parser<'a, Pattern> {
    let struct_or_enum = move |input| {
        let (input, type_path) = type_path().parse(input)?;

        let (input, _) = space0().parse(input)?;

        let (input, opening) = any_char.pred(|c| ['{', '('].contains(c)).parse(input)?;

        match opening {
            '{' => {
                let matched_field = pair(
                    BoxedParser::new(whitespace_wrap(left(
                        whitespace_wrap(non_keyword_ident()),
                        literal(":"),
                    ))),
                    pattern(),
                );
                let bound_field = whitespace_wrap(non_keyword_ident())
                    .map(|name| (name.clone(), Pattern::Binding { name }));
                // Box to not hit Rust type recursion limit :)
                let field_parser = BoxedParser::new(any_of!(matched_field, bound_field));
                let (input, fields) =
                    delimited_list(whitespace_wrap(field_parser), ",").parse(input)?;

                let (input, _) = pair(space0(), literal("}")).parse(input)?;

                Ok((input, Pattern::Struct { type_path, fields }))
            }
            '(' => {
                let (input, fields) =
                    delimited_list(whitespace_wrap(pattern()), ",").parse(input)?;
                let (input, _) = pair(space0(), literal(")")).parse(input)?;

                Ok((input, Pattern::Enum { type_path, fields }))
            }
            _ => unreachable!(),
        }
    };

    let binding = non_keyword_ident().map(|name| Pattern::Binding { name });

    any_of!(struct_or_enum, binding)
}

fn if_let_expression<'a>() -> impl Parser<'a, Expression> {
    move |input| {
        let (input, _) = all_of!(
            literal("{{"),
            space0(),
            literal("if"),
            space1(),
            literal("let"),
            space1()
        )
        .parse(input)?;

        let (input, pattern) = pattern().parse(input)?;

        let (input, _) = whitespace_wrap(literal("=")).parse(input)?;

        let (input, data) = code().parse(input)?;

        let (input, _) = pair(space0(), literal("}}")).parse(input)?;

        let (input, true_arm) = zero_or_more(expression()).parse(input)?;

        let (input, false_arm) = if let Ok((input, _)) = pair(
            pair(literal("{{"), whitespace_wrap(literal("else"))),
            literal("}}"),
        )
        .parse(input)
        {
            zero_or_more(expression()).parse(input)?
        } else {
            (input, vec![])
        };

        let (input, _) = pair(
            pair(literal("{{"), whitespace_wrap(literal("end"))),
            literal("}}"),
        )
        .parse(input)?;

        Ok((
            input,
            Expression::IfLet {
                pattern,
                data: Box::new(data),
                true_arm,
                false_arm,
            },
        ))
    }
}

fn if_expression<'a>() -> impl Parser<'a, Expression> {
    move |input| {
        let (input, _) = pair(literal("{{"), space0()).parse(input)?;

        let (input, _) = literal("if").parse(input)?;

        let (input, _) = space1().parse(input)?;

        let (input, condition) = code().parse(input)?;

        let (input, _) = pair(space0(), literal("}}")).parse(input)?;

        let (input, true_arm) = zero_or_more(expression()).parse(input)?;

        let (input, false_arm) = if let Ok((input, _)) = pair(
            pair(literal("{{"), whitespace_wrap(literal("else"))),
            literal("}}"),
        )
        .parse(input)
        {
            zero_or_more(expression()).parse(input)?
        } else {
            (input, vec![])
        };

        let (input, _) = pair(
            pair(literal("{{"), whitespace_wrap(literal("end"))),
            literal("}}"),
        )
        .parse(input)?;

        Ok((
            input,
            Expression::If {
                condition: Box::new(condition),
                true_arm,
                false_arm,
            },
        ))
    }
}

fn match_arm<'a>() -> impl Parser<'a, (Pattern, Vec<Expression>)> {
    move |input| {
        let (input, _) = pair(literal("{{="), space0()).parse(input)?;

        let (input, pattern) = pattern().parse(input)?;

        let (input, _) = pair(space0(), literal("}}")).parse(input)?;

        let (input, exprs) = zero_or_more(expression()).parse(input)?;

        Ok((input, (pattern, exprs)))
    }
}

fn match_expression<'a>() -> impl Parser<'a, Expression> {
    move |input| {
        let (input, _) =
            all_of!(literal("{{"), space0(), literal("match"), space1()).parse(input)?;

        let (input, data) = left(whitespace_wrap(code()), literal("}}")).parse(input)?;

        let (input, arms) = one_or_more(whitespace_wrap(match_arm())).parse(input)?;

        let (input, _) = pair(
            pair(literal("{{"), whitespace_wrap(literal("end"))),
            literal("}}"),
        )
        .parse(input)?;

        Ok((
            input,
            Expression::Match {
                data: Box::new(data),
                arms,
            },
        ))
    }
}

fn for_expression<'a>() -> impl Parser<'a, Expression> {
    move |input| {
        let (input, _) = pair(literal("{{"), space0()).parse(input)?;

        let (input, _) = literal("for").parse(input)?;

        let (input, _) = space1().parse(input)?;

        let (input, binding) = non_keyword_ident().parse(input)?;

        let (input, _) = space1().parse(input)?;

        let (input, _) = literal("in").parse(input)?;

        let (input, _) = space1().parse(input)?;

        let (input, iterator) = code().parse(input)?;

        let (input, _) = pair(space0(), literal("}}")).parse(input)?;

        let (input, body) = zero_or_more(expression()).parse(input)?;

        let (input, _) = pair(
            pair(literal("{{"), whitespace_wrap(literal("end"))),
            literal("}}"),
        )
        .parse(input)?;

        Ok((
            input,
            Expression::For {
                binding,
                iterator: Box::new(iterator),
                body,
            },
        ))
    }
}

fn binary_operator<'a>() -> impl Parser<'a, &'static str> {
    // TODO: Replace this with a more performant parser?
    any_of!(
        literal("=="),
        literal("!="),
        literal(">="),
        literal(">"),
        literal("<="),
        literal("<"),
        literal("&&"),
        literal("||")
    )
}

fn code<'a>() -> impl Parser<'a, CodeExpression> {
    move |input| {
        let group_parser = move |input| {
            left(
                right(pair(space0(), literal("(")), whitespace_wrap(code())),
                pair(literal(")"), space0()),
            )
            .map(|expr| CodeExpression::ParenGroup { on: Box::new(expr) })
            .parse(input)
        };

        let (mut input, mut expr) = any_of!(group_parser, ungrouped_code()).parse(input)?;

        while let Ok((new_input, bin_op)) = whitespace_wrap(binary_operator()).parse(input) {
            input = new_input;

            let (new_input, right) = code().parse(input)?;
            input = new_input;

            expr = CodeExpression::BinOp {
                left: Box::new(expr),
                right: Box::new(right),
                op: BinaryOperator::from(bin_op),
            }
        }
        Ok((input, expr))
    }
}

fn ungrouped_code<'a>() -> impl Parser<'a, CodeExpression> {
    move |input| {
        // Put in closure to not break rustc
        let ref_parser = move |input| {
            right(whitespace_wrap(literal("&")), code())
                .map(|on| CodeExpression::Ref { on: Box::new(on) })
                .parse(input)
        };

        let deref_parser = move |input| {
            right(whitespace_wrap(literal("*")), code())
                .map(|on| CodeExpression::Deref { on: Box::new(on) })
                .parse(input)
        };

        let not_parser = move |input| {
            right(whitespace_wrap(literal("!")), code())
                .map(|on| CodeExpression::Not { on: Box::new(on) })
                .parse(input)
        };

        let expr_parser = any_of!(deref_parser, ref_parser, not_parser, code_expr_or_literal());

        let (input, expr) = expr_parser.parse(input)?;

        Ok((input, expr))
    }
}

fn string_literal<'a>() -> impl Parser<'a, CodeExpression> {
    quoted_string().map(|value| CodeExpression::StringLiteral { value })
}

fn number_literal<'a>() -> impl Parser<'a, CodeExpression> {
    any_of!(
        float.map(|value| CodeExpression::FloatLiteral { value }),
        int.map(|value| CodeExpression::IntLiteral { value })
    )
}

fn bool_literal<'a>() -> impl Parser<'a, CodeExpression> {
    any_of!(literal("true"), literal("false")).map(|value| CodeExpression::BoolLiteral {
        value: value == "true",
    })
}

fn code_expr_or_literal<'a>() -> impl Parser<'a, CodeExpression> {
    any_of!(
        number_literal(),
        string_literal(),
        bool_literal(),
        code_expr()
    )
}

fn code_expr<'a>() -> impl Parser<'a, CodeExpression> {
    whitespace_wrap(move |input| {
        let (mut remaining, mut left) = symbol().parse(input)?;

        while let Ok((new_remaining, punc)) =
            any_char.pred(|c| ['.', '('].contains(&c)).parse(remaining)
        {
            match punc {
                '.' => {
                    let (new_remaining, field) = non_keyword_ident().parse(new_remaining)?;
                    remaining = new_remaining;
                    left = CodeExpression::Accessor {
                        on: Box::new(left),
                        field,
                    };
                }
                '(' => {
                    let (new_remaining, params) = call_params().parse(new_remaining)?;
                    remaining = new_remaining;
                    left = CodeExpression::Call {
                        on: Box::new(left),
                        params,
                    };
                }
                _ => unreachable!(),
            }
        }

        Ok((remaining, left))
    })
}

fn call_params<'a>() -> impl Parser<'a, Vec<CodeExpression>> {
    // We have to box up so we don't hit type recursion limits :)
    left(
        delimited_list(whitespace_wrap(BoxedParser::new(code())), ","),
        literal(")"),
    )
}

fn delimited_list<'a, P, A>(parser: P, delimiter: &'static str) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut items = vec![];

        while let Ok((new_remaining, item)) = parser.parse(input) {
            items.push(item);
            input = new_remaining;

            let (new_remaining, _) = match literal(delimiter).parse(input) {
                Ok(res) => res,
                Err(_) => break,
            };
            input = new_remaining;
        }

        Ok((input, items))
    }
}

fn symbol<'a>() -> impl Parser<'a, CodeExpression> {
    either(
        right(literal("@"), non_keyword_ident()).map(|name| CodeExpression::Symbol {
            name,
            assigned: true,
        }),
        non_keyword_ident().map(|name| CodeExpression::Symbol {
            name,
            assigned: false,
        }),
    )
}

fn non_keyword_ident<'a>() -> impl Parser<'a, String> {
    identifier.pred(|word| !KEYWORDS.contains(&word.as_str()))
}

// Parser utilities

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, predicate: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, predicate))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

fn literal<'a>(expected: &'static str) -> impl Parser<'a, &'static str> {
    move |input: &'a str| {
        if input.starts_with(expected) {
            Ok((&input[expected.len()..], expected))
        } else {
            Err(input)
        }
    }
}

fn float(input: &str) -> ParseResult<f32> {
    let mut matched = String::new();

    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_numeric() || next == '-' => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_numeric() {
            matched.push(next);
        } else if next == '.' {
            matched.push(next);

            while let Some(next) = chars.next() {
                if next.is_numeric() {
                    matched.push(next);
                } else {
                    break;
                }
            }
            break;
        } else {
            return Err(input);
        }
    }

    let number = matched.parse().expect("Failed to parse number");

    let next_index = matched.len();
    Ok((&input[next_index..], number))
}

fn int(input: &str) -> ParseResult<i64> {
    let mut matched = String::new();

    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_numeric() || next == '-' => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_numeric() {
            matched.push(next);
        } else {
            break;
        }
    }

    let number = matched.parse().expect("Failed to parse number");

    let next_index = matched.len();
    Ok((&input[next_index..], number))
}

fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();

    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' || next == '_' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        let (next_input, result1) = parser1.parse(input)?;
        let (final_input, result2) = parser2.parse(next_input)?;
        Ok((final_input, (result1, result2)))
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        let (next_input, result) = parser.parse(input)?;
        Ok((next_input, map_fn(result)))
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, right)| right)
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        let (next_input, val) = parser.parse(input)?;
        input = next_input;
        result.push(val);

        while let Ok((next_input, val)) = parser.parse(input) {
            input = next_input;
            result.push(val);
        }

        Ok((input, result))
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = vec![];

        while let Ok((next_input, val)) = parser.parse(input) {
            input = next_input;
            result.push(val);
        }

        Ok((input, result))
    }
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], c)),
        None => Err(input),
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        let (next_input, item) = parser.parse(input)?;
        if predicate(&item) {
            Ok((next_input, item))
        } else {
            Err(input)
        }
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| {
        let (next_input, result) = parser.parse(input)?;
        f(result).parse(next_input)
    }
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    map(
        right(
            literal(r#"""#),
            left(zero_or_more(any_char.pred(|c| *c != '"')), literal(r#"""#)),
        ),
        |chars| chars.into_iter().collect(),
    )
}

fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_literals() {
        let literal_parser = literal("test");

        assert_eq!(literal_parser.parse("test"), Ok(("", "test")));
        assert_eq!(literal_parser.parse("nope"), Err("nope"));
        assert_eq!(literal_parser.parse("test more"), Ok((" more", "test")));
    }

    #[test]
    fn test_identifier() {
        assert_eq!(
            identifier("hello_world"),
            Ok(("", "hello_world".to_string()))
        );
        assert_eq!(identifier("1hello_world"), Err("1hello_world"));
        assert_eq!(
            identifier("hello world"),
            Ok((" world", "hello".to_string()))
        );
    }

    #[test]
    fn test_pair() {
        let at_parser = pair(literal("@"), identifier);

        assert_eq!(
            at_parser.parse("@name"),
            Ok(("", ("@", "name".to_string())))
        );
        assert_eq!(
            at_parser.parse("@name more"),
            Ok((" more", ("@", "name".to_string())))
        );
        assert_eq!(at_parser.parse("nope"), Err("nope"));
    }

    #[test]
    fn test_left() {
        let parser = left(identifier, literal("/>"));
        assert_eq!(parser.parse("thing/>"), Ok(("", "thing".to_string())));
        assert_eq!(
            parser.parse("thing/> more"),
            Ok((" more", "thing".to_string()))
        );
        assert_eq!(parser.parse("thing>"), Err(">"));
    }

    #[test]
    fn test_right() {
        let at_parser = right(literal("@"), identifier);

        assert_eq!(at_parser.parse("@name"), Ok(("", "name".to_string())));
        assert_eq!(
            at_parser.parse("@name more"),
            Ok((" more", "name".to_string()))
        );
        assert_eq!(at_parser.parse("nope"), Err("nope"));
    }

    #[test]
    fn test_one_or_more() {
        let list_parser = one_or_more(left(identifier, literal(",")));

        assert_eq!(
            list_parser.parse("list,of,things,"),
            Ok((
                "",
                vec!["list".to_string(), "of".to_string(), "things".to_string(),]
            ))
        );
        assert_eq!(
            list_parser.parse("list,of,things,more"),
            Ok((
                "more",
                vec!["list".to_string(), "of".to_string(), "things".to_string(),]
            ))
        );
        // Got the first ident, but error at space after "no"
        assert_eq!(list_parser.parse("no list"), Err(" list"),);
    }

    #[test]
    fn test_zero_or_more() {
        let list_parser = zero_or_more(left(identifier, literal(",")));

        assert_eq!(
            list_parser.parse("list,of,things,"),
            Ok((
                "",
                vec!["list".to_string(), "of".to_string(), "things".to_string(),]
            ))
        );
        assert_eq!(
            list_parser.parse("list,of,things,more"),
            Ok((
                "more",
                vec!["list".to_string(), "of".to_string(), "things".to_string(),]
            ))
        );
        // Got the first ident, but error at space after "no"
        assert_eq!(list_parser.parse("no list"), Ok(("no list", vec![])));
    }

    #[test]
    fn test_pred() {
        let o_parser = pred(any_char, |c| *c == 'o');

        assert_eq!(o_parser.parse("one"), Ok(("ne", 'o')));
        assert_eq!(o_parser.parse("none"), Err("none"));
    }

    #[test]
    fn test_quoted_string() {
        assert_eq!(
            quoted_string().parse(r#""quoted string""#),
            Ok(("", "quoted string".to_string()))
        );
        assert_eq!(
            quoted_string().parse(r#""quoted string" more"#),
            Ok((" more", "quoted string".to_string()))
        );
        assert_eq!(quoted_string().parse("no string"), Err("no string"));
    }

    // TODO: Split this into a number of smaller tests

    #[test]
    fn test_template() {
        let template = r#"<h1>Hello {{ @name }}!</h1><p>Count is {{ count }}</p>{{ @deal.business.address }}{{ @business.display_name() }}{{ @business.display_name(time, @name) }}{{ for name in @names }}<h3>{{ name }}</h3>{{ end }}{{ if @thing }}Enabled!{{ else }}Disabled{{ end }}{{ if let Expression::Code(inner, Some(thing)) = thing }}{{ inner }}{{ end }}{{ if let some::module::Thing { a: Some(b), c, d: Other { a } } = my_thing }}{{ a }}{{ end }}"#;
        // dbg!(parse_template().parse(template),);
        assert_eq!(
            parse_template().parse(template),
            Ok((
                "",
                Template {
                    expressions: vec![
                        Expression::Literal("<h1>Hello ".to_string()),
                        Expression::CodeBlock(CodeExpression::Symbol {
                            name: "name".into(),
                            assigned: true
                        }),
                        Expression::Literal("!</h1><p>Count is ".to_string()),
                        Expression::CodeBlock(CodeExpression::Symbol {
                            name: "count".into(),
                            assigned: false,
                        }),
                        Expression::Literal("</p>".to_string()),
                        Expression::CodeBlock(CodeExpression::Accessor {
                            on: Box::new(CodeExpression::Accessor {
                                on: Box::new(CodeExpression::Symbol {
                                    name: "deal".into(),
                                    assigned: true
                                }),
                                field: "business".into(),
                            }),
                            field: "address".into()
                        }),
                        Expression::CodeBlock(CodeExpression::Call {
                            on: Box::new(CodeExpression::Accessor {
                                on: Box::new(CodeExpression::Symbol {
                                    name: "business".into(),
                                    assigned: true
                                }),
                                field: "display_name".into(),
                            }),
                            params: vec![]
                        }),
                        Expression::CodeBlock(CodeExpression::Call {
                            on: Box::new(CodeExpression::Accessor {
                                on: Box::new(CodeExpression::Symbol {
                                    name: "business".into(),
                                    assigned: true
                                }),
                                field: "display_name".into(),
                            }),
                            params: vec![
                                CodeExpression::Symbol {
                                    name: "time".into(),
                                    assigned: false,
                                },
                                CodeExpression::Symbol {
                                    name: "name".into(),
                                    assigned: true,
                                },
                            ]
                        }),
                        Expression::For {
                            iterator: Box::new(CodeExpression::Symbol {
                                name: "names".into(),
                                assigned: true
                            }),
                            binding: "name".into(),
                            body: vec![
                                Expression::Literal("<h3>".into()),
                                Expression::CodeBlock(CodeExpression::Symbol {
                                    name: "name".into(),
                                    assigned: false,
                                }),
                                Expression::Literal("</h3>".into()),
                            ]
                        },
                        Expression::If {
                            condition: Box::new(CodeExpression::Symbol {
                                name: "thing".into(),
                                assigned: true
                            }),
                            true_arm: vec![Expression::Literal("Enabled!".into()),],
                            false_arm: vec![Expression::Literal("Disabled".into())],
                        },
                        Expression::IfLet {
                            pattern: Pattern::Enum {
                                type_path: TypePath {
                                    segments: vec!["Expression".into(), "Code".into()],
                                },
                                fields: vec![
                                    Pattern::Binding {
                                        name: "inner".into()
                                    },
                                    Pattern::Enum {
                                        type_path: TypePath {
                                            segments: vec!["Some".into()],
                                        },
                                        fields: vec![Pattern::Binding {
                                            name: "thing".into()
                                        },],
                                    },
                                ],
                            },
                            data: Box::new(CodeExpression::Symbol {
                                name: "thing".into(),
                                assigned: false,
                            }),
                            true_arm: vec![Expression::CodeBlock(CodeExpression::Symbol {
                                name: "inner".into(),
                                assigned: false,
                            },),],
                            false_arm: vec![],
                        },
                        Expression::IfLet {
                            pattern: Pattern::Struct {
                                type_path: TypePath {
                                    segments: vec!["some".into(), "module".into(), "Thing".into(),],
                                },
                                fields: vec![
                                    (
                                        "a".into(),
                                        Pattern::Enum {
                                            type_path: TypePath {
                                                segments: vec!["Some".into(),],
                                            },
                                            fields: vec![Pattern::Binding { name: "b".into() },],
                                        },
                                    ),
                                    ("c".into(), Pattern::Binding { name: "c".into() },),
                                    (
                                        "d".into(),
                                        Pattern::Struct {
                                            type_path: TypePath {
                                                segments: vec!["Other".into(),],
                                            },
                                            fields: vec![(
                                                "a".into(),
                                                Pattern::Binding { name: "a".into() },
                                            ),],
                                        },
                                    ),
                                ],
                            },
                            data: Box::new(CodeExpression::Symbol {
                                name: "my_thing".into(),
                                assigned: false,
                            }),
                            true_arm: vec![Expression::CodeBlock(CodeExpression::Symbol {
                                name: "a".into(),
                                assigned: false,
                            },),],
                            false_arm: vec![],
                        },
                    ]
                }
            ))
        );
    }

    #[test]
    fn test_template_ref() {
        let result = parse_template().parse("{{ &@test.thing }}");

        assert_eq!(
            result,
            Ok((
                "",
                Template {
                    expressions: vec![Expression::CodeBlock(CodeExpression::Ref {
                        on: Box::new(CodeExpression::Accessor {
                            on: Box::new(CodeExpression::Symbol {
                                name: "test".into(),
                                assigned: true
                            }),
                            field: "thing".into()
                        })
                    })],
                }
            ))
        );
    }

    #[test]
    fn test_match_expression() {
        let result = parse_template()
            .parse("{{ match thing }}{{= Some(a) }}{{ a }}{{= None }}None{{ end }}");

        assert_eq!(
            result,
            Ok((
                "",
                Template {
                    expressions: vec![Expression::Match {
                        data: Box::new(CodeExpression::Symbol {
                            name: "thing".into(),
                            assigned: false,
                        }),
                        arms: vec![
                            (
                                Pattern::Enum {
                                    type_path: TypePath {
                                        segments: vec!["Some".into()],
                                    },
                                    fields: vec![Pattern::Binding { name: "a".into() },],
                                },
                                vec![Expression::CodeBlock(CodeExpression::Symbol {
                                    name: "a".into(),
                                    assigned: false,
                                },),],
                            ),
                            (
                                Pattern::Binding {
                                    name: "None".into()
                                },
                                vec![Expression::Literal("None".into(),),],
                            ),
                        ],
                    },],
                },
            ))
        );
    }

    #[test]
    fn test_bin_op() {
        let result = parse_template().parse("{{ if @count > 3.0 }}{{ end }}");
        assert_eq!(
            result,
            Ok((
                "",
                Template {
                    expressions: vec![Expression::If {
                        condition: Box::new(CodeExpression::BinOp {
                            op: BinaryOperator::Gt,
                            left: Box::new(CodeExpression::Symbol {
                                name: "count".into(),
                                assigned: true
                            }),
                            right: Box::new(CodeExpression::FloatLiteral { value: 3.0 }),
                        }),
                        true_arm: vec![],
                        false_arm: vec![],
                    }],
                }
            ))
        );

        let result =
            parse_template().parse("{{ if (@count > 3) && (@thing != \"hello\") }}{{ end }}");

        assert_eq!(
            result,
            Ok((
                "",
                Template {
                    expressions: vec![Expression::If {
                        condition: Box::new(CodeExpression::BinOp {
                            op: BinaryOperator::And,
                            left: Box::new(CodeExpression::ParenGroup {
                                on: Box::new(CodeExpression::BinOp {
                                    op: BinaryOperator::Gt,
                                    left: Box::new(CodeExpression::Symbol {
                                        name: "count".into(),
                                        assigned: true,
                                    }),
                                    right: Box::new(CodeExpression::IntLiteral { value: 3 }),
                                })
                            }),
                            right: Box::new(CodeExpression::ParenGroup {
                                on: Box::new(CodeExpression::BinOp {
                                    op: BinaryOperator::NotEq,
                                    left: Box::new(CodeExpression::Symbol {
                                        name: "thing".into(),
                                        assigned: true,
                                    }),
                                    right: Box::new(CodeExpression::StringLiteral {
                                        value: "hello".into()
                                    }),
                                })
                            }),
                        }),
                        true_arm: vec![],
                        false_arm: vec![],
                    },],
                },
            ))
        );
    }
}
