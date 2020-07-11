const KEYWORDS: &'static [&'static str] = &["if", "else", "for", "match", "end"];

macro_rules! any_of {
    ($($parser:expr),*) => {
        #[allow(unused_assignments)]
        move |input| {
            let mut err = Err(input);
            $(
                match ($parser).parse(input) {
                    ok @ Ok(_) => return ok,
                    e @ Err(_) => err = e,
                }
            )*
            err
        }
    };
}

// TODO: Rename this to something that makes more sense
#[derive(Debug, PartialEq)]
pub enum CodeExpression {
    Symbol {
        name: String,
        assigned: bool,
    },
    Accessor {
        on: Box<CodeExpression>,
        field: String,
    },
    Call {
        on: Box<CodeExpression>,
        params: Vec<CodeExpression>,
    },
}

impl CodeExpression {
    pub fn get_dependencies(&self) -> Vec<String> {
        match self {
            CodeExpression::Symbol { name, assigned } => {
                if *assigned {
                    vec![name.clone()]
                } else {
                    vec![]
                }
            }
            CodeExpression::Accessor { on, .. } => on.get_dependencies(),
            CodeExpression::Call { on, params } => {
                let mut deps = on.get_dependencies();
                for param in params {
                    deps.extend(param.get_dependencies().into_iter());
                }
                deps
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(String),
    CodeBlock(CodeExpression),
    For {
        iterator: Box<CodeExpression>,
        binding: String,
        body: Vec<Expression>,
    },
    If {
        condition: Box<CodeExpression>,
        true_arm: Vec<Expression>,
        false_arm: Vec<Expression>,
    },
}

impl Expression {
    pub fn get_dependencies(&self) -> Vec<String> {
        match self {
            Expression::Literal(_) => vec![],
            Expression::CodeBlock(code_expr) => code_expr.get_dependencies(),
            Expression::For { iterator, body, .. } => {
                let mut deps = iterator.get_dependencies();
                deps.extend(
                    body.iter()
                        .flat_map(|expr| expr.get_dependencies().into_iter()),
                );
                deps
            }
            Expression::If {
                condition,
                true_arm,
                false_arm,
            } => {
                let mut deps = condition.get_dependencies();
                deps.extend(
                    true_arm
                        .iter()
                        .flat_map(|expr| expr.get_dependencies().into_iter()),
                );
                deps.extend(
                    false_arm
                        .iter()
                        .flat_map(|expr| expr.get_dependencies().into_iter()),
                );
                deps
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Template {
    pub expressions: Vec<Expression>,
}

pub fn parse_template<'a>() -> impl Parser<'a, Template> {
    zero_or_more(expression()).map(|expressions| Template { expressions })
}

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

    any_of! {
        for_expression(), if_expression(), symbol_or_call
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

fn code<'a>() -> impl Parser<'a, CodeExpression> {
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

fn literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| {
        if input.starts_with(expected) {
            Ok((&input[expected.len()..], ()))
        } else {
            Err(input)
        }
    }
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

        assert_eq!(literal_parser.parse("test"), Ok(("", ())));
        assert_eq!(literal_parser.parse("nope"), Err("nope"));
        assert_eq!(literal_parser.parse("test more"), Ok((" more", ())));
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

        assert_eq!(at_parser.parse("@name"), Ok(("", ((), "name".to_string()))));
        assert_eq!(
            at_parser.parse("@name more"),
            Ok((" more", ((), "name".to_string())))
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

    #[test]
    fn test_template() {
        let template = r#"<h1>Hello {{ @name }}!</h1><p>Count is {{ count }}</p>{{ @deal.business.address }}{{ @business.display_name() }}{{ @business.display_name(time, @name) }}{{ for name in @names }}<h3>{{ name }}</h3>{{ end }}{{ if @thing }}Enabled!{{ else }}Disabled{{ end }}"#;
        dbg!(parse_template().parse(template),);
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
                    ]
                }
            ))
        );
    }
}
