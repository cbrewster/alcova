#[derive(Debug, PartialEq)]
pub struct Template {
    pub expressions: Vec<TemplateBlock>,
}

// TODO: Maybe combine unary operators (&, *, !)
#[derive(Debug, PartialEq)]
pub enum Expression {
    Ref {
        on: Box<Expression>,
    },
    Deref {
        on: Box<Expression>,
    },
    Not {
        on: Box<Expression>,
    },
    Symbol {
        name: String,
        assigned: bool,
    },
    Accessor {
        on: Box<Expression>,
        field: String,
    },
    Call {
        on: Box<Expression>,
        params: Vec<Expression>,
    },
    BinOp {
        op: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    // TODO: Handle precedence ourselves.
    // We don't do proper precedence handling.
    // As long as we make sure to output the same Rust code as what we get, including paren
    // groupings, rustc should handle the precedence for us.
    ParenGroup {
        on: Box<Expression>,
    },
    // TODO: Maybe we should support all the different number variants.
    IntLiteral {
        value: i32,
    },
    FloatLiteral {
        value: f32,
    },
    StringLiteral {
        value: String,
    },
    BoolLiteral {
        value: bool,
    },
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    EqEq,
    NotEq,
    Le,
    Lt,
    Ge,
    Gt,
    And,
    Or,
}

impl From<&str> for BinaryOperator {
    fn from(op: &str) -> Self {
        use BinaryOperator::*;

        match op {
            "==" => EqEq,
            "!=" => NotEq,
            "<=" => Le,
            "<" => Lt,
            ">=" => Ge,
            ">" => Gt,
            "&&" => And,
            "||" => Or,
            _ => panic!("Unexpected binary operator {}", op),
        }
    }
}

impl Expression {
    pub fn get_dependencies(&self) -> Vec<String> {
        match self {
            Expression::Ref { on }
            | Expression::Deref { on }
            | Expression::Not { on }
            | Expression::ParenGroup { on } => on.get_dependencies(),
            Expression::Symbol { name, assigned } => {
                if *assigned {
                    vec![name.clone()]
                } else {
                    vec![]
                }
            }
            Expression::Accessor { on, .. } => on.get_dependencies(),
            Expression::Call { on, params } => {
                let mut deps = on.get_dependencies();
                for param in params {
                    deps.extend(param.get_dependencies());
                }
                deps
            }
            Expression::BinOp { left, right, .. } => {
                let mut deps = left.get_dependencies();
                deps.extend(right.get_dependencies());
                deps
            }

            Expression::IntLiteral { .. }
            | Expression::FloatLiteral { .. }
            | Expression::StringLiteral { .. }
            | Expression::BoolLiteral { .. } => vec![],
        }
    }
}

// TODO: All these Vec<Expression>s should be templates

#[derive(Debug, PartialEq)]
pub enum TemplateBlock {
    Literal(String),
    Expression(Expression),
    For {
        iterator: Box<Expression>,
        binding: String,
        body: Vec<TemplateBlock>,
    },
    If {
        condition: Box<Expression>,
        true_arm: Vec<TemplateBlock>,
        false_arm: Vec<TemplateBlock>,
    },
    IfLet {
        pattern: Pattern,
        data: Box<Expression>,
        true_arm: Vec<TemplateBlock>,
        false_arm: Vec<TemplateBlock>,
    },
    Match {
        data: Box<Expression>,
        arms: Vec<(Pattern, Vec<TemplateBlock>)>,
    },
}

#[derive(Debug, PartialEq)]
pub struct TypePath {
    pub segments: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Binding {
        name: String,
    },
    Enum {
        type_path: TypePath,
        fields: Vec<Pattern>,
    },
    Struct {
        type_path: TypePath,
        fields: Vec<(String, Pattern)>,
    },
}

impl TemplateBlock {
    pub fn get_dependencies(&self) -> Vec<String> {
        match self {
            TemplateBlock::Literal(_) => vec![],
            TemplateBlock::Expression(code_expr) => code_expr.get_dependencies(),
            TemplateBlock::For { iterator, body, .. } => {
                let mut deps = iterator.get_dependencies();
                deps.extend(body.iter().flat_map(|expr| expr.get_dependencies()));
                deps
            }
            TemplateBlock::If {
                condition,
                true_arm,
                false_arm,
            } => {
                let mut deps = condition.get_dependencies();
                deps.extend(true_arm.iter().flat_map(|expr| expr.get_dependencies()));
                deps.extend(false_arm.iter().flat_map(|expr| expr.get_dependencies()));
                deps
            }
            TemplateBlock::IfLet {
                data,
                true_arm,
                false_arm,
                ..
            } => {
                let mut deps = data.get_dependencies();
                deps.extend(true_arm.iter().flat_map(|expr| expr.get_dependencies()));
                deps.extend(false_arm.iter().flat_map(|expr| expr.get_dependencies()));
                deps
            }
            TemplateBlock::Match { data, arms } => {
                let mut deps = data.get_dependencies();
                for arm in arms {
                    for expr in &arm.1 {
                        deps.extend(expr.get_dependencies());
                    }
                }
                deps
            }
        }
    }
}
