#[derive(Debug, PartialEq)]
pub struct Template {
    pub expressions: Vec<Expression>,
}

// TODO: Rename this to something that makes more sense
// TODO: Maybe combine unary operators (&, *, !)
#[derive(Debug, PartialEq)]
pub enum CodeExpression {
    Ref {
        on: Box<CodeExpression>,
    },
    Deref {
        on: Box<CodeExpression>,
    },
    Not {
        on: Box<CodeExpression>,
    },
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
    BinOp {
        op: BinaryOperator,
        left: Box<CodeExpression>,
        right: Box<CodeExpression>,
    },
    NumberLiteral {
        value: i64,
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

impl CodeExpression {
    pub fn get_dependencies(&self) -> Vec<String> {
        match self {
            CodeExpression::Ref { on } => on.get_dependencies(),
            CodeExpression::Deref { on } => on.get_dependencies(),
            CodeExpression::Not { on } => on.get_dependencies(),
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
                    deps.extend(param.get_dependencies());
                }
                deps
            }
            CodeExpression::BinOp { left, right, .. } => {
                let mut deps = left.get_dependencies();
                deps.extend(right.get_dependencies());
                deps
            }

            CodeExpression::NumberLiteral { .. }
            | CodeExpression::StringLiteral { .. }
            | CodeExpression::BoolLiteral { .. } => vec![],
        }
    }
}

// TODO: All these Vec<Expression>s should be templates

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
    IfLet {
        pattern: Pattern,
        data: Box<CodeExpression>,
        true_arm: Vec<Expression>,
        false_arm: Vec<Expression>,
    },
    Match {
        data: Box<CodeExpression>,
        arms: Vec<(Pattern, Vec<Expression>)>,
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

impl Expression {
    pub fn get_dependencies(&self) -> Vec<String> {
        match self {
            Expression::Literal(_) => vec![],
            Expression::CodeBlock(code_expr) => code_expr.get_dependencies(),
            Expression::For { iterator, body, .. } => {
                let mut deps = iterator.get_dependencies();
                deps.extend(body.iter().flat_map(|expr| expr.get_dependencies()));
                deps
            }
            Expression::If {
                condition,
                true_arm,
                false_arm,
            } => {
                let mut deps = condition.get_dependencies();
                deps.extend(true_arm.iter().flat_map(|expr| expr.get_dependencies()));
                deps.extend(false_arm.iter().flat_map(|expr| expr.get_dependencies()));
                deps
            }
            Expression::IfLet {
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
            Expression::Match { data, arms } => {
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
