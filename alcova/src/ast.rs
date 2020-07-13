#[derive(Debug, PartialEq)]
pub struct Template {
    pub expressions: Vec<Expression>,
}

// TODO: Rename this to something that makes more sense
#[derive(Debug, PartialEq)]
pub enum CodeExpression {
    Ref {
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
}

impl CodeExpression {
    pub fn get_dependencies(&self) -> Vec<String> {
        match self {
            CodeExpression::Ref { on } => on.get_dependencies(),
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
            Expression::IfLet {
                data,
                true_arm,
                false_arm,
                ..
            } => {
                let mut deps = data.get_dependencies();
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
            Expression::Match { data, arms } => {
                let mut deps = data.get_dependencies();
                for arm in arms {
                    for expr in &arm.1 {
                        deps.extend(expr.get_dependencies().into_iter());
                    }
                }
                deps
            }
        }
    }
}
