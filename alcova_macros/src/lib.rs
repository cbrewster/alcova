extern crate proc_macro;

use alcova::{
    parse_template, BinaryOperator, CodeExpression, Expression, Parser, Pattern, TypePath,
};
use proc_macro::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use quote::quote;
use std::fs::File;
use std::{io::prelude::*, path::PathBuf};
use syn::{self, export::quote::format_ident, Lit, Meta, NestedMeta};

#[proc_macro_error]
#[proc_macro_derive(LiveTemplate, attributes(alcova))]
pub fn live_template(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    impl_live_template(&ast)
}

fn impl_live_template(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;

    let mut template_path: Option<String> = None;

    for meta in ast
        .attrs
        .iter()
        .filter_map(|attr| attr.parse_meta().ok())
        .filter(|meta| meta.path().is_ident("alcova"))
    {
        if let Meta::List(list) = meta {
            for meta in list.nested {
                if let NestedMeta::Meta(Meta::NameValue(named)) = meta {
                    if named.path.is_ident("template") {
                        if let Lit::Str(path) = named.lit {
                            template_path = Some(path.value());
                            break;
                        } else {
                            abort!(ast.ident, "Expected template path to be a string.");
                        }
                    }
                }
            }
        }
    }

    let template_path = match template_path {
        Some(template_path) => template_path,
        None =>
        abort!(ast.ident, "No template path was provided. Please provide a template path with the #[alcova(template = \"template.html.rtl\")] attribute."),
    };

    let manifest_dir = PathBuf::from(
        std::env::var("CARGO_MANIFEST_DIR").expect("Failed to get CARGO_MANIFEST_DIR"),
    );

    let template_path = manifest_dir.join(template_path);

    let mut template_file = match File::open(&template_path) {
        Ok(file) => file,
        Err(err) => abort!(ast.ident, format!("Failed to open template: {}", err)),
    };

    let mut contents = String::new();
    if let Err(err) = template_file.read_to_string(&mut contents) {
        abort!(ast.ident, format!("Error reading template {}", err));
    }

    let template = match parse_template().parse(&contents) {
        Ok(("", template)) => template,
        _ => abort!(
            ast.ident,
            "An error occurred parsing template. TODO: Show parse errors"
        ),
    };

    let slots = match generate_slots(&template) {
        Ok(slots) => slots,
        Err(err) => abort!(ast.ident, err),
    };

    let changes = match generate_changes(&template) {
        Ok(changes) => changes,
        Err(err) => abort!(ast.ident, err),
    };

    let template_path = template_path.to_string_lossy();

    let phony_ident = format_ident!("_RUST_C_RECOMPILE_{}", name);

    let expanded = quote! {
        #[allow(non_upper_case_globals)]
        const #phony_ident: &'static str = include_str!(#template_path);

        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl liveview::LiveTemplate for #name {
            fn render(&self) -> liveview::RenderedTemplate {
                use std::convert::TryInto;

                liveview::RenderedTemplate {
                    slots: vec![
                        #slots
                    ]
                }
            }

            fn changes(&self, old_template: &Self) -> liveview::Changes {
                use std::convert::TryInto;

                let mut changes = vec![];

                #changes

                liveview::Changes { changes }
            }
        }
    };

    // println!("generated: {}", expanded);

    expanded.into()
}

fn generate_code_expression(
    expression: &CodeExpression,
    assignee: &str,
) -> proc_macro2::TokenStream {
    let mut tokens: Vec<proc_macro2::TokenTree> = vec![];

    match expression {
        CodeExpression::Ref { on } => {
            let on = generate_code_expression(on, assignee);
            tokens.extend(quote! { &#on });
        }
        CodeExpression::Deref { on } => {
            let on = generate_code_expression(on, assignee);
            tokens.extend(quote! { *#on });
        }
        CodeExpression::Not { on } => {
            let on = generate_code_expression(on, assignee);
            tokens.extend(quote! { !(#on) });
        }
        CodeExpression::ParenGroup { on } => {
            let on = generate_code_expression(on, assignee);
            tokens.extend(quote! { ( #on ) });
        }
        CodeExpression::NumberLiteral { value } => {
            tokens.extend(quote! { (#value).try_into().unwrap() });
        }
        CodeExpression::StringLiteral { value } => {
            tokens.extend(quote! { #value });
        }
        CodeExpression::BoolLiteral { value } => {
            tokens.extend(quote! { #value });
        }
        CodeExpression::Symbol { name, assigned } => {
            let name = format_ident!("{}", name);
            let name = if *assigned {
                let assign = format_ident!("{}", assignee);
                quote! { #assign.#name }
            } else {
                quote! { #name }
            };

            tokens.extend(quote! { #name });
        }
        CodeExpression::Accessor { on, field } => {
            let on = generate_code_expression(on, assignee);
            let field = format_ident!("{}", field);
            tokens.extend(quote! { #on.#field });
        }
        CodeExpression::Call { on, params } => {
            let on = generate_code_expression(on, assignee);

            let params = params
                .iter()
                .map(|param| generate_code_expression(param, assignee));

            tokens.extend(quote! { #on( #( #params, )* ) });
        }
        CodeExpression::BinOp { op, left, right } => {
            let op = match op {
                BinaryOperator::EqEq => quote! { == },
                BinaryOperator::NotEq => quote! { != },
                BinaryOperator::Le => quote! { <= },
                BinaryOperator::Lt => quote! { < },
                BinaryOperator::Ge => quote! { >= },
                BinaryOperator::Gt => quote! { > },
                BinaryOperator::And => quote! { && },
                BinaryOperator::Or => quote! { || },
            };

            let left = generate_code_expression(left, assignee);
            let right = generate_code_expression(right, assignee);

            tokens.extend(quote! {
                #left #op #right
            });
        }
    }

    tokens.into_iter().collect()
}

fn generate_type_path(type_path: &TypePath) -> proc_macro2::TokenStream {
    let segments = type_path
        .segments
        .iter()
        .map(|segment| format_ident!("{}", segment));

    quote! { #( #segments )::* }
}

fn generate_pattern(pattern: &Pattern) -> proc_macro2::TokenStream {
    let mut tokens: Vec<proc_macro2::TokenTree> = vec![];

    match pattern {
        Pattern::Binding { name } => {
            let name = format_ident!("{}", name);
            tokens.extend(quote! { #name });
        }
        Pattern::Enum { type_path, fields } => {
            let type_path = generate_type_path(type_path);
            let fields = fields.iter().map(|pattern| generate_pattern(pattern));

            tokens.extend(quote! {
                #type_path (#( #fields,)*)
            });
        }
        Pattern::Struct { type_path, fields } => {
            let type_path = generate_type_path(type_path);

            let patterns = fields.iter().map(|(_, pattern)| generate_pattern(pattern));

            let bindings = fields
                .iter()
                .map(|(binding, _)| format_ident!("{}", binding));

            tokens.extend(quote! {
                #type_path { #( #bindings: #patterns,)* }
            });
        }
    }

    tokens.into_iter().collect()
}

fn generate_expression(expression: &Expression, assignee: &str) -> proc_macro2::TokenStream {
    let mut tokens: Vec<proc_macro2::TokenTree> = vec![];

    match expression {
        Expression::Literal(text) => tokens.extend(quote! {
            #text,
        }),
        Expression::CodeBlock(code_expr) => {
            let code_expr = generate_code_expression(code_expr, assignee);
            tokens.extend(quote! { #code_expr.to_string() });
        }
        Expression::For {
            binding,
            iterator,
            body,
        } => {
            let binding = format_ident!("{}", binding);
            let iterator = generate_code_expression(iterator, "self");

            let body = body.iter().map(|child| generate_expression(child, "self"));

            tokens.extend(quote! {
                {
                    let mut string = String::new();
                    for #binding in #iterator {
                        #( string.push_str(&#body); )*
                    }
                    string
                }
            });
        }
        Expression::If {
            condition,
            true_arm,
            false_arm,
        } => {
            let condition = generate_code_expression(condition, assignee);

            let true_arm = true_arm
                .iter()
                .map(|expr| generate_expression(expr, "self"));
            let false_arm = false_arm
                .iter()
                .map(|expr| generate_expression(expr, "self"));

            tokens.extend(quote! {
                {
                    let mut string = String::new();
                    if #condition {
                        #( string.push_str(&#true_arm) )*
                    } else {
                        #( string.push_str(&#false_arm) )*
                    }
                    string
                }
            });
        }
        Expression::IfLet {
            pattern,
            data,
            true_arm,
            false_arm,
        } => {
            let pattern = generate_pattern(pattern);
            let data = generate_code_expression(data, assignee);

            let true_arm = true_arm
                .iter()
                .map(|expr| generate_expression(expr, "self"));
            let false_arm = false_arm
                .iter()
                .map(|expr| generate_expression(expr, "self"));

            tokens.extend(quote! {
                {
                    let mut string = String::new();
                    if let #pattern = #data {
                        #( string.push_str(&#true_arm) )*
                    } else {
                        #( string.push_str(&#false_arm) )*
                    }
                    string
                }
            });
        }
        Expression::Match { data, arms } => {
            let data = generate_code_expression(data, assignee);
            let arms = arms.iter().map(|arm| generate_match_arm(arm, assignee));
            tokens.extend(quote! {
                {
                    let mut string = String::new();
                    match #data {
                        #( #arms )*
                    };
                    string
                }
            });
        }
    }

    tokens.into_iter().collect()
}

fn generate_match_arm(
    (pattern, expressions): &(Pattern, Vec<Expression>),
    assignee: &str,
) -> proc_macro2::TokenStream {
    let pattern = generate_pattern(pattern);

    let expressions = expressions
        .iter()
        .map(|expr| generate_expression(expr, assignee));

    quote! {
        #pattern => {
            #( string.push_str(&#expressions); )*
        },
    }
}

fn generate_slots(template: &alcova::Template) -> Result<proc_macro2::TokenStream, &'static str> {
    let mut tokens: Vec<proc_macro2::TokenTree> = vec![];

    for expression in &template.expressions {
        let value = generate_expression(expression, "self");
        match expression {
            Expression::Literal(_) => {
                tokens.extend(quote! {
                    liveview::Slot::Static(#value),
                });
            }
            Expression::CodeBlock(_)
            | Expression::For { .. }
            | Expression::If { .. }
            | Expression::IfLet { .. }
            | Expression::Match { .. } => {
                tokens.extend(quote! {
                    liveview::Slot::Dynamic(#value),
                });
            }
        }
    }

    Ok(tokens.into_iter().collect())
}

fn generate_changes(template: &alcova::Template) -> Result<proc_macro2::TokenStream, &'static str> {
    let mut tokens: Vec<proc_macro2::TokenTree> = vec![];

    for (i, expression) in template.expressions.iter().enumerate() {
        let deps = expression.get_dependencies();
        if deps.is_empty() {
            continue;
        }

        let new_value = generate_expression(expression, "self");

        let fields = deps.iter().map(|field| format_ident!("{}", field));

        let conditional = quote! {
            #( old_template.#fields != self.#fields )||*
        };

        tokens.extend(quote! {
            if #conditional {
                changes.push((#i, #new_value));
            }
        });
    }

    Ok(tokens.into_iter().collect())
}
