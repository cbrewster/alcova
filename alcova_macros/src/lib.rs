extern crate proc_macro;

use alcova::{parse_template, CodeExpression, Expression, Parser};
use proc_macro::{TokenStream, TokenTree};
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
                liveview::RenderedTemplate {
                    slots: vec![
                        #slots
                    ]
                }
            }

            fn changes(&self, old_template: &Self) -> liveview::Changes {
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
        CodeExpression::Symbol { name, assigned } => {
            let name = format_ident!("{}", name);
            let name = if *assigned {
                let assign = format_ident!("{}", assignee);
                quote! { #assign.#name }
            } else {
                quote! { #name }
            };

            tokens.extend(quote! { #name })
        }
        CodeExpression::Accessor { on, field } => {
            let on = generate_code_expression(on, assignee);
            let field = format_ident!("{}", field);
            tokens.extend(quote! { #on.#field });
        }
        CodeExpression::Call { on, params } => {
            let on = generate_code_expression(on, assignee);
            let mut param_tokens = vec![];
            for param in params {
                let param = generate_code_expression(param, assignee);
                param_tokens.extend(quote! { &#param, });
            }
            let params: proc_macro2::TokenStream = param_tokens.into_iter().collect();

            tokens.extend(quote! { #on(#params) });
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
            let mut body_tokens = vec![];

            for child in body {
                let child = generate_expression(child, "self");
                body_tokens.extend(quote! {
                    string.push_str(&#child);
                });
            }

            let body: proc_macro2::TokenStream = body_tokens.into_iter().collect();

            tokens.extend(quote! {
                {
                    let mut string = String::new();
                    for #binding in &#iterator {
                        #body
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
            let mut true_arm_tokens = vec![];

            for child in true_arm {
                let child = generate_expression(child, "self");
                true_arm_tokens.extend(quote! {
                    string.push_str(&#child);
                });
            }

            let true_arm: proc_macro2::TokenStream = true_arm_tokens.into_iter().collect();

            let mut false_arm_tokens = vec![];

            for child in false_arm {
                let child = generate_expression(child, "self");
                false_arm_tokens.extend(quote! {
                    string.push_str(&#child);
                });
            }

            let false_arm: proc_macro2::TokenStream = false_arm_tokens.into_iter().collect();

            tokens.extend(quote! {
                {
                    let mut string = String::new();
                    if #condition {
                        #true_arm
                    } else {
                        #false_arm
                    }
                    string
                }
            });
        }
    }

    tokens.into_iter().collect()
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
            Expression::CodeBlock(_) | Expression::For { .. } | Expression::If { .. } => {
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

        let mut conditional_tokens = vec![];
        for (i, dep) in deps.iter().enumerate() {
            let field = format_ident!("{}", dep);
            conditional_tokens.extend(quote! {old_template.#field != self.#field});

            if i != deps.len() - 1 {
                conditional_tokens.extend(quote! { || });
            }
        }
        let conditional: proc_macro2::TokenStream = conditional_tokens.into_iter().collect();
        let new_value = generate_expression(expression, "self");
        tokens.extend(quote! {
            if #conditional {
                changes.push((#i, #new_value));
            }
        });
    }

    Ok(tokens.into_iter().collect())
}
