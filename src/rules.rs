use proc_macro_error2::{abort, abort_call_site};
use proc_macro2::{Delimiter, Group, Ident, Spacing, Span, TokenStream, TokenTree};
use quote::quote;

use crate::utils::*;

pub fn eager_macro_rules(stream: TokenStream) -> TokenStream {
    let found_crate = find_crate();

    let mut stream = stream.into_iter().peekable();

    let hidden_ident = match stream.peek() {
        None => return TokenStream::new(),

        Some(TokenTree::Punct(p)) if p.as_char() == '$' => {
            let dollar = stream.next();
            match stream.next() {
                None => abort!(dollar, "expected ident after $"),
                Some(TokenTree::Ident(ident)) => ident,
                Some(_token) => abort!(_token, "expected ident after $"),
            }
        }
        Some(_token) => Ident::new(EAGER2_IDENT, Span::call_site()),
    };

    let mut outputs = vec![];

    while stream.peek().is_some() {
        let mut metas = vec![];
        loop {
            match stream.peek() {
                None => abort_call_site!("expected # or macro_rules"),
                Some(TokenTree::Punct(p)) if p.as_char() == '#' => {
                    let pound = stream.next().unwrap();
                    match stream.next() {
                        None => abort_call_site!("expected ["),
                        Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Bracket => {
                            metas.push(pound);
                            metas.push(TokenTree::Group(g));
                        }
                        Some(_token) => abort!(_token, "expected ["),
                    }
                }
                Some(TokenTree::Ident(i)) if i == "macro_rules" => break,
                Some(t) => abort!(t, "expected token `#` or `macro_rules`"),
            }
        }
        let _macro_rules = stream.next().unwrap();
        match stream.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == '!' => {}
            None => abort_call_site!("expected token `!`"),
            Some(t) => abort!(t, "expected token `!`"),
        }
        let macro_name = match stream.next() {
            Some(TokenTree::Ident(i)) => i,
            None => abort_call_site!("expected ident"),
            Some(t) => abort!(t, "expected ident"),
        };
        let group = match stream.next() {
            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => g,
            None => abort_call_site!("expected {{"),
            Some(t) => abort!(t, "expected {{"),
        };

        struct Rule {
            grammer: Group,
            expansion: Group,
        }

        let mut rules = vec![];
        let mut stream = group.stream().into_iter();
        loop {
            let grammer = match stream.next() {
                None => break,
                Some(TokenTree::Group(g)) => g,
                Some(t) => abort!(t, "expected {{ or [ or ("),
            };
            // Arrow
            match stream.next() {
                Some(TokenTree::Punct(p))
                    if p.as_char() == '=' && p.spacing() == Spacing::Joint => {}
                None => abort_call_site!("expected ="),
                Some(t) => abort!(t, "expected ="),
            }
            match stream.next() {
                Some(TokenTree::Punct(p)) if p.as_char() == '>' => {}
                None => abort_call_site!("expected >"),
                Some(t) => abort!(t, "expected >"),
            }
            let expansion = match stream.next() {
                None => break,
                Some(TokenTree::Group(g)) => g,
                Some(t) => abort!(t, "expected {{ or [ or ("),
            };
            match stream.next() {
                None => {
                    rules.push(Rule { grammer, expansion });
                    break;
                }
                Some(TokenTree::Punct(p)) if p.as_char() == ';' => {
                    rules.push(Rule { grammer, expansion });
                }
                Some(t) => abort!(t, "expected ;"),
            }
        }

        let eager_call_sigil = eager_call_sigil();
        let eager_rules = rules.iter().map(|Rule { grammer, expansion }| {
            let grammer = grammer.stream();
            let expansion = expansion.stream();
            quote! {
                (
                    #eager_call_sigil[$($#hidden_ident:tt)*]
                    #grammer
                ) => {
                    #found_crate::eager_internal!{
                        #eager_call_sigil[$($#hidden_ident)*]
                        #expansion
                    }
                };
            }
        });
        let pure_rules = rules.iter().map(|Rule { grammer, expansion }| {
            let grammer = grammer.stream();
            let expansion = expansion.stream();
            quote! {
                (
                    #grammer
                ) => {
                    #expansion
                };
            }
        });

        let output = quote! {
            #(#metas)*
            macro_rules! #macro_name{
                #(#eager_rules)*
                // Put the pure version after so eager is always tried first
                #(#pure_rules)*
            }
        };

        outputs.push(output);
    }

    let output = quote! { #(#outputs)* };

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning! {"eager_macro_rules output: {}", output}

    output
}
