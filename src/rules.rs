use std::iter::Peekable;

use proc_macro2::{token_stream, Delimiter, Group, Ident, Spacing, Span, TokenStream, TokenTree};
use proc_macro_error2::{abort, abort_call_site, ResultExt};
use quote::quote;

#[allow(clippy::wildcard_imports)]
use crate::{state::State, utils::*};

fn expand_rule(
    span: Span,
    hidden_ident: &Ident,
    found_crate: &TokenStream,
    eager_call_sigil: &TokenTree,
    stream: &mut Peekable<token_stream::IntoIter>,
) -> TokenStream {
    struct Rule {
        grammer: Group,
        expansion: Group,
    }

    let mut metas = vec![];
    loop {
        match stream.peek() {
            None => abort_call_site!(
                "unexpected end of macro invocation";
                note="while trying to match token `#` or ident `macro_rules`"),
            Some(TokenTree::Punct(p)) if p.as_char() == '#' => {
                let pound = stream.next().unwrap();
                metas.push(pound);

                let g = expect_group(stream.next_or(span), Delimiter::Bracket).unwrap_or_abort();
                metas.push(g.into());
            }
            Some(TokenTree::Ident(i)) if i == "macro_rules" => break,
            Some(t) => abort!(t, "expected token `#` or or ident `macro_rules`"),
        }
    }
    let _macro_rules = stream.next().unwrap();
    expect_punct(stream.next_or(span), '!').unwrap_or_abort();
    let macro_name =
        expect_ident(stream.next_or(span), Param::Named("macro_name")).unwrap_or_abort();

    let group = expect_group(stream.next_or(span), Delimiter::Brace).unwrap_or_abort();

    let mut rules = vec![];
    let span = group.span();
    let mut stream = group.stream().into_iter();
    loop {
        let Some(tt) = stream.next() else { break };
        let grammer = expect_group(Ok(tt), Param::Named("grammer")).unwrap_or_abort();

        // Arrow
        expect_punct(stream.next_or(span), ('=', Spacing::Joint)).unwrap_or_abort();
        expect_punct(stream.next_or(span), '>').unwrap_or_abort();

        let expansion =
            expect_group(stream.next_or(span), Param::Named("expansion")).unwrap_or_abort();

        rules.push(Rule { grammer, expansion });

        let Some(tt) = stream.next() else { break };
        expect_punct(Ok(tt), ';').unwrap_or_abort();
    }

    let eager_rules = rules.iter().map(|Rule { grammer, expansion }| {
        let grammer = grammer.stream();
        let expansion = expansion.stream();
        quote! {
            (
                #eager_call_sigil[$($#hidden_ident:tt)*]
                #grammer
            ) => {
                #found_crate::eager!{
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

    quote! {
        #(#metas)*
        macro_rules! #macro_name{
            #(#eager_rules)*
            // Put the pure version after so eager is always tried first
            #(#pure_rules)*
        }
    }
}

fn expand_rules_legacy(
    found_crate: &TokenStream,
    eager_call_sigil: &TokenTree,
    stream: token_stream::IntoIter,
) -> TokenStream {
    let mut stream = stream.peekable();

    let span = Span::call_site();

    let hidden_ident = match stream.peek() {
        None => return TokenStream::new(),

        Some(TokenTree::Punct(p)) if p.as_char() == '$' => {
            let _dollar = stream.next();
            expect_ident(stream.next_or(span), Param::Named("eager_ident")).unwrap_or_abort()
        }
        Some(_token) => get_eager_2_ident(),
    };

    let mut outputs = vec![];

    while stream.peek().is_some() {
        let output = expand_rule(
            span,
            &hidden_ident,
            found_crate,
            eager_call_sigil,
            &mut stream,
        );

        outputs.push(output);
    }

    let output = quote! { #(#outputs)* };

    output
}

pub fn eager_macro_rules(stream: TokenStream) -> TokenStream {
    let found_crate = find_crate();
    let eager_call_sigil = eager_call_sigil();
    let found_crate_path = crate_path(&found_crate);

    // As with all eager-macros, first try the sigil.
    // If we find one, then the caller was eager, so we can just
    // pick up where they left off
    let state = State::decode_from_stream(stream.clone(), |v| {
        expand_rules_legacy(&found_crate_path, &eager_call_sigil, v).into_iter()
    })
    .ok();

    let output = match state.map(|s| s.process(&found_crate)) {
        // If we were eager and finished, just output the finished result
        Some(Ok(processed)) => quote! { #processed },

        // If we were eager and didn't finish it means we found an external
        // eager macro we need to call, pass control over to them
        // and record our state
        Some(Err((state, eager_macro, stream))) => {
            quote! {
                #eager_macro{#eager_call_sigil [#state] #stream}
            }
        }
        None => expand_rules_legacy(&found_crate_path, &eager_call_sigil, stream.into_iter()),
    };

    #[cfg(feature = "trace_macros")]
    proc_macro_error2::emit_call_site_warning!("eager_macro_rules output: {}", output);

    output
}

pub fn eager_macro(attr: TokenStream, stream: TokenStream) -> TokenStream {
    let found_crate = find_crate();
    let eager_call_sigil = eager_call_sigil();
    let found_crate_path = crate_path(&found_crate);
    let span = Span::call_site();

    let hidden_ident = {
        let mut stream = attr.into_iter();
        let hidden_ident = match stream.next() {
            Some(attr) => expect_ident(Ok(attr), Param::Named("eager_ident")).unwrap_or_abort(),
            None => get_eager_2_ident(),
        };
        if stream.next().is_some() {
            abort!(span, "`eager_macro` only takes 1 input");
        }
        hidden_ident
    };

    let output = expand_rule(
        span,
        &hidden_ident,
        &found_crate_path,
        &eager_call_sigil,
        &mut stream.into_iter().peekable(),
    );

    #[cfg(feature = "trace_macros")]
    proc_macro_error2::emit_call_site_warning!("eager_macro output: {}", output);

    output
}
