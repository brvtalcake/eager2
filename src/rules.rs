use proc_macro_error2::{ResultExt, abort, abort_call_site};
use proc_macro2::{Delimiter, Group, Spacing, Span, TokenStream, TokenTree, token_stream};
use quote::quote;

use crate::{state::State, utils::*};

fn expand_rules(
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
        let mut metas = vec![];
        loop {
            match stream.peek() {
                None => abort_call_site!(
                    "unexpected end of macro invocation";
                    note="while trying to match token `#` or ident `macro_rules`"),
                Some(TokenTree::Punct(p)) if p.as_char() == '#' => {
                    let pound = stream.next().unwrap();
                    metas.push(pound);

                    let g =
                        expect_group(stream.next_or(span), Delimiter::Bracket).unwrap_or_abort();
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

        struct Rule {
            grammer: Group,
            expansion: Group,
        }

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
        expand_rules(&found_crate_path, &eager_call_sigil, v).into_iter()
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
        None => expand_rules(&found_crate_path, &eager_call_sigil, stream.into_iter()),
    };

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!("eager_macro_rules output: {}", output);

    output
}
