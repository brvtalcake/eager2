use proc_macro2::{Delimiter, Ident, Span, TokenStream};
use quote::quote;

use crate::state::{Mode, State};
use crate::utils::*;

pub fn eval(stream: TokenStream, eager: bool) -> TokenStream {
    let name = if eager { "eager" } else { "lazy" };

    #[cfg(feature = "trace_macros")]
    proc_macro_error2::emit_call_site_warning!("{} input: {}", name, stream);

    eval_helper(stream, eager, name)
}

pub fn eager_wrap(stream: TokenStream, name: &str) -> TokenStream {
    #[cfg(feature = "trace_macros")]
    proc_macro_error2::emit_call_site_warning!("{} input: {}", name, stream);

    let macro_name = Ident::new(name, Span::call_site());
    let stream = quote! {#macro_name!{#stream}};
    eval_helper(stream, true, name)
}

fn eval_helper(stream: TokenStream, eager: bool, _name: &str) -> TokenStream {
    // Check the caller mode
    let mode = Mode::eager(eager);
    let found_crate = find_crate();
    let eager_call_sigil = eager_call_sigil();
    // As with all eager-macros, first try the sigil
    // If we find one, then the caller was eager, so we can just
    // pick up where they left off
    let state = State::decode_from_stream(stream.clone(), |v| v);

    // Fallback to the case where the caller wasn't eager
    // in which case we start from scratch
    let state =
        state.unwrap_or_else(|_| State::new(Span::call_site(), Delimiter::Bracket, mode, stream));

    // Try to move the state as far forward as we can
    let output = match state.process(&found_crate) {
        // If we finished, just output the finished result
        Ok(processed) => quote! { #processed },
        // If we didn't finish it means we found an external
        // eager macro we need to call, pass control over to them
        // and record our state
        Err((state, eager_macro, stream)) => {
            quote! {
                #eager_macro{#eager_call_sigil [#state] #stream}
            }
        }
    };

    #[cfg(feature = "trace_macros")]
    proc_macro_error2::emit_call_site_warning!("{} output: {}", _name, output);

    output
}
