use proc_macro2::{Delimiter, Span, TokenStream};
use quote::quote;

use crate::state::{Mode, State};
use crate::utils::*;

pub fn eager(stream: TokenStream) -> TokenStream {
    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!("eager input: {}", stream);

    let found_crate = find_crate();
    let eager_call_sigil = eager_call_sigil();

    let state = State::decode_from_stream(stream.clone())
        .unwrap_or_else(|_| State::new(Span::call_site(), Delimiter::Bracket, Mode::Eager, stream));
    let output = match state.process(&found_crate) {
        Ok(processed) => quote! { #processed },
        Err((state, eager_macro, stream)) => {
            quote! {
                #eager_macro{#eager_call_sigil [#state] #stream}
            }
        }
    };

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!("eager output: {}", output);

    output
}

pub fn lazy(stream: TokenStream) -> TokenStream {
    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!("lazy input: {}", stream);

    let found_crate = find_crate();
    let eager_call_sigil = eager_call_sigil();
    let state = State::decode_from_stream(stream.clone())
        .unwrap_or_else(|_| State::new(Span::call_site(), Delimiter::Bracket, Mode::Lazy, stream));
    let output = match state.process(&found_crate) {
        Ok(processed) => quote! { #processed },
        Err((state, eager_macro, stream)) => {
            quote! {
                #eager_macro{#eager_call_sigil [#state] #stream}
            }
        }
    };

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!("lazy output: {}", output);

    output
}
