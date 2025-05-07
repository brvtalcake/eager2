use eager2_core::pm::{ToTokens, TokenStream};

use crate::init::init;

pub fn lazy_apply(attrs: TokenStream, item: TokenStream) -> TokenStream {
    init();

    #[cfg(feature = "trace_macros")]
    println!("lazy_apply input: ({}) {}", attrs, stream);

    let output = match eager2_core::funcs::lazy_apply(attrs, item) {
        Ok(output) => output,
        Err(err) => return err.into_token_stream(),
    };

    #[cfg(feature = "trace_macros")]
    println!("lazy_apply output: {}", output);

    output
}
