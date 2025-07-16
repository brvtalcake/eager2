use eager2::{eager, eager_macro_rules};
use proc_macro::TokenStream;

#[proc_macro]
pub fn do_something(input: TokenStream) -> TokenStream {
    input
}

macro_rules! test {
    ($lit:literal $($tokens:tt)*) => {
        $lit
    };
    (0𓊆eager2𓊇[$($__eager2_ident_hyR7dMdkMPcptU6h21dioFE3EhoLprgj:tt)*]$($tokens:tt)*) => {
        $crate::eager!{
            0𓊆eager2𓊇[$($__eager2_ident_hyR7dMdkMPcptU6h21dioFE3EhoLprgj)*]$($tokens)* $($tokens)*
        }
    };
    ($($tokens:tt)*) => {
        $($tokens)* $($tokens)*
    };
}

eager! { test! {
    a b
}}
