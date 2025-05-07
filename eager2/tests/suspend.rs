use eager2::{eager, eager_macro, lazy, suspend_eager};

#[eager_macro]
macro_rules! fn_body {
    ()=>{ foo() {} };
}
#[test]
fn build_fn() {
    eager! {
        #[doc = "just a function"]
        suspend_eager!{eager!{fn fn_body!()}}
    }
    foo();
}

#[test]
fn build_fn2() {
    lazy! {
        #[doc = "just a function"]
        suspend_eager!{eager!{fn fn_body!()}}
    }
    foo();
}
