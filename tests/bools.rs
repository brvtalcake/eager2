use eager2::{eager, eager_macro_rules};

eager_macro_rules! {
    macro_rules! test_macro{
        {1} =>{ 1 };
    }
}

#[test]
fn eq() {
    assert!(eager! {eq!{{a}, {a}}});
    assert!(!eager! {eq!{{a}, {b}}});
    assert!(eager! {eq!{{lazy!{1}}, {test_macro!{1}}}});
    assert!(!eager! {eq!{{lazy!{test_macro!{1}}}, {test_macro!{1}}}});
}

#[test]
fn iif() {
    let v = 1;
    eager! {iif!(
        eq!{{a}, {a}},
        {let v = v*10;},
        {asdf * / 4}
    )};
    assert_eq!(v, 10);
    eager! {iif!(
        eq!{{a}, {b}},
        {asdf * / 4},
        {let v = v*10;},
    )};
    assert_eq!(v, 100);
}
