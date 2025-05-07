use eager2::{eager, eager_macro};

#[eager_macro]
macro_rules! test_macro {
    ($v:ident) => {
        let $v = 0;
    };
    (delay_eager!{$(@ $_:tt)? 0, $v:ident}) => {
        let $v = 10;
    };
    (delay_eager!{$(@ $_:tt)? 1, $v:ident}) => {
        let $v = 20;
    };
}

#[test]
fn test() {
    eager! {
        lazy!{test_macro!}
        {
            delay_eager!{0, v}
        }
    }
    assert_eq!(v, 0);

    eager! {
        lazy!{test_macro!}
        {
            delay_eager!{1, v}
        }
    }
    assert_eq!(v, 10);

    eager! {
        lazy!{test_macro!}
        {
            delay_eager!{2, v}
        }
    }
    assert_eq!(v, 20);
}
