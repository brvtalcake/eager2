mod test_concat {
    use eager2::eager;

    macro_rules! lazy_macro {
        ("success") => {
            1
        };
        ($($v:tt)*) => {
            0
        };
    }

    #[test]
    fn test() {
        let x = eager! {
            lazy!{
                lazy_macro!{
                    eager!{
                        concat!{"suc", "cess"}
                    }
                }
            }
        };
        let y = lazy_macro! {
            concat!{"suc", "cess"}
        };
        assert_eq!(1, x);
        assert_eq!(0, y);
    }
}
