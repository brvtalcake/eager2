mod eager_column {
    use eager2::eager;

    #[test]
    fn test_column() {
        // TODO: `eager2::column!()` should return the **start**
        //       of the macro call. But when wrapped in `eager! { }`,
        //       it currently returns ...
        //       eager2::column!()
        //                      ^
        //                     this
        //       ... while it should return ...
        //       eager2::column!()
        //       ^
        //      this
        #[rustfmt::skip]
        const A: u32 =                 column!();
        const B: u32 = eager! { column!() };
        assert_eq!(A, B); // ???

        #[rustfmt::skip]
        assert_eq!(eager! {
            eager2::column!()
        },                 column!());
        #[rustfmt::skip]
        assert_eq!(eager! {
            eager2::column!()
        },                 std::column!());

        #[rustfmt::skip]
        assert_eq!(eager! {
            column!()
        },         column!());
        #[rustfmt::skip]
        assert_eq!(eager! {
            column!()
        },         std::column!());
    }
}
