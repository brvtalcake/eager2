mod eager_line {
    use eager2::eager;

    #[test]
    fn test_line() {
        assert_eq!(eager! { eager2::line!() }, line!());
        assert_eq!(eager! { eager2::line!() }, line!());
    }
}
