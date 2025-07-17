#[cfg(test)]
mod test {
    use eager2::eager;
    use dummy::add;

    #[test]
    fn it_works() {
        assert_eq!(
            eager! {
                0 add!(1)
            },
            1
        );
        assert_eq!(
            eager! {
                0 add!(1) add!(1) add!(1)
            },
            3
        );

        assert_eq!(
            eager! {
                0 add!(2)
            },
            2
        );
        assert_eq!(
            eager! {
                0 add!(2) add!(2) add!(2)
            },
            6
        );
    }
}
