//!
//! This crate contains four main macros used to simulate eager macro expansion:
//!
//! * [`eager!`]: Attempts to eagerly expands almost every macro invocation in its body.
//! * [`eager_macro_rules!`]: Used to declare macro that can be eagerly expanded with `eager!`.
//! * [`lazy!`]: Used in `eager!` to revert to lazy macro expansion.
//! * [`suspend_eager!`]: Like `lazy!`, but completely suspends expansion.
//!
//! In addition to these primary macros, some eager versions of standard library are provided:
//! `compile_error!`, `concat!`, `env!`, `include!`, `include_str!`, `stringify!`. If you wish to
//! use the lazy versions you can either insert them inside a `lazy!{}` block, or use the full path
//! (e.g. `std::concat`).
//!
//! # Usage
//!
//! ```
//! use eager2::{eager_macro_rules, eager};
//!
//! //Declare an eager macro
//! eager_macro_rules!{
//!     macro_rules! plus_1{
//!         ()=>{+ 1};
//!     }
//! }
//!
//! // Use the macro inside an eager! call to expand it eagerly
//! assert_eq!(4, eager!{2 plus_1!() plus_1!()});
//! ```
//!
//!
//! Expanding on this, some additional helpers are provided
//!
//! * `ccase!`: Modifies the case of strings and idents.
//! * `eager_if!`: Selects between two streams of tokens.
//! * `token_eq!`: Compares two trees of tokens for equality.
//! * `unstringify!`: Parses a string into a stream of tokens.
//!
//! See each macro's documentation for details.
//!

use proc_macro_error2::proc_macro_error;

mod egroup;
mod exec;
mod impls;
mod rules;
mod state;
mod utils;

/// [[eager!](macro.eager.html)] Declares [eager!](macro.eager.html)-enabled macros.
///
/// ## Syntax
/// ```
/// macro_rules! eager_macro_rules {
///     ($($dollar:tt $id_1:ident)?
///     $(
///         $(#[$($metas:meta)*])*
///         macro_rules! $macro_name:ident {
///             $($rules:tt => $expansions:tt);+ $(;)?
///         }
///     )*) => { ... };
/// }
/// ```
/// # Usage
///
/// Wraps the usual `macro_rules!` syntax. First an identifier may be given, preceded by '$'.
/// Then any number of macro declarations can be given using the usual `macro_rules!` syntax.
/// Documentation and attributes are also given in the
/// usual way just before each `macro_rules!`, i.e. inside `eager_macro_rules!`.
///
/// Some restrictions apply to the `macro_rules!` declarations:
///
/// * `macro_rules!` should avoid using identifiers that start with `__eager2_ident_`. This is
/// because in order to make macros eager, `eager_macro_rules` needs to add it's own unique
/// fragment identifier that does conflict with any of those present in the `macro_rules`. If
/// absoultely necessary, it is possible to override this identifier by providing an alternative,
/// prefixed with the token `$` as the first tokens to `eager_macro_rules`.
/// * No macros should be called with, nor should expansions emit the literal `0𓊆eager2𓊇` as their
/// first token, as this could conflict with the way eager expansion is preserved.
///
/// # `eager!`-enabling example
///
/// [eager!](macro.eager.html)-enabling the following macro:
/// ```
/// /// Some documentation
/// #[macro_export]
/// macro_rules! some_macro{
/// 	()=>{};
/// }
/// ```
/// is done by wrapping it in `eager_macro_rules!` as follows:
/// ```
/// use eager2::eager_macro_rules;
///
/// eager_macro_rules!{
/// 	/// Some documentation
///     #[macro_export]
///     macro_rules! some_macro{
/// 	    ()=>{};
///     }
/// }
/// ```
/// where `()=>{};` is the list of rules that comprise the macro.
#[proc_macro]
#[proc_macro_error]
pub fn eager_macro_rules(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    rules::eager_macro_rules(stream.into()).into()
}

/// [[eager!](macro.eager.html)] Emulates eager expansion of macros.
///
/// # Example
///
/// ```
/// use eager2::{eager_macro_rules, eager};
///
/// //Declare an eager macro
/// eager_macro_rules!{
///     macro_rules! plus_1{
///         ()=>{+ 1};
///     }
/// }
///
/// fn main(){
/// 	// Use the macro inside an eager! call to expand it eagerly
/// 	assert_eq!(4, eager!{2 plus_1!() plus_1!()});
/// }
/// ```
///
/// # Usage
///
/// `eager!` can wrap any code, and if that code contains a macro call, that macro will be
/// expanded before its consumer. This means:
///
/// * If a macro call is given as an argument to another macro, the first macro will be expanded
/// first.
/// * All macros will be fully expanded before `eager!` expands. Therefore, otherwise illegal
/// intermediate expansion steps are possible.
///
/// `eager!` does not work with any macro; only macros declared using [`eager_macro_rules!`] may be
/// used. Such macros are said to be `eager!`-enabled.
///
/// To facilitate the use of non-`eager!`-enabled macros inside an `eager!` call,
/// a [`lazy!`] block can be inserted. Everything inside the `lazy!` block will be lazily expanded,
/// while everything outside it will continue to be eagerly expanded. Since, `lazy!` reverts
/// to the usual rules for macro expansion, an `eager!` block can be inserted inside the `lazy!`
/// block, to re-enable eager expansion for some subset of it.
///
/// [`eager_macro_rules!`]: macro.eager_macro_rules.html
/// [`lazy!`]: macro.lazy.html
///
/// # Cons
///
/// * Because of the way `eager!` is implemented - still using recursive macros - the compiler's
/// default macro recursion limit can be exceeded. So occasionally you may have to use
/// `#![recursion_limit="256"]` or higher.
///
/// * Debugging an eagerly expanded macro can be quite difficult. [`compile_error!`] can sometimes
/// be helpful in this regard. This crate also has `trace_macros` feature which only operates on `nightly`,
/// and can be quite verbose. Contributions to improve this are very welcome.
///
/// * Only `eager!`-enabled macros can be eagerly expanded, so existing macros do not gain much.
/// The `lazy!` block alleviates this a bit, by allowing the use of existing macros in it, while
/// eager expansion can be done around them. Luckily, `eager!`-enabling an existing macro should
/// not be too much trouble using [`eager_macro_rules!`].
///
/// ---
/// # Macro expansions
///
/// Rust is lazy when it comes to macro expansion. When the compiler sees a macro call, it will
/// try to expand the macro without looking at its arguments or what the expansion becomes.
/// Using `eager!`, previously illegal macro expansions can be made possible.
///
/// The following is a non-exhaustive list of illegal macro patterns that can be used with `eager!`.
///
/// ### The arguments to a macro usually cannot be the resulting expansion of another macro call:
/// Say you have a macro that adds two numbers:
/// ```ignore
/// macro_rules! add{
///     ($e1:expr, $e2:expr)=> {$e1 + $e2}
/// }
/// ```
/// And a macro that expands to two comma-separated numbers:
///
/// ```ignore
/// macro_rules! two_and_three{
///     ()=>{2,3}
/// }
/// ```
///
/// You cannot use the expansion of `two_and_three!` as an argument to `add!`:
/// ```ignore
/// let x = add!(two_and_three!()); // error
/// ```
/// The compiler will complain about no rule in `add!` accepting `two_and_three`, since it does not
/// get expanded before the `add!`, who requires two expressions and not just one.
///
/// With eager expansion, this can be made possible:
/// ```
/// use eager2::{eager_macro_rules, eager};
///
/// eager_macro_rules!{
///     macro_rules! add{
///         ($e1:expr, $e2:expr)=> {$e1 + $e2}
///     }
///
///     macro_rules! two_and_three{
///     	()=>{2,3}
///     }
/// }
///
/// fn main(){
/// 	let x = eager!{add!(two_and_three!())};
/// 	assert_eq!(5, x);
/// }
/// ```
///
/// ### Macros are illegal in some contexts (e.g. as an identifier)
///
/// Say you have a macro that expands to an identifier:
/// ```ignore
/// macro_rules! id{
///     ()=> {SomeStruct}
/// }
/// ```
/// And want to use it to declare a struct:
/// ```ignore
/// struct id!(){
///     v: u32
/// }
/// ```
///
/// This will not compile since macros are illegal in identifier position. The compiler does
/// not check whether the expansion of the macro will result in valid Rust code.
///
/// With eager expansion, `id!` will expand before the `eager!` block , making it possible to use it
/// in an identifier position:
/// ```
/// use eager2::{eager_macro_rules, eager};
///
/// eager_macro_rules!{
///     macro_rules! id{
///         ()=> {SomeStruct}
/// 	}
/// }
///
/// eager!{
///     struct id!(){
///         v: u32
///     }
/// }
///
/// fn main(){
/// 	let some_struct = SomeStruct{v: 4};
///     assert_eq!(4, some_struct.v);
/// }
/// ```
/// To circumvent any restriction on where macros can be used, we can therefore just wrap
/// the code surrounding the macro call with `eager!`. The `eager!` must still be in a valid position,
/// but in the worst case it can be put around the whole item
/// (struct, trait, implement, function, etc.).
///
///
/// ### No intermediate expansion step can include invalid syntax
///
/// Say we want to create a macro that interprets natural language, converting it into an expression.
///
/// We start by declaring a macro that interprets operator words:
/// ```ignore
/// macro_rules! op{
///     ( plus ) => { + };
///     ( minus ) => { - };
/// }
/// ```
///
/// We then declare a macro that interprets integer words:
/// ```ignore
/// macro_rules! integer{
///     ( one ) => { 1 };
///     ( two ) => { 2 };
/// }
/// ```
///
/// Lastly, we declare the top-level macro that uses the previous two macros to
/// expand into an expression:
/// ```ignore
/// macro_rules! calculate{
///     ( $lhs:tt $op:tt $rhs:tt ) => {
///          integer!{$lhs} op!{$op} integer!{$rhs}
///     };
/// }
/// ```
///
/// Using this macro will fail to compile:
/// ```ignore
/// let x = calculate!(one plus two); //Error
/// ```
///
/// Looking at the first expansion step we can see that three macro calls in a sequence
/// are not a valid expression:
/// ```ignore
/// let x = integer!(one) op!{plus} integer!(two); //Error
/// ```
///
/// We can circumvent this restriction, by having `calculate!` wrap its output in an `eager!`:
///
/// ```
/// use eager2::{eager_macro_rules, eager};
///
/// eager_macro_rules!{
///     macro_rules! op{
///         ( plus ) => { + };
///         ( minus ) => { - };
///     }
///
///     macro_rules! integer{
///         ( one ) => { 1 };
///         ( two ) => { 2 };
///     }
///
///     macro_rules! calculate{
///         ( $lhs:tt $op:tt $rhs:tt ) => {
///              eager!{integer!{$lhs} op!{$op} integer!{$rhs}}
///         };
/// 	}
/// }
///
/// fn main(){
/// 	let x = calculate!(one plus two);
/// 	assert_eq!(3, x);
/// }
/// ```
/// In this case, `calculate!` does not actually have to be `eager!`-enabled, since it is not inserted
/// into an `eager!` block. Though - as per [the conventions](#conventions) - we do enable it such
/// that others may later use it inside an `eager!` block.
///
///
/// # Conventions
///
/// Since we expect the use of this macro to be broadly applicable, we propose the following
/// conventions for the Rust community to use, to ease interoperability.
///
/// ## Documentation
///
/// To make it clearly visible that a given macro is `eager!`-enabled, its short rustdoc description
/// must start with a pair of brackets, within which a link to the official `eager!` macro documentation
/// must be provided. The link's visible text must be 'eager!' and
/// the brackets must not be part of the link.
#[proc_macro]
#[proc_macro_error]
pub fn eager(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eval(stream.into(), true).into()
}

/// [[eager!](macro.eager.html)] Used within an [`eager!`](macro.eager.html) to revert to lazy expansion.
///
/// If this macro is called independently of `eager!`, it is equivalent to `eager!{lazy!{...}}`.
#[proc_macro]
#[proc_macro_error]
pub fn lazy(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eval(stream.into(), false).into()
}

/// [[eager!](macro.eager.html)] Used within an [`eager!`](macro.eager.html) to fully revert to lazy expansion.
///
/// If this macro is called independently of `eager!`, it simply expands to the tokens passed in.
#[proc_macro]
#[proc_macro_error]
pub fn suspend_eager(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    stream
}

/// [[eager!](macro.eager.html)] Causes compilation to fail with the given error message when encountered.
///
/// This macro should be used when a crate uses a conditional compilation strategy to provide
/// better error messages for erroneous conditions. It's the compiler-level form of [`panic!`],
/// but emits an error during *compilation* rather than at *runtime*.
#[proc_macro]
#[proc_macro_error]
pub fn compile_error(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "compile_error").into()
}

/// [[eager!](macro.eager.html)] Concatenates literals into a static string slice.
///
/// This macro takes any number of comma-separated literals, yielding an
/// expression of type `&'static str` which represents all of the literals
/// concatenated left-to-right.
///
/// Integer and floating point literals are [stringified](core::stringify) in order to be
/// concatenated.
#[proc_macro]
#[proc_macro_error]
pub fn concat(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "concat").into()
}

/// [[eager!](macro.eager.html)] Inspects an environment variable at compile time.
///
/// This macro will expand to the value of the named environment variable at
/// compile time, yielding an expression of type `&'static str`
#[proc_macro]
#[proc_macro_error]
pub fn env(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "env").into()
}

/// [[eager!](macro.eager.html)] Optionally inspects an environment variable at compile time.
///
/// If the named environment variable is present at compile time, this will expand into an
/// expression of type `Option<&'static str>` whose value is `Some` of the value of the environment
/// variable (a compilation error will be emitted if the environment variable is not a valid Unicode
/// string). If the environment variable is not present, then this will expand to `None``.
#[proc_macro]
#[proc_macro_error]
pub fn option_env(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "option_env").into()
}

/// [[eager!](macro.eager.html)] Parses a file as an expression or an item according to the context.
/// The included file is placed in the surrounding code
/// [unhygienically](https://doc.rust-lang.org/reference/macros-by-example.html#hygiene). If
/// the included file is parsed as an expression and variables or functions share names across
/// both files, it could result in variables or functions being different from what the
/// included file expected.
///
/// The included file is located relative to the current file (similarly to how modules are
/// found). The provided path is interpreted in a platform-specific way at compile time. So,
/// for instance, an invocation with a Windows path containing backslashes `\` would not
/// compile correctly on Unix.
#[proc_macro]
#[proc_macro_error]
pub fn include(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "include").into()
}
/// [[eager!](macro.eager.html)] Includes a file as a reference to a byte array.
///
/// The file is located relative to the current file (similarly to how
/// modules are found). The provided path is interpreted in a platform-specific
/// way at compile time. So, for instance, an invocation with a Windows path
/// containing backslashes `\` would not compile correctly on Unix.
///
/// This macro will yield an expression of type &'static [u8; N] which is the contents of the file.
#[proc_macro]
#[proc_macro_error]
pub fn include_bytes(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "include_bytes").into()
}

/// [[eager!](macro.eager.html)] Includes a UTF-8 encoded file as a string.
///
/// The file is located relative to the current file (similarly to how
/// modules are found). The provided path is interpreted in a platform-specific
/// way at compile time. So, for instance, an invocation with a Windows path
/// containing backslashes `\` would not compile correctly on Unix.
///
/// This macro will yield an expression of type `&'static str` which is the
/// contents of the file.
#[proc_macro]
#[proc_macro_error]
pub fn include_str(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "include_str").into()
}

/// [[eager!](macro.eager.html)] Stringifies its arguments.
///
/// This macro will yield an expression of type `&'static str` which is the
/// stringification of all the tokens passed to the macro. No restrictions
/// are placed on the syntax of the macro invocation itself.
///
/// Note that the expanded results of the input tokens may change in the
/// future. You should be careful if you rely on the output.
#[proc_macro]
#[proc_macro_error]
pub fn stringify(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "stringify").into()
}

/// [[eager!](macro.eager.html)] Convert strings between snake case, kebab case, camel case,
/// title case, pascal case, and so many more.
#[proc_macro]
#[proc_macro_error]
pub fn ccase(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "ccase").into()
}

/// [[eager!](macro.eager.html)] Conditionally expands into one of two blocks.
#[proc_macro]
#[proc_macro_error]
pub fn eager_if(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "eager_if").into()
}

/// [[eager!](macro.eager.html)] Expands into the token `true` if two arguments
/// are token-wise equivalent
#[proc_macro]
#[proc_macro_error]
pub fn token_eq(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "token_eq").into()
}

/// [[eager!](macro.eager.html)] Parses a string as tokens.
#[proc_macro]
#[proc_macro_error]
pub fn unstringify(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impls::eager_wrap(stream.into(), "unstringify").into()
}
