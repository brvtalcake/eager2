use proc_macro::{Ident, Punct, Spacing, Span};

use crate::{
    consts::{get_eager_2_pm_ident, get_path_sep},
    parse::IdentOrString,
    pm::{Delimiter, Group, Literal, ToTokens, TokenStream},
};
pub trait NextOr: Iterator {
    fn next_or<E>(&mut self, e: E) -> Result<Self::Item, E>;
}

impl<I: Iterator> NextOr for I {
    fn next_or<E>(&mut self, e: E) -> Result<Self::Item, E> {
        self.next().ok_or(e)
    }
}

pub trait PopNext {
    type Item;
    fn pop_next(&mut self) -> Option<Self::Item>;
}

impl<T> PopNext for Vec<T>
where
    T: Iterator,
{
    type Item = T::Item;
    fn pop_next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(n) = self.last_mut()?.next() {
                return Some(n);
            }
            self.pop();
        }
    }
}

#[must_use]
pub(crate) fn lit_string(s: &str) -> TokenStream {
    Literal::string(s).into_token_stream()
}

#[must_use]
pub(crate) fn punct_alone(ch: char) -> TokenStream {
    Punct::new(ch, Spacing::Alone).into_token_stream()
}

#[must_use]
pub(crate) fn punct_joint(ch: char) -> TokenStream {
    Punct::new(ch, Spacing::Joint).into_token_stream()
}

#[must_use]
pub(crate) fn ident_call_site(ident: &str) -> TokenStream {
    Ident::new(ident, Span::call_site()).into_token_stream()
}

#[must_use]
pub(crate) fn braced<T>(collection: impl IntoIterator<Item = T>) -> TokenStream
where
    TokenStream: Extend<T>,
{
    let mut stream = TokenStream::new();
    stream.extend(collection);
    Group::new(Delimiter::Brace, stream).into_token_stream()
}

#[must_use]
pub(crate) fn bracketed<T>(collection: impl IntoIterator<Item = T>) -> TokenStream
where
    TokenStream: Extend<T>,
{
    let mut stream = TokenStream::new();
    stream.extend(collection);
    Group::new(Delimiter::Bracket, stream).into_token_stream()
}

#[must_use]
pub(crate) fn parenthesized<T>(collection: impl IntoIterator<Item = T>) -> TokenStream
where
    TokenStream: Extend<T>,
{
    let mut stream = TokenStream::new();
    stream.extend(collection);
    Group::new(Delimiter::Parenthesis, stream).into_token_stream()
}

#[must_use]
pub(crate) fn lifetime(name: IdentOrString) -> TokenStream {
    let mut stream = TokenStream::new();
    punct_joint('\'').to_tokens(&mut stream);
    ident_call_site(
        match name {
            IdentOrString::Ident(i) => i.to_string(),
            IdentOrString::String(s) => s,
        }
        .as_str(),
    )
    .to_tokens(&mut stream);
    stream
}

#[must_use]
pub fn eager_data(
    eager_call_sigil: &Literal,
    state: TokenStream,
    tail: TokenStream,
) -> TokenStream {
    let mut tokens = TokenStream::new();
    eager_call_sigil.to_tokens(&mut tokens);
    Group::new(Delimiter::Bracket, state).to_tokens(&mut tokens);
    tokens.extend(tail);
    tokens
}

#[must_use]
pub(crate) fn decl(
    mutable: bool,
    ident: impl ToTokens,
    value: impl ToTokens,
    ty: Option<impl ToTokens>,
) -> TokenStream {
    let mut stream = TokenStream::new();
    stream.extend([
        ident_call_site("let"),
        if mutable {
            ident_call_site("mut")
        } else {
            TokenStream::new()
        },
        ident.into_token_stream(),
        if let Some(ty) = ty {
            let mut tmp = TokenStream::new();
            tmp.extend([punct_alone(':'), ty.into_token_stream()]);
            tmp
        } else {
            TokenStream::new()
        },
        punct_alone('='),
        value.into_token_stream(),
        punct_alone(';'),
    ]);
    stream
}

#[must_use]
pub(crate) fn path(
    with_initial_sep: bool,
    components: impl IntoIterator<Item: ToTokens>,
) -> TokenStream {
    let sep = get_path_sep();
    let mut stream = if with_initial_sep {
        sep.clone()
    } else {
        TokenStream::new()
    };
    stream.extend(
        components
            .into_iter()
            .map(ToTokens::into_token_stream)
            .intersperse_with(|| sep.clone()),
    );
    stream
}

trait ChainMethod: ToTokens + Sized {
    fn chain_method(
        self,
        method: TokenStream,
        args: impl IntoIterator<Item: ToTokens>,
    ) -> TokenStream {
        let mut stream = Group::new(Delimiter::None, self.into_token_stream()).into_token_stream();
        stream.extend([
            punct_alone('.'),
            method.into_token_stream(),
            parenthesized(
                args.into_iter()
                    .map(ToTokens::into_token_stream)
                    .intersperse(punct_alone(',')),
            ),
        ]);
        stream
    }
}
impl<T: ToTokens + Sized> ChainMethod for T {}

pub(crate) trait Function: ToTokens + Sized {
    fn funcall(self, args: impl IntoIterator<Item: ToTokens>) -> TokenStream {
        let mut stream = Group::new(Delimiter::None, self.into_token_stream()).into_token_stream();
        parenthesized(
            args.into_iter()
                .map(ToTokens::into_token_stream)
                .intersperse(punct_alone(',')),
        )
        .to_tokens(&mut stream);
        stream
    }
}

impl<T: ToTokens + Sized> Function for T {}

#[must_use]
#[inline]
pub(crate) fn make_eager_proc_macro_body(
    eager_call_sigil: &TokenStream,
    input: &Ident,
    input_type: &TokenStream,
    output_type: &TokenStream,
    user_input: &Ident,
    user_input_type: &TokenStream,
    user_output_type: &TokenStream,
    body: &Group,
) -> TokenStream {
    braced([
        // let <user-lambda> = ... ;
        decl(
            false,
            get_eager_2_pm_ident(Some("user_lambda")).into_token_stream(),
            [
                // | <user-input>: <user-input-type> | -> <user-output-type>
                punct_alone('|'),
                user_input.to_token_stream(),
                punct_alone(':'),
                user_input_type.clone(),
                punct_alone('|'),
                punct_joint('-'),
                punct_alone('>'),
                user_output_type.clone(),
                // { ... };
                body.to_token_stream(),
            ],
            None::<TokenStream>,
        ),
        // let mut <input-copy> = <input>.clone();
        decl(
            true,
            get_eager_2_pm_ident(Some("input_copy")),
            input
                .to_token_stream()
                .chain_method(ident_call_site("clone"), [] as [TokenStream; _]),
            Some(input_type.clone()),
        ),
        // 'eager_rule: { <test-if-eager-evaluating> }
        lifetime(IdentOrString::Ident(get_eager_2_pm_ident(Some(
            "eager_rule",
        )))),
        punct_alone(':'),
        braced([
            // let mut <iter> = <input>.into_iter();
            decl(
                true,
                get_eager_2_pm_ident(Some("iter")),
                input
                    .to_token_stream()
                    .chain_method(ident_call_site("into_iter"), [] as [TokenStream; _]),
                None::<TokenStream>,
            ),
        ]),
    ])
}
