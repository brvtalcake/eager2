use proc_macro::{Ident, Punct, Spacing, Span};

use crate::{
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
