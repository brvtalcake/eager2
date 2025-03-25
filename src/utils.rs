use std::borrow::Cow;
use std::str::FromStr;

use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro_error2::abort;
use proc_macro_error2::{Diagnostic, Level, abort_call_site, diagnostic};
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Span, TokenStream, TokenTree};
use quote::quote;

pub const EAGER2_IDENT: &str = "__eager2_ident_hyR7dMdkMPcptU6h21dioFE3EhoLprgj";

pub const EAGER_CALL_SIGIL: &str = "0𓊆eager2𓊇";
pub const LAZY_SIGIL: &str = "𓆉";
pub const EAGER_SIGIL: &str = "𓂺";
pub const SIGIL_ERROR: &str = "expected 𓆉 or 𓂺";

pub fn find_crate() -> Cow<'static, str> {
    match crate_name("eager2") {
        Err(e) => abort_call_site!(
            "eager2 is not present in `Cargo.toml`";
            error = "{}", e;
        ),
        Ok(FoundCrate::Itself) => Cow::Borrowed("eager2"),
        Ok(FoundCrate::Name(name)) => Cow::Owned(name),
    }
}
pub fn find_crate_path() -> TokenStream {
    let found_crate = find_crate();
    let found_crate = Ident::new(&found_crate, Span::call_site());
    quote! {::#found_crate}
}

pub trait NextOr: Iterator {
    fn next_or<E>(&mut self, e: E) -> Result<Self::Item, E>;
}

impl<I: Iterator> NextOr for I {
    fn next_or<E>(&mut self, e: E) -> Result<Self::Item, E> {
        self.next().ok_or(e)
    }
}

pub fn eager_call_sigil() -> TokenTree {
    Literal::from_str(EAGER_CALL_SIGIL).unwrap().into()
}

pub enum Param<'a, V> {
    Named(&'a str),
    ExactValue(V),
}

impl<V> From<V> for Param<'_, V> {
    fn from(v: V) -> Self {
        Self::ExactValue(v)
    }
}

pub fn expect_punct(tt: Result<TokenTree, Span>, c: char) -> Result<Punct, Diagnostic> {
    match tt {
        Err(span) => Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match token `{}`", c)),
        Ok(TokenTree::Punct(p)) if p.as_char() == c => Ok(p),
        Ok(tt) => Err(diagnostic!(tt, Level::Error, "expected token: `{}`", c)),
    }
}

pub fn expect_ident<'a, 'b>(
    tt: Result<TokenTree, Span>,
    s: impl Into<Param<'a, &'b str>>,
) -> Result<Ident, Diagnostic> {
    match (tt, s.into()) {
        (Err(span), Param::ExactValue(s)) => Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match ident `{}`", s)),
        (Err(span), Param::Named(s)) => Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match ident `${}:ident`", s)),
        (Ok(TokenTree::Ident(i)), Param::ExactValue(s)) if i == s => Ok(i),
        (Ok(TokenTree::Ident(i)), Param::Named(_)) => Ok(i),
        (Ok(tt), Param::ExactValue(s)) => {
            Err(diagnostic!(tt, Level::Error, "expected ident: `{}`", s))
        }
        (Ok(tt), Param::Named(s)) => Err(diagnostic!(
            tt,
            Level::Error,
            "expected ident: `${}:ident`",
            s
        )),
    }
}

pub fn expect_group<'a>(
    tt: Result<TokenTree, Span>,
    d: impl Into<Param<'a, Delimiter>>,
) -> Result<Group, Diagnostic> {
    fn to_char(d: Delimiter) -> char {
        match d {
            Delimiter::Parenthesis => '(',
            Delimiter::Brace => '{',
            Delimiter::Bracket => '[',
            Delimiter::None => '∅',
        }
    }
    match (tt, d.into()) {
        (Err(span), Param::ExactValue(d)) => Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match ident `{}`", to_char(d))),
        (Err(span), Param::Named(g)) => Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match ident `${}:group`", g)),
        (Ok(TokenTree::Group(g)), Param::ExactValue(d)) if g.delimiter() == d => Ok(g),
        (Ok(TokenTree::Group(g)), Param::Named(_)) => Ok(g),
        (Ok(tt), Param::ExactValue(d)) => Err(diagnostic!(
            tt,
            Level::Error,
            "expected token: `{}`",
            to_char(d)
        )),
        (Ok(tt), Param::Named(g)) => Err(diagnostic!(
            tt,
            Level::Error,
            "expected one of '(', '[', or '{{ for `${}:group`",
            g
        )),
    }
}

pub fn expect_string_literal(tt: Result<TokenTree, Span>) -> Result<String, Diagnostic> {
    let tt = match tt {
        Err(span) => {
            return Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match string literal"));
        }
        Ok(TokenTree::Literal(l)) => {
            let string = l.to_string();
            if !string.starts_with('"') || !string.ends_with('"') {
                return Err(diagnostic!(l, Level::Error, "expected string literal"));
            }
            return Ok(string);
        }
        Ok(TokenTree::Group(g)) if g.delimiter() == Delimiter::None => {
            let mut stream = g.stream().into_iter();
            let tt = stream.next();
            if stream.next().is_none() {
                if let Ok(v) = expect_string_literal(tt.ok_or(g.span())) {
                    return Ok(v);
                }
            }
            TokenTree::Group(g)
        }
        Ok(tt) => tt,
    };
    Err(diagnostic!(
        tt,
        Level::Error,
        "expected string literal, found token `{}`",
        tt
    ))
}

pub fn expect_ident_or_string(
    tt: Result<TokenTree, Span>,
) -> Result<Result<Ident, String>, Diagnostic> {
    let tt = match tt {
        Err(span) => {
            return Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match ident or string literal `$input`"));
        }
        Ok(TokenTree::Ident(i)) => return Ok(Ok(i)),
        Ok(TokenTree::Literal(l)) => {
            let string = l.to_string();
            if !string.starts_with('"') || !string.ends_with('"') {
                abort!(l, "expected ident or string literal")
            }
            return Ok(Err(string));
        }
        Ok(TokenTree::Group(g)) if g.delimiter() == Delimiter::None => {
            let mut stream = g.stream().into_iter();
            let tt = stream.next();
            if stream.next().is_none() {
                if let Ok(v) = expect_ident_or_string(tt.ok_or(g.span())) {
                    return Ok(v);
                }
            }
            TokenTree::Group(g)
        }

        Ok(tt) => tt,
    };
    Err(diagnostic!(
        tt,
        Level::Error,
        "expected ident or string literal"
    ))
}

pub fn expect_literal<'a, 'b>(
    tt: Result<TokenTree, Span>,
    s: impl Into<Param<'a, &'b str>>,
) -> Result<Literal, Diagnostic> {
    match (tt, s.into()) {
        (Err(span), Param::ExactValue(s)) => Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match ident `{}`", s)),
        (Err(span), Param::Named(s)) => Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match ident `${}:ident`", s)),
        (Ok(TokenTree::Literal(l)), Param::ExactValue(s)) if l.to_string() == s => Ok(l),
        (Ok(TokenTree::Literal(l)), Param::Named(_)) => Ok(l),
        (Ok(tt), Param::ExactValue(s)) => {
            Err(diagnostic!(tt, Level::Error, "expected ident: `{}`", s))
        }
        (Ok(tt), Param::Named(s)) => Err(diagnostic!(
            tt,
            Level::Error,
            "expected ident: `${}:ident`",
            s
        )),
    }
}
