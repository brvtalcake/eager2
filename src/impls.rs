use std::path::{self, Path, PathBuf};
use std::str::FromStr;
use std::{env, fs, mem};

use dyn_clone::DynClone;
use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro_error2::{Diagnostic, Level, ResultExt, diagnostic};
use proc_macro_error2::{abort, abort_call_site};
use proc_macro2::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree, token_stream,
};
use quote::{ToTokens, TokenStreamExt, quote};

const EAGER_CALL_SIGIL: &str = "0𓊆eager2𓊇";
const LAZY_SIGIL: &str = "𓆉";
const EAGER_SIGIL: &str = "𓂺";
const SIGIL_ERROR: &str = "expected 𓆉 or 𓂺";

const EAGER2_IDENT: &str = "__eager2_ident_hyR7dMdkMPcptU6h21dioFE3EhoLprgj";

enum Param<'a, V> {
    Named(&'a str),
    ExactValue(V),
}

impl<V> From<V> for Param<'_, V> {
    fn from(v: V) -> Self {
        Self::ExactValue(v)
    }
}

fn expect_punct(tt: Result<TokenTree, Span>, c: char) -> Result<Punct, Diagnostic> {
    match tt {
        Err(span) => Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match token `{}`", c)),
        Ok(TokenTree::Punct(p)) if p.as_char() == c => Ok(p),
        Ok(tt) => Err(diagnostic!(tt, Level::Error, "expected token: `{}`", c)),
    }
}

fn expect_ident<'a, 'b>(
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

fn expect_group<'a>(
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
fn expect_string_literal(tt: Result<TokenTree, Span>) -> Result<String, Diagnostic> {
    match tt {
        Err(span) => Err(diagnostic!(
            span, Level::Error, "unexpected end of macro invocation";
            note = "while trying to match string literal")),
        Ok(TokenTree::Literal(l)) => {
            let string = l.to_string();
            if !string.starts_with('"') || !string.ends_with('"') {
                return Err(diagnostic!(l, Level::Error, "expected string literal"));
            }
            Ok(string)
        }
        Ok(tt) => Err(diagnostic!(tt, Level::Error, "expected string literal")),
    }
}

fn expect_literal<'a, 'b>(
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

pub fn eager_macro_rules(stream: TokenStream) -> TokenStream {
    let found_crate = match crate_name("eager2") {
        Err(e) => abort_call_site!(
            "eager2 is not present in `Cargo.toml`";
            error = "{}", e;
        ),
        Ok(FoundCrate::Itself) => Ident::new("eager2", Span::call_site()),
        Ok(FoundCrate::Name(name)) => Ident::new(&name, Span::call_site()),
    };
    let found_crate = quote! {::#found_crate};

    let mut stream = stream.into_iter().peekable();

    let hidden_ident = match stream.peek() {
        None => return TokenStream::new(),

        Some(TokenTree::Punct(p)) if p.as_char() == '$' => {
            let dollar = stream.next();
            match stream.next() {
                None => abort!(dollar, "expected ident after $"),
                Some(TokenTree::Ident(ident)) => ident,
                Some(_token) => abort!(_token, "expected ident after $"),
            }
        }
        Some(_token) => Ident::new(EAGER2_IDENT, Span::call_site()),
    };

    let mut outputs = vec![];

    while stream.peek().is_some() {
        let mut metas = vec![];
        loop {
            match stream.peek() {
                None => abort_call_site!("expected # or macro_rules"),
                Some(TokenTree::Punct(p)) if p.as_char() == '#' => {
                    let pound = stream.next().unwrap();
                    match stream.next() {
                        None => abort_call_site!("expected ["),
                        Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Bracket => {
                            metas.push(pound);
                            metas.push(TokenTree::Group(g));
                        }
                        Some(_token) => abort!(_token, "expected ["),
                    }
                }
                Some(TokenTree::Ident(i)) if i == "macro_rules" => break,
                Some(t) => abort!(t, "expected token `#` or `macro_rules`"),
            }
        }
        let _macro_rules = stream.next().unwrap();
        match stream.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == '!' => {}
            None => abort_call_site!("expected token `!`"),
            Some(t) => abort!(t, "expected token `!`"),
        }
        let macro_name = match stream.next() {
            Some(TokenTree::Ident(i)) => i,
            None => abort_call_site!("expected ident"),
            Some(t) => abort!(t, "expected ident"),
        };
        let group = match stream.next() {
            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => g,
            None => abort_call_site!("expected {{"),
            Some(t) => abort!(t, "expected {{"),
        };

        struct Rule {
            grammer: Group,
            expansion: Group,
        }

        let mut rules = vec![];
        let mut stream = group.stream().into_iter();
        loop {
            let grammer = match stream.next() {
                None => break,
                Some(TokenTree::Group(g)) => g,
                Some(t) => abort!(t, "expected {{ or [ or ("),
            };
            // Arrow
            match stream.next() {
                Some(TokenTree::Punct(p))
                    if p.as_char() == '=' && p.spacing() == Spacing::Joint => {}
                None => abort_call_site!("expected ="),
                Some(t) => abort!(t, "expected ="),
            }
            match stream.next() {
                Some(TokenTree::Punct(p)) if p.as_char() == '>' => {}
                None => abort_call_site!("expected >"),
                Some(t) => abort!(t, "expected >"),
            }
            let expansion = match stream.next() {
                None => break,
                Some(TokenTree::Group(g)) => g,
                Some(t) => abort!(t, "expected {{ or [ or ("),
            };
            match stream.next() {
                None => {
                    rules.push(Rule { grammer, expansion });
                    break;
                }
                Some(TokenTree::Punct(p)) if p.as_char() == ';' => {
                    rules.push(Rule { grammer, expansion });
                }
                Some(t) => abort!(t, "expected ;"),
            }
        }

        let eager_call_sigil = Literal::from_str(EAGER_CALL_SIGIL).unwrap();

        let eager_rules = rules.iter().map(|Rule { grammer, expansion }| {
            let grammer = grammer.stream();
            let expansion = expansion.stream();
            quote! {
                (
                    @eager[$($#hidden_ident:tt)*]
                    #grammer
                ) => {
                    #found_crate::eager_internal!{
                        #eager_call_sigil[$($#hidden_ident)*]
                        #expansion
                    }
                };
            }
        });
        let pure_rules = rules.iter().map(|Rule { grammer, expansion }| {
            let grammer = grammer.stream();
            let expansion = expansion.stream();
            quote! {
                (
                    #grammer
                ) => {
                    #expansion
                };
            }
        });

        let output = quote! {
            #(#metas)*
            macro_rules! #macro_name{
                #(#eager_rules)*
                // Put the pure version after so eager is always tried first
                #(#pure_rules)*
            }
        };

        outputs.push(output);
    }

    let output = quote! { #(#outputs)* };

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning! {"eager_macro_rules output: {}", output}

    output
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Mode {
    Eager,
    Lazy,
}

impl TryFrom<&'_ str> for Mode {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Ok(match s {
            "eager" => Mode::Eager,
            "lazy" => Mode::Lazy,
            _ => return Err(()),
        })
    }
}

impl Mode {
    fn from(span: Span, t: Option<TokenTree>) -> Self {
        let i = match t {
            Some(TokenTree::Ident(i)) => i,
            None => abort!(span, "{}", SIGIL_ERROR),
            Some(t) => abort!(t, "{}", SIGIL_ERROR),
        };
        match i.to_string().as_str() {
            LAZY_SIGIL => Self::Lazy,
            EAGER_SIGIL => Self::Eager,
            _ => abort!(i, "{}", SIGIL_ERROR),
        }
    }
}

impl ToTokens for Mode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let sigil = match self {
            Self::Eager => EAGER_SIGIL,
            Self::Lazy => LAZY_SIGIL,
        };

        tokens.append(Ident::new(sigil, Span::call_site()));
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ExecutableMacroType {
    Env,
    Concat,
    Stringify,
    Include,
    IncludeStr,

    CCase,
    Eq,
    Iif,
    Unstringify,
}

impl TryFrom<&str> for ExecutableMacroType {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "env" => Self::Env,
            "concat" => Self::Concat,
            "stringify" => Self::Stringify,
            "include" => Self::Include,
            "include_str" => Self::IncludeStr,

            "ccase" => Self::CCase,
            "eq" => Self::Eq,
            "iif" => Self::Iif,
            "unstringify" => Self::Unstringify,
            _ => return Err(()),
        })
    }
}

fn execute_env(
    span: Span,
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
    processed_out: &mut Vec<TokenTree>,
) {
    match (locked.is_empty(), free.len() + processed.len()) {
        (false, 0..=3) | (true, 1..=4) => {}
        _ => abort!(span, "`env!()` takes 1 or 2 arguments"),
    }

    let mut args = free.into_iter().chain(locked).chain(processed);
    let key = expect_string_literal(Ok(args.next().unwrap())).unwrap_or_abort();
    args.next()
        .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());
    let error = args
        .next()
        .map(|tt| expect_string_literal(Ok(tt)).unwrap_or_abort());
    if args.next().is_some() {
        abort!(span, "`env!()` takes 1 or 2 arguments")
    }

    let value = match (env::var(&key[1..key.len() - 1]), error) {
        (Ok(value), _) => value,
        (Err(_), Some(error)) => abort!(span, "{}", error),
        (Err(env::VarError::NotPresent), None) => abort!(
            span,
            "environment variable `{}` not defined at compile time",
            key
        ),
        (Err(env::VarError::NotUnicode(_)), None) => abort!(
            span,
            "environment variable `{}` was present but not unicode at compile time",
            key
        ),
    };
    processed_out.push(TokenTree::Literal(Literal::string(&value)));
}
fn execute_stringify(
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
    processed_out: &mut Vec<TokenTree>,
) {
    let s = TokenStream::from_iter(free.into_iter().chain(locked).chain(processed)).to_string();
    processed_out.push(TokenTree::Literal(Literal::string(&s)));
}
fn execute_concat(
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
    processed_out: &mut Vec<TokenTree>,
) {
    let mut buffer = String::new();
    let mut args = free.into_iter().chain(locked).chain(processed);
    while let Some(t) = args.next() {
        let s = expect_string_literal(Ok(t)).unwrap_or_abort();
        buffer += &s[1..s.len() - 1];
        args.next()
            .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());
    }
    processed_out.push(TokenTree::Literal(Literal::string(&buffer)));
}

fn include_helper(
    span: Span,
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
) -> String {
    let mut args = free
        .iter()
        .cloned()
        .chain(locked)
        .chain(processed.iter().cloned());
    let string = args.next();
    let string_span = string.as_ref().map(|v| v.span());
    let string = expect_string_literal(string.ok_or(span)).unwrap_or_abort();
    let string_span = string_span.unwrap();
    args.next()
        .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());

    let mut path = Path::new(&string[1..string.len() - 1]);
    if path.is_relative() {
        abort!(
            string_span,
            "relative path is not supported here; use `include!(concat!(env!(\"CARGO_MANIFEST_DIR\"), ...))"
        )
    }

    // Make Windows verbatim paths work even with mixed path separators, which
    // can happen when a path is produced using `concat!`.
    let path_buf: PathBuf;
    if let Some(path::Component::Prefix(prefix)) = path.components().next() {
        if prefix.kind().is_verbatim() {
            path_buf = path.components().collect();
            path = &path_buf;
        }
    }

    match fs::read_to_string(path) {
        Ok(content) => content,
        Err(err) => abort!(span, "Couldn't read {}: {:?}", path.display(), err),
    }
}

fn execute_include(
    span: Span,
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
    unprocessed: &mut Box<dyn TokenIterator>,
) {
    let content = include_helper(span, free, locked, processed);
    let parsed = TokenStream::from_str(&content).unwrap();

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning! {"include result:{}", parsed}

    let old_unproccesed = mem::replace(unprocessed, Box::new([].into_iter()));
    *unprocessed = Box::new(parsed.into_iter().chain(old_unproccesed))
}

fn execute_include_str(
    span: Span,
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
    processed_out: &mut Vec<TokenTree>,
) {
    let content = include_helper(span, free, locked, processed);
    let string = Literal::string(&content[1..content.len() - 1]);

    processed_out.push(string.into());
}

fn execute_ccase(
    span: Span,
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
    processed_out: &mut Vec<TokenTree>,
) {
    use convert_case::{Boundary, Case, Casing, Converter, pattern::Pattern};

    let mut args = free.into_iter().chain(locked).chain(processed);

    let input = match args.next() {
        None => abort!(
            span, "unexpected end of macro invocation";
            note = "while trying to match ident or string literal `$input`"),
        Some(TokenTree::Ident(i)) => Ok(i),
        Some(TokenTree::Literal(l)) => {
            let string = l.to_string();
            if !string.starts_with('"') || !string.ends_with('"') {
                abort!(l, "expected ident or string literal")
            }
            Err(string)
        }
        Some(tt) => abort!(tt, "expected ident or string literal"),
    };
    expect_punct(args.next().ok_or(span), ',').unwrap_or_abort();

    let [mut from, mut boundaries, mut to, mut pattern, mut delimeter] =
        [const { Option::<(Ident, Span, String)>::None }; 5];
    while let Some(tt) = args.next() {
        let arg_name = expect_ident(Ok(tt), Param::Named("arg_name")).unwrap_or_abort();
        let dest = if arg_name == "f" || arg_name == "from" {
            &mut from
        } else if arg_name == "b" || arg_name == "boundaries" {
            &mut boundaries
        } else if arg_name == "t" || arg_name == "to" {
            &mut to
        } else if arg_name == "p" || arg_name == "pattern" {
            &mut pattern
        } else if arg_name == "d" || arg_name == "delimeter" {
            &mut delimeter
        } else {
            abort!(
                arg_name,
                "expected ident one of [`f`, `from`, `b`, `boundaries`, `t`, `to`, `p`, `pattern`, `d`, `delimeter`]"
            );
        };
        if let Some(dest) = dest.as_ref() {
            abort!(arg_name, "duplicate arg_name";
                note = dest.0.span() => "previous found here");
        }

        expect_punct(args.next().ok_or(span), ':').unwrap_or_abort();
        let arg_val = args.next();
        let arg_span = arg_val.as_ref().map(|v| v.span());
        let arg_val = expect_string_literal(arg_val.ok_or(span)).unwrap_or_abort();
        let arg_span = arg_span.unwrap();
        args.next()
            .map(|tt| expect_punct(Ok(tt), ',').unwrap_or_abort());

        *dest = Some((
            arg_name,
            arg_span,
            arg_val[1..arg_val.len() - 1].to_string(),
        ));
    }

    const ALL_CASES: &[(&str, Case)] = &{
        [
            ("snake", Case::Snake),
            ("constant", Case::Constant),
            ("uppersnake", Case::UpperSnake),
            ("ada", Case::Ada),
            ("kebab", Case::Kebab),
            ("cobol", Case::Cobol),
            ("upperkebab", Case::UpperKebab),
            ("train", Case::Train),
            ("flat", Case::Flat),
            ("upperflat", Case::UpperFlat),
            ("pascal", Case::Pascal),
            ("uppercamel", Case::UpperCamel),
            ("camel", Case::Camel),
            ("lower", Case::Lower),
            ("upper", Case::Upper),
            ("title", Case::Title),
            ("sentence", Case::Sentence),
            ("alternating", Case::Alternating),
            ("toggle", Case::Toggle),
            ("screaming", Case::Constant),
            ("alternate", Case::Alternating),
        ]
    };

    fn case_value_parser(span: Span, s: &str) -> Case {
        let case_str = s.to_case(Case::Flat);
        for (name, case) in ALL_CASES {
            if case_str == *name {
                return *case;
            }
        }
        abort!(
            span,
            "'{}' is not a valid case.  See documentation for a list of cases.",
            s
        );
    }
    const ALL_PATTERNS: &[(&str, Pattern)] = &{
        use convert_case::pattern;
        [
            ("uppercase", pattern::uppercase),
            ("lowercase", pattern::lowercase),
            ("capital", pattern::capital),
            ("camel", pattern::camel),
            ("toggle", pattern::toggle),
            ("alternating", pattern::alternating),
            ("sentence", pattern::sentence),
        ]
    };

    fn pattern_value_parser(span: Span, s: &str) -> Pattern {
        let pattern_str = s.to_case(Case::Flat);
        for pattern in ALL_PATTERNS {
            let pattern_in_flat = pattern.0.to_case(Case::Flat);
            if pattern_str == pattern_in_flat {
                return pattern.1;
            }
        }
        abort!(
            span,
            "'{}' is not a valid pattern.  See documentation for list of patterns.",
            s
        );
    }

    match (
        from.as_ref(),
        boundaries.as_ref(),
        to.as_ref(),
        pattern.as_ref(),
        delimeter.as_ref(),
    ) {
        (Some((a, _, _)), Some((b, _, _)), _, _, _)
        | (_, _, Some((a, _, _)), Some((b, _, _)), _)
        | (_, _, Some((a, _, _)), _, Some((b, _, _))) => {
            abort!(a.span(), "arg conflicts with other arg";
                note = b.span() => "other arg" )
        }
        (_, _, _, None, Some((d, _, _))) => {
            abort!(d.span(), "`delimeter` requires missing argument `pattern`";
                note = span => "missing arg `pattern`")
        }
        (_, _, None, None, _) => abort!(span, "missing argument `to` or `pattern`"),
        _ => {}
    }

    // TODO: check args
    let mut conv = Converter::new();
    if let Some((_, _, boundaries)) = boundaries {
        debug_assert!(from.is_none());
        let boundaries = Boundary::defaults_from(boundaries.as_str());
        conv = conv.set_boundaries(&boundaries);
    }
    if let Some((_, span, from)) = from {
        conv = conv.from_case(case_value_parser(span, &from));
    }
    if let Some((_, _, delimeter)) = delimeter {
        debug_assert!(pattern.is_some());
        // --delimeter
        conv = conv.set_delim(delimeter);
    }
    if let Some((_, span, pattern)) = pattern {
        debug_assert!(to.is_none());
        conv = conv.set_pattern(pattern_value_parser(span, &pattern));
    }
    if let Some((_, span, to)) = to {
        conv = conv.to_case(case_value_parser(span, &to));
    }
    let result = match input {
        Ok(ident) => {
            let result = conv.convert(ident.to_string());
            // Parse to error check
            let tokens: Vec<_> = TokenStream::from_str(&result)
                .unwrap_or_default()
                .into_iter()
                .collect();
            match tokens.as_slice() {
                [ident @ TokenTree::Ident(_)] => ident.clone(),
                _ => abort!(span, "`{}` is not a valid identifier", result),
            }
        }
        Err(string) => {
            let result = conv.convert(&string[1..string.len() - 1]);
            Literal::string(&result).into()
        }
    };
    processed_out.push(result);
}

fn execute_eq(
    span: Span,
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
    processed_out: &mut Vec<TokenTree>,
) {
    let mut args = free
        .iter()
        .cloned()
        .chain(locked)
        .chain(processed.iter().cloned());
    fn tt_eq((a, b): (TokenTree, TokenTree)) -> bool {
        match (a, b) {
            (TokenTree::Group(a), TokenTree::Group(b)) => group_eq(&a, &b),
            (TokenTree::Ident(a), TokenTree::Ident(b)) => a == b,
            (TokenTree::Punct(a), TokenTree::Punct(b)) => a.as_char() == b.as_char(),
            (TokenTree::Literal(a), TokenTree::Literal(b)) => a.to_string() == b.to_string(),
            _ => false,
        }
    }
    fn group_eq(a: &Group, b: &Group) -> bool {
        a.delimiter() == b.delimiter() && a.stream().into_iter().zip(b.stream()).all(tt_eq)
    }

    let mut prev = None;
    for i in 0.. {
        let name = format!("arg_{}", i);
        let next = expect_group(args.next().ok_or(span), Param::Named(&name)).unwrap_or_abort();
        if let Some(prev) = prev.take() {
            if !group_eq(&prev, &next) {
                processed_out.push(Ident::new("false", span).into());
                return;
            }
        }
        prev = Some(next);

        if let Some(comma) = args.next() {
            expect_punct(Ok(comma), ',').unwrap_or_abort();
        } else {
            break;
        }
    }
    processed_out.push(Ident::new("true", span).into());
}

fn execute_iif(
    span: Span,
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
    processed_out: &mut Vec<TokenTree>,
) {
    let mut args = free
        .iter()
        .cloned()
        .chain(locked)
        .chain(processed.iter().cloned());
    let check = expect_ident(args.next().ok_or(span), Param::Named("check")).unwrap_or_abort();
    expect_punct(args.next().ok_or(span), ',').unwrap_or_abort();
    let true_case =
        expect_group(args.next().ok_or(span), Param::Named("true_case")).unwrap_or_abort();
    expect_punct(args.next().ok_or(span), ',').unwrap_or_abort();
    let false_case =
        expect_group(args.next().ok_or(span), Param::Named("false_case")).unwrap_or_abort();
    args.next()
        .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());

    if check == "true" {
        processed_out.extend(true_case.stream());
    } else if check == "false" {
        processed_out.extend(false_case.stream());
    } else {
        abort!(check, "expected either token `true` or token `false");
    }
}

fn execute_unstringify(
    span: Span,
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
    unprocessed: &mut Box<dyn TokenIterator>,
) {
    let mut args = free
        .iter()
        .cloned()
        .chain(locked)
        .chain(processed.iter().cloned());
    let string = expect_string_literal(args.next().ok_or(span)).unwrap_or_abort();
    args.next()
        .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());

    let unstrung = TokenStream::from_str(&string[1..string.len() - 1]).unwrap();

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning! {"unstringify result:{}", unstrung}

    let old_unproccesed = mem::replace(unprocessed, Box::new([].into_iter()));
    *unprocessed = Box::new(unstrung.into_iter().chain(old_unproccesed))
}

impl ExecutableMacroType {
    fn execute(
        self,
        span: Span,
        free: Vec<TokenTree>,
        locked: TokenStream,
        processed: Vec<TokenTree>,
        state: &mut EagerState,
    ) {
        #[cfg(feature = "debug")]
        proc_macro_error2::emit_call_site_warning! {"executing {:?}: {}", self, TokenStream::from_iter(processed.iter().cloned())}

        match self {
            Self::Concat => {
                execute_concat(free, locked, processed, &mut state.processed);
            }
            Self::Env => {
                execute_env(span, free, locked, processed, &mut state.processed);
            }
            Self::Stringify => {
                execute_stringify(free, locked, processed, &mut state.processed);
            }
            Self::Include => {
                execute_include(span, free, locked, processed, &mut state.unprocessed);
            }
            Self::IncludeStr => {
                execute_include_str(span, free, locked, processed, &mut state.processed);
            }

            Self::CCase => {
                execute_ccase(span, free, locked, processed, &mut state.processed);
            }
            Self::Eq => {
                execute_eq(span, free, locked, processed, &mut state.processed);
            }
            Self::Iif => {
                execute_iif(span, free, locked, processed, &mut state.processed);
            }
            Self::Unstringify => {
                execute_unstringify(span, free, locked, processed, &mut state.unprocessed);
            }
        }
    }
}

enum MacroType {
    ModeSwitch(Mode),
    Exec(ExecutableMacroType),
    Unknown,
}

impl MacroType {
    fn try_new(tokens: &[TokenTree], mode_only: bool) -> Option<Self> {
        fn get_token_string(tt: &TokenTree) -> String {
            let TokenTree::Ident(i) = tt else {
                unreachable!()
            };
            i.to_string()
        }

        let token_string = match tokens.len() {
            // Zero isn't enough
            // One token is just a `!` which isn't enough
            0 | 1 => return None,

            // e.g. [`eager`, `!`]
            2 => get_token_string(&tokens[0]),

            // e.g. [`eager2`, `:`, `:`, `eager`, `!`]
            5 => match get_token_string(&tokens[0]).as_str() {
                "eager2" | "crate" => get_token_string(&tokens[3]),
                _ => return None,
            },
            // e.g. [`$`, `crate`, `:`, `:`, `eager`, `!`]
            6 => match get_token_string(&tokens[1]).as_str() {
                "crate" => get_token_string(&tokens[4]),
                _ => return None,
            },
            // e.g. [`:`, `:`, `eager2`, `:`, `:`, `eager`, `!`]
            7 => match get_token_string(&tokens[2]).as_str() {
                "eager2" => get_token_string(&tokens[5]),
                _ => return None,
            },
            _ if mode_only => return None,
            _ => return Some(Self::Unknown),
        };
        let token_string = token_string.as_str();

        // Try mode first
        if let Ok(mode) = token_string.try_into() {
            return Some(Self::ModeSwitch(mode));
        }
        // Stop if we're limited to mode only
        if mode_only {
            return None;
        }

        if let Ok(exec) = token_string.try_into() {
            return Some(Self::Exec(exec));
        }

        Some(Self::Unknown)
    }
}

struct ModeSwitchTrailingMacro<'a> {
    tokens: &'a mut Vec<TokenTree>,
    offset: usize,
    mode: Mode,
}
struct ExecutableTrailingMacro<'a> {
    tokens: &'a mut Vec<TokenTree>,
    offset: usize,
    exec: ExecutableMacroType,
}
struct UnknownTrailingMacro<'a> {
    tokens: &'a mut Vec<TokenTree>,
    offset: usize,
}
enum TrailingMacro<'a> {
    ModeSwitch(ModeSwitchTrailingMacro<'a>),
    Exec(ExecutableTrailingMacro<'a>),
    Unknown(UnknownTrailingMacro<'a>),
}

impl<'a> TrailingMacro<'a> {
    fn try_new(tokens: &'a mut Vec<TokenTree>, mode_only: bool) -> Option<Self> {
        let i = {
            let mut iter = tokens.iter().enumerate().rev();
            match iter.next() {
                // Exclamation found, continue checking
                Some((_, TokenTree::Punct(p))) if p.as_char() == '!' => {}
                // No exclamation
                None | Some(_) => return None,
            }

            loop {
                match iter.next() {
                    // All tokens in the vec are part of macro path
                    // len = 1 + 3*n; (`:` `:` `ident`)+ `!`
                    None => break 0,
                    // Continue checking on ident
                    Some((_, TokenTree::Ident(_))) => {}
                    // len = 1 + 3*n; (`:` `:` `ident`)+
                    Some((i, _)) => break i + 1,
                }
                match iter.next() {
                    // All tokens in the vec are part of macro path
                    // len = 2 + 3*n; (`ident` `:` `:`)+ `ident` `!`
                    None => break 0,
                    // Continue checking on colon
                    Some((_, TokenTree::Punct(p))) if p.as_char() == ':' => {}
                    // Break on dollar sign
                    // len = 3 + 3*n; `$` (`ident` `:` `:`)+ `ident` `!`
                    Some((i, TokenTree::Punct(p))) if p.as_char() == '$' => break i,
                    // Something other than colon, so end and exclude
                    // len = 2 + 3*n; (`ident` `:` `:`)+ `ident` `!`
                    Some((i, _)) => break i + 1,
                }
                match iter.next() {
                    // All tokens in the vec except the head colon are part of macro path
                    // len = 2 + 3*n; (`ident` `:` `:`)+ `ident` `!`
                    None => break 1,
                    // Continue checking on second colon
                    Some((_, TokenTree::Punct(p)))
                        if p.as_char() == ':' && p.spacing() == Spacing::Joint => {}
                    // Something other than second colon, so end and exclude head colon
                    // len = 2 + 3*n; (`ident` `:` `:`)+ `ident` `!`
                    Some((i, _)) => break i + 2,
                }
            }
        };

        Some(match MacroType::try_new(&tokens[i..], mode_only)? {
            MacroType::ModeSwitch(mode) => TrailingMacro::ModeSwitch(ModeSwitchTrailingMacro {
                tokens,
                offset: i,
                mode,
            }),
            MacroType::Exec(exec) => TrailingMacro::Exec(ExecutableTrailingMacro {
                tokens,
                offset: i,
                exec,
            }),
            MacroType::Unknown => {
                TrailingMacro::Unknown(UnknownTrailingMacro { tokens, offset: i })
            }
        })
    }
    fn mode(&self) -> Option<Mode> {
        match self {
            TrailingMacro::ModeSwitch(m) => Some(m.mode),
            _ => None,
        }
    }
}
impl ModeSwitchTrailingMacro<'_> {
    fn truncate(self) -> Mode {
        self.tokens.truncate(self.offset);
        self.mode
    }
}
impl ExecutableTrailingMacro<'_> {
    fn truncate(self) -> ExecutableMacroType {
        self.tokens.truncate(self.offset);
        self.exec
    }
}
impl UnknownTrailingMacro<'_> {
    fn split_off(self) -> Vec<TokenTree> {
        self.tokens.split_off(self.offset)
    }
}

struct MacroPath {
    pub segments: Vec<TokenTree>,
}
impl ToTokens for MacroPath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(self.segments.iter())
    }
}

pub fn eager(stream: TokenStream) -> TokenStream {
    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning! {"eager input: {}", stream}

    let state = EagerState::new(Span::call_site(), Delimiter::Bracket, Mode::Eager, stream);
    let output = match state.process() {
        Ok((free, locked, processed)) => quote! { #(#free)* #locked #(#processed)* },
        Err((state, eager_macro, stream)) => {
            quote! {
                #eager_macro{@eager #state #stream}
            }
        }
    };

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning! {"eager output: {}", output}

    output
}

pub fn lazy(stream: TokenStream) -> TokenStream {
    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning! {"lazy input: {}", stream}

    let state = EagerState::new(Span::call_site(), Delimiter::Bracket, Mode::Lazy, stream);
    let output = match state.process() {
        Ok((free, locked, processed)) => quote! { #(#free)* #locked #(#processed)* },
        Err((state, eager_macro, stream)) => {
            quote! {
                #eager_macro{@eager #state #stream}
            }
        }
    };

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning! {"lazy output: {}", output}

    output
}

pub fn eager_internal(stream: TokenStream) -> TokenStream {
    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning! {"eager_internal input: {}", stream}

    let mut stream = stream.into_iter();

    expect_literal(stream.next().ok_or_else(Span::call_site), EAGER_CALL_SIGIL).unwrap_or_abort();

    let group = expect_group(
        stream.next().ok_or_else(Span::call_site),
        Delimiter::Bracket,
    )
    .unwrap_or_abort();

    let mut state = EagerState::from(group);
    state.inject_expanded(stream);
    let result = state.process();

    let output = match result {
        Ok((free, locked, processed)) => quote! { #(#free)* #locked #(#processed)* },
        Err((state, eager_macro, stream)) => {
            quote! {
                #eager_macro{@eager #state #stream}
            }
        }
    };

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning! {"eager_internal output: {}", output}

    output
}

struct EagerState {
    span: Span,
    delim: Delimiter,
    mode: Mode,
    free: Vec<TokenTree>,
    locked: TokenStream,
    processed: Vec<TokenTree>,
    stack: Option<Box<EagerState>>,
    unprocessed: Box<dyn TokenIterator>,
}

trait TokenIterator: DynClone + Iterator<Item = TokenTree> {}

impl<T: DynClone + Iterator<Item = TokenTree>> TokenIterator for T {}

dyn_clone::clone_trait_object!(TokenIterator);

impl ToTokens for EagerState {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut output = TokenStream::new();
        self.mode.to_tokens(&mut output);
        output.append(Group::new(
            Delimiter::Bracket,
            self.free.iter().cloned().collect(),
        ));
        output.append(Group::new(Delimiter::Bracket, self.locked.clone()));
        output.append(Group::new(
            Delimiter::Bracket,
            self.processed.iter().cloned().collect(),
        ));
        if let Some(stack) = self.stack.as_ref() {
            stack.to_tokens(&mut output);
        } else {
            output.append(Group::new(Delimiter::Bracket, TokenStream::new()));
        }
        output.append_all(dyn_clone::clone_box(&*self.unprocessed));

        tokens.append(Group::new(self.delim, output));
    }
}

impl From<Group> for EagerState {
    fn from(g: Group) -> Self {
        let span = g.span();
        let delim = g.delimiter();

        let mut stream = g.stream().into_iter();
        let mode = Mode::from(g.span(), stream.next());
        let free = expect_group(stream.next().ok_or(span), Delimiter::Bracket).unwrap_or_abort();
        let locked = expect_group(stream.next().ok_or(span), Delimiter::Bracket).unwrap_or_abort();
        let processed =
            expect_group(stream.next().ok_or(span), Delimiter::Bracket).unwrap_or_abort();
        let stack =
            expect_group(stream.next().ok_or(span), Param::Named("stack")).unwrap_or_abort();
        let stack = if stack.stream().is_empty() {
            None
        } else {
            Some(Box::new(Self::from(stack)))
        };

        Self {
            span,
            delim,
            mode,
            free: free.stream().into_iter().collect(),
            locked: locked.stream(),
            processed: processed.stream().into_iter().collect(),
            stack,
            unprocessed: Box::new(stream),
        }
    }
}

impl EagerState {
    fn new(span: Span, delim: Delimiter, mode: Mode, stream: TokenStream) -> Self {
        Self {
            span,
            delim,
            mode,
            free: vec![],
            locked: TokenStream::new(),
            processed: vec![],
            stack: None,
            unprocessed: Box::new(stream.into_iter()),
        }
    }
    fn inject_expanded(&mut self, expanded: token_stream::IntoIter) {
        if let Some(stack) = self.stack.as_mut() {
            stack.inject_expanded(expanded);
        } else {
            let unproccesed = mem::replace(&mut self.unprocessed, Box::new([].into_iter()));
            self.unprocessed = Box::new(expanded.chain(unproccesed))
        }
    }
    fn process(
        mut self,
    ) -> Result<(Vec<TokenTree>, TokenStream, Vec<TokenTree>), (Self, MacroPath, TokenStream)> {
        if let Some(stack) = self.stack.take() {
            let delim = stack.delim;
            let span = stack.span;
            let (free, locked, processed) = match stack.process() {
                Ok(v) => v,
                Err((state, path, stream)) => {
                    self.stack = Some(Box::new(state));
                    return Err((self, path, stream));
                }
            };

            use TrailingMacro::{Exec, ModeSwitch, Unknown};
            let source = if self.locked.is_empty() {
                &mut self.processed
            } else {
                &mut self.free
            };
            match TrailingMacro::try_new(source, self.mode == Mode::Lazy) {
                // Handle mode switches
                Some(ModeSwitch(tm)) => {
                    match (
                        self.mode,
                        tm.truncate(),
                        self.locked.is_empty(),
                        locked.is_empty(),
                    ) {
                        (_, Mode::Eager, _, true) => {
                            self.processed.extend_from_slice(&free);
                            self.processed.extend_from_slice(&processed);
                        }

                        (_, Mode::Eager, true, false) => {
                            debug_assert!(self.free.is_empty());
                            self.free = mem::take(&mut self.processed);
                            self.free.extend_from_slice(&free);
                            self.locked = locked;
                            self.processed.extend_from_slice(&processed);
                        }
                        (_, Mode::Eager, false, false) => {
                            self.locked.append_all(mem::take(&mut self.processed));
                            self.locked.append_all(free);
                            self.locked.append_all(locked);
                            self.processed.extend_from_slice(&processed);
                        }

                        (Mode::Eager, Mode::Lazy, true, _) => {
                            debug_assert!(self.free.is_empty());
                            self.free = mem::take(&mut self.processed);
                            self.free.extend_from_slice(&free);
                            self.locked = locked;
                            self.locked.append_all(processed);
                        }
                        (Mode::Eager, Mode::Lazy, false, _) => {
                            self.locked.append_all(mem::take(&mut self.processed));
                            self.locked.append_all(free);
                            self.locked.append_all(locked);
                            self.locked.append_all(processed);
                        }

                        (Mode::Lazy, Mode::Lazy, _, _) => {
                            self.locked.append_all(mem::take(&mut self.processed));
                            self.locked.append_all(free);
                            self.locked.append_all(locked);
                            self.locked.append_all(processed);
                        }
                    }
                }

                // Execute known macros
                Some(Exec(tm)) => {
                    // mode_only should have been false to get here
                    debug_assert_eq!(self.mode, Mode::Eager);

                    let macro_type = tm.truncate();
                    macro_type.execute(span, free, locked, processed, &mut self)
                }

                // Return control to unknow macros
                Some(Unknown(tm)) => {
                    // mode_only should have been false to get here
                    debug_assert_eq!(self.mode, Mode::Eager);

                    let segments = tm.split_off();
                    let path = MacroPath { segments };
                    let stream = free.into_iter().chain(locked).chain(processed).collect();
                    return Err((self, path, stream));
                }
                None => {
                    let stream = free.into_iter().chain(locked).chain(processed).collect();
                    self.processed
                        .push(TokenTree::Group(Group::new(delim, stream)));
                }
            }
        }

        while let Some(token) = self.unprocessed.next() {
            match token {
                TokenTree::Group(g) => {
                    use TrailingMacro::{Exec, ModeSwitch, Unknown};
                    // lookback to see if this group is a macro param
                    // (looking especially for mode-switching macros)
                    let tm = TrailingMacro::try_new(&mut self.processed, false);

                    let inner_mode = tm.as_ref().and_then(|tm| tm.mode()).unwrap_or(self.mode);

                    #[cfg(feature = "debug")]
                    proc_macro_error2::emit_call_site_warning! {"processing with mode {:?}: {}", inner_mode, g}
                    let delim = g.delimiter();
                    let span = g.span();
                    let stack = EagerState::new(span, delim, inner_mode, g.stream());

                    let (free, locked, processed) = match stack.process() {
                        Ok(v) => v,
                        Err((state, path, stream)) => {
                            self.stack = Some(Box::new(state));
                            return Err((self, path, stream));
                        }
                    };
                    match (tm, self.mode) {
                        // Handle mode switches
                        (Some(ModeSwitch(tm)), _) => {
                            match (
                                self.mode,
                                tm.truncate(),
                                self.locked.is_empty(),
                                locked.is_empty(),
                            ) {
                                (_, Mode::Eager, _, true) => {
                                    self.processed.extend_from_slice(&processed);
                                }
                                (_, Mode::Eager, true, false) => {
                                    debug_assert!(self.free.is_empty());
                                    self.free = mem::take(&mut self.processed);
                                    self.free.extend_from_slice(&free);
                                    self.locked = locked;
                                    self.processed.extend_from_slice(&processed);
                                }
                                (_, Mode::Eager, false, false) => {
                                    self.locked.append_all(mem::take(&mut self.processed));
                                    self.locked.append_all(locked);
                                    self.processed.extend_from_slice(&processed);
                                }

                                (Mode::Eager, Mode::Lazy, true, _) => {
                                    debug_assert!(self.free.is_empty());
                                    self.free = mem::take(&mut self.processed);
                                    self.free.extend_from_slice(&free);
                                    self.locked = locked;
                                    self.locked.append_all(processed);
                                }
                                (Mode::Eager, Mode::Lazy, false, _)
                                | (Mode::Lazy, Mode::Lazy, _, _) => {
                                    self.locked.append_all(mem::take(&mut self.processed));
                                    self.locked.append_all(free);
                                    self.locked.append_all(locked);
                                    self.locked.append_all(processed);
                                }
                            }
                        }
                        // Execute known macros
                        (Some(Exec(tm)), Mode::Eager) => {
                            let macro_type = tm.truncate();
                            macro_type.execute(span, free, locked, processed, &mut self)
                        }
                        // Hand over execution to unknown macros if in eager mode
                        (Some(Unknown(tm)), Mode::Eager) => {
                            let segments = tm.split_off();
                            let path = MacroPath { segments };
                            let stream = processed.into_iter().collect();
                            return Err((self, path, stream));
                        }

                        (_, Mode::Lazy) | (None, Mode::Eager) => {
                            let stream = free.into_iter().chain(locked).chain(processed).collect();
                            self.processed
                                .push(TokenTree::Group(Group::new(delim, stream)));
                        }
                    }
                }
                t => self.processed.push(t),
            }
        }

        debug_assert!(self.stack.is_none());
        Ok((self.free, self.locked, self.processed))
    }
}
