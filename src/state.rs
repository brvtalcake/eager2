use std::path::{self, Path, PathBuf};
use std::str::FromStr;
use std::{env, fs, mem};

use proc_macro_error2::abort;
use proc_macro_error2::{Diagnostic, ResultExt};
use proc_macro2::{
    Delimiter, Group, Ident, Literal, Spacing, Span, TokenStream, TokenTree, token_stream,
};
use quote::{ToTokens, TokenStreamExt};

use crate::utils::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Mode {
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
    pub fn eager(b: bool) -> Self {
        if b { Self::Eager } else { Self::Lazy }
    }
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

enum Stack {
    Empty,
    Raw(Group),
    Processed(Box<State>, Option<TrailingMacro>),
}

impl Stack {
    fn take(&mut self) -> Self {
        mem::replace(self, Self::Empty)
    }
    fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}
impl ToTokens for Stack {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use Stack::*;
        match self {
            Empty => {
                tokens.append(Group::new(Delimiter::Bracket, TokenStream::new()));
            }
            Raw(g) => g.to_tokens(tokens),
            Processed(s, _) => s.to_tokens(tokens),
        }
    }
}

enum EfficientGroup<P> {
    Raw(Group),
    Processed(P),
}

impl<P: Default> Default for EfficientGroup<P> {
    fn default() -> Self {
        Self::Processed(P::default())
    }
}

impl<P> From<Group> for EfficientGroup<P> {
    fn from(g: Group) -> Self {
        Self::Raw(g)
    }
}
enum EgIntoIter<P: IntoIterator<Item = TokenTree>> {
    Raw(token_stream::IntoIter),
    Processed(P::IntoIter),
}

impl<P: IntoIterator<Item = TokenTree>> Clone for EgIntoIter<P>
where
    P::IntoIter: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Processed(p) => Self::Processed(p.clone()),
            Self::Raw(g) => Self::Raw(g.clone()),
        }
    }
}

impl<P: IntoIterator<Item = TokenTree>> Iterator for EgIntoIter<P> {
    type Item = TokenTree;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Processed(p) => p.next(),
            Self::Raw(g) => g.next(),
        }
    }
}

impl<P: IntoIterator<Item = TokenTree>> IntoIterator for EfficientGroup<P> {
    type IntoIter = EgIntoIter<P>;
    type Item = TokenTree;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Processed(p) => EgIntoIter::Processed(p.into_iter()),
            Self::Raw(g) => EgIntoIter::Raw(g.stream().into_iter()),
        }
    }
}

type EfficientGroupT = EfficientGroup<TokenStream>;
type EfficientGroupV = EfficientGroup<Vec<TokenTree>>;

impl ToTokens for EfficientGroupT {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Raw(g) => g.to_tokens(tokens),
            Self::Processed(s) => Group::new(Delimiter::Bracket, s.clone()).to_tokens(tokens),
        }
    }
}
impl ToTokens for EfficientGroupV {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Raw(g) => g.to_tokens(tokens),
            Self::Processed(s) => {
                Group::new(Delimiter::Bracket, s.iter().cloned().collect()).to_tokens(tokens)
            }
        }
    }
}

impl EfficientGroupV {
    fn push(&mut self, tt: TokenTree) {
        self.as_mut_vec().push(tt)
    }

    fn as_mut_vec(&mut self) -> &mut Vec<TokenTree> {
        if let Self::Raw(r) = self {
            let v = r.stream().into_iter().collect();
            *self = Self::Processed(v);
        }

        let Self::Processed(p) = self else {
            unreachable!()
        };
        p
    }
    fn append_to_stream(&self, tokens: &mut TokenStream) {
        match self {
            Self::Processed(p) => tokens.append_all(p.iter().cloned()),
            Self::Raw(g) => tokens.extend(g.stream()),
        }
    }
    fn append(&mut self, other: Self) {
        let v = self.as_mut_vec();
        match other {
            Self::Processed(mut p) => v.append(&mut p),
            Self::Raw(g) => v.extend(g.stream()),
        }
    }
    fn take(&mut self) -> Self {
        mem::replace(self, Self::Processed(vec![]))
    }
}
impl EfficientGroupT {
    fn is_empty(&self) -> bool {
        match self {
            Self::Processed(p) => p.is_empty(),
            Self::Raw(g) => g.stream().is_empty(),
        }
    }
    fn append_to_stream(&self, tokens: &mut TokenStream) {
        match self {
            Self::Processed(p) => tokens.extend(p.clone()),
            Self::Raw(g) => tokens.extend(g.stream()),
        }
    }
    fn into_stream(self) -> TokenStream {
        match self {
            Self::Processed(p) => p,
            Self::Raw(g) => g.stream(),
        }
    }
    fn as_mut_stream(&mut self) -> &mut TokenStream {
        if let Self::Raw(r) = self {
            *self = Self::Processed(r.stream());
        }

        let Self::Processed(p) = self else {
            unreachable!()
        };
        p
    }
    fn append<P>(&mut self, other: EfficientGroup<P>)
    where
        P: IntoIterator<Item = TokenTree>,
    {
        let s = self.as_mut_stream();
        match other {
            EfficientGroup::Processed(p) => s.append_all(p),
            EfficientGroup::Raw(g) => s.append_all(g.stream()),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ExecutableMacroType {
    CompileError,
    Concat,
    Env,
    Include,
    IncludeStr,
    Stringify,

    CCase,
    EagerIf,
    TokenEq,
    Unstringify,
}

impl TryFrom<&str> for ExecutableMacroType {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "compile_error" => Self::CompileError,
            "concat" => Self::Concat,
            "env" => Self::Env,
            "include" => Self::Include,
            "include_str" => Self::IncludeStr,
            "stringify" => Self::Stringify,

            "ccase" => Self::CCase,
            "eager_if" => Self::EagerIf,
            "token_eq" => Self::TokenEq,
            "unstringify" => Self::Unstringify,

            _ => return Err(()),
        })
    }
}

fn execute_env(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    let mut args = stream.into_iter();
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
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    let s = TokenStream::from_iter(stream).to_string();
    processed_out.push(TokenTree::Literal(Literal::string(&s)));
}
fn execute_concat(
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    let mut buffer = String::new();
    let mut args = stream.into_iter();
    while let Some(tt) = args.next() {
        let i = expect_ident(Ok(tt.clone()), "true");
        let l = expect_literal(Ok(tt), Param::Named("arg"));

        match (i, l) {
            (_, Ok(l)) => {
                let s = l.to_string();
                if s.starts_with('"') && s.ends_with('"') {
                    buffer += &s[1..s.len() - 1];
                } else {
                    buffer += &s;
                }
            }
            (Ok(i), _) => buffer += &i.to_string(),
            (_, Err(e)) => e.abort(),
        }
        args.next()
            .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());
    }
    processed_out.push(TokenTree::Literal(Literal::string(&buffer)));
}

fn include_helper(span: Span, stream: impl IntoIterator<Item = TokenTree>) -> String {
    let mut args = stream.into_iter();
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
    stream: impl IntoIterator<Item = TokenTree>,
    unprocessed: &mut Vec<token_stream::IntoIter>,
) {
    let content = include_helper(span, stream);
    let parsed = TokenStream::from_str(&content).unwrap();

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!("include result:{}", parsed);

    unprocessed.push(parsed.into_iter());
}

fn execute_include_str(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    let content = include_helper(span, stream);
    let string = Literal::string(&content[1..content.len() - 1]);

    processed_out.push(string.into());
}

fn execute_compile_error(span: Span, stream: impl IntoIterator<Item = TokenTree>) {
    abort!(span, "{}", TokenStream::from_iter(stream));
}

fn execute_ccase(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    use convert_case::{Boundary, Case, Casing, Converter, pattern::Pattern};

    let mut args = stream.into_iter();

    let input = expect_ident_or_string(args.next().ok_or(span)).unwrap_or_abort();
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

fn execute_token_eq(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    let mut args = stream.into_iter();
    struct TtWrapper(TokenTree);
    impl PartialEq for TtWrapper {
        fn eq(&self, other: &Self) -> bool {
            match (eat_zero_group(self.0.clone()), eat_zero_group(other.0.clone())) {
                (TokenTree::Group(a), TokenTree::Group(b)) => group_eq(&a, &b),
                (TokenTree::Ident(a), TokenTree::Ident(b)) => a == b,
                (TokenTree::Punct(a), TokenTree::Punct(b)) => a.as_char() == b.as_char(),
                (TokenTree::Literal(a), TokenTree::Literal(b)) => a.to_string() == b.to_string(),
                _ => false,
            }
        }
    }
    fn stream_eq(a: TokenStream, b: TokenStream) -> bool {
        a.is_empty() == b.is_empty() && a.into_iter().map(TtWrapper).eq(b.into_iter().map(TtWrapper))
    }
    fn group_eq(a: &Group, b: &Group) -> bool {
        a.delimiter() == b.delimiter() && stream_eq(a.stream(), b.stream())
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

fn execute_eager_if(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    unprocessed: &mut Vec<token_stream::IntoIter>,
) {
    let mut args = stream.into_iter();

    let check = expect_ident(args.next().ok_or(span), Param::Named("check")).unwrap_or_abort();
    if args.next().is_some() {
        abort!(span, "`eager_if!()` takes 1 argument")
    }

    let check = if check == "true" {
        true
    } else if check == "false" {
        false
    } else {
        abort!(check, "expected either token `true` or token `false");
    };

    fn get_one(unprocessed: &mut Vec<token_stream::IntoIter>) -> Option<TokenTree> {
        while let Some(stream) = unprocessed.last_mut() {
            if let Some(tt) = stream.next() {
                return Some(tt);
            }
            unprocessed.pop();
        }
        None
    }

    let true_case =
        expect_group(get_one(unprocessed).ok_or(span), Param::Named("true_case")).unwrap_or_abort();

    let false_case = expect_group(get_one(unprocessed).ok_or(span), Param::Named("false_case"))
        .unwrap_or_abort();

    let output = if check { true_case } else { false_case }.stream();

    unprocessed.push(output.into_iter());
}

fn execute_unstringify(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    unprocessed: &mut Vec<token_stream::IntoIter>,
) {
    let mut args = stream.into_iter();
    let string = expect_string_literal(args.next().ok_or(span)).unwrap_or_abort();
    args.next()
        .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());

    let unstrung = TokenStream::from_str(&string[1..string.len() - 1]).unwrap();

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!("unstringify result:{}", unstrung);

    unprocessed.push(unstrung.into_iter())
}

impl ExecutableMacroType {
    fn execute(
        self,
        span: Span,
        stream: impl Clone + IntoIterator<Item = TokenTree>,
        state: &mut State,
    ) {
        #[cfg(feature = "debug")]
        proc_macro_error2::emit_call_site_warning!(
            "executing {:?}: {}",
            self,
            TokenStream::from_iter(stream.clone())
        );

        match self {
            Self::Concat => {
                execute_concat(stream, &mut state.processed);
            }
            Self::Env => {
                execute_env(span, stream, &mut state.processed);
            }
            Self::Stringify => {
                execute_stringify(stream, &mut state.processed);
            }
            Self::Include => {
                execute_include(span, stream, &mut state.unprocessed);
            }
            Self::IncludeStr => {
                execute_include_str(span, stream, &mut state.processed);
            }
            Self::CompileError => {
                execute_compile_error(span, stream);
            }

            Self::CCase => {
                execute_ccase(span, stream, &mut state.processed);
            }
            Self::TokenEq => {
                execute_token_eq(span, stream, &mut state.processed);
            }
            Self::EagerIf => {
                execute_eager_if(span, stream, &mut state.unprocessed);
            }
            Self::Unstringify => {
                execute_unstringify(span, stream, &mut state.unprocessed);
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
    fn try_new(found_crate: &str, tokens: &[TokenTree], mode_only: bool) -> Option<Self> {
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
                "crate" => get_token_string(&tokens[3]),
                c if c == found_crate => get_token_string(&tokens[3]),
                _ => return None,
            },
            // e.g. [`$`, `crate`, `:`, `:`, `eager`, `!`]
            6 => match get_token_string(&tokens[1]).as_str() {
                "crate" => get_token_string(&tokens[4]),
                _ => return None,
            },
            // e.g. [`:`, `:`, `eager2`, `:`, `:`, `eager`, `!`]
            7 => match get_token_string(&tokens[2]).as_str() {
                c if c == found_crate => get_token_string(&tokens[5]),
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

struct ModeSwitchTrailingMacro {
    offset: usize,
    mode: Mode,
}
struct ExecutableTrailingMacro {
    offset: usize,
    exec: ExecutableMacroType,
}
struct UnknownTrailingMacro {
    offset: usize,
}

enum TrailingMacro {
    ModeSwitch(ModeSwitchTrailingMacro),
    Exec(ExecutableTrailingMacro),
    Unknown(UnknownTrailingMacro),
}

impl TrailingMacro {
    fn try_new(found_crate: &str, tokens: &[TokenTree], mode_only: bool) -> Option<Self> {
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

        Some(
            match MacroType::try_new(found_crate, &tokens[i..], mode_only)? {
                MacroType::ModeSwitch(mode) => {
                    TrailingMacro::ModeSwitch(ModeSwitchTrailingMacro { offset: i, mode })
                }
                MacroType::Exec(exec) => {
                    TrailingMacro::Exec(ExecutableTrailingMacro { offset: i, exec })
                }
                MacroType::Unknown => TrailingMacro::Unknown(UnknownTrailingMacro { offset: i }),
            },
        )
    }
    fn mode(&self) -> Option<Mode> {
        match self {
            TrailingMacro::ModeSwitch(m) => Some(m.mode),
            _ => None,
        }
    }
}
impl ModeSwitchTrailingMacro {
    fn truncate(self, tokens: &mut Vec<TokenTree>) -> Mode {
        tokens.truncate(self.offset);
        self.mode
    }
}
impl ExecutableTrailingMacro {
    fn truncate(self, tokens: &mut Vec<TokenTree>) -> ExecutableMacroType {
        tokens.truncate(self.offset);
        self.exec
    }
}
impl UnknownTrailingMacro {
    fn split_off(self, tokens: &mut Vec<TokenTree>) -> Vec<TokenTree> {
        tokens.split_off(self.offset)
    }
}

pub struct MacroPath {
    pub segments: Vec<TokenTree>,
}
impl ToTokens for MacroPath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(self.segments.iter())
    }
}

pub struct State {
    span: Span,
    delim: Delimiter,
    mode: Mode,
    free: EfficientGroupT,
    locked: EfficientGroupT,
    processed: EfficientGroupV,
    stack: Stack,
    unprocessed: Vec<token_stream::IntoIter>,
}

impl ToTokens for State {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut state = TokenStream::new();
        self.mode.to_tokens(&mut state);
        self.free.to_tokens(&mut state);
        self.locked.to_tokens(&mut state);
        self.processed.to_tokens(&mut state);
        self.stack.to_tokens(&mut state);
        for stream in self.unprocessed.iter().rev() {
            state.extend(stream.clone());
        }

        tokens.append(Group::new(self.delim, state));
    }
}

pub struct FullyProccesedState(State);

impl ToTokens for FullyProccesedState {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.free.append_to_stream(tokens);
        self.0.locked.append_to_stream(tokens);
        self.0.processed.append_to_stream(tokens);
    }
}

impl State {
    // Should only be called with the top level stream
    pub fn decode_from_stream<F>(stream: TokenStream, expansion: F) -> Result<Self, Diagnostic>
    where
        F: FnOnce(token_stream::IntoIter) -> token_stream::IntoIter,
    {
        use Delimiter::Bracket;

        let span = Span::call_site();
        let mut stream = stream.into_iter();
        expect_literal(stream.next_or(span), EAGER_CALL_SIGIL)?;

        let group = expect_group(stream.next_or(span), Bracket).unwrap_or_abort();

        let span = group.span();
        let mut group = group.stream().into_iter();
        let state =
            expect_group(group.next_or(span), Param::Named("eager2_state")).unwrap_or_abort();
        if group.next().is_some() {
            abort!(span, "`eager2_state` should only contain one group")
        }

        Ok(Self::decode_from_group(state, Some(expansion(stream))))
    }

    // Only should be called on encoded groups
    fn decode_from_group(g: Group, extra: Option<token_stream::IntoIter>) -> Self {
        use Delimiter::Bracket;

        let span = g.span();
        let delim = g.delimiter();

        let mut stream = g.stream().into_iter();
        let mode = Mode::from(g.span(), stream.next());
        let free = expect_group(stream.next_or(span), Bracket).unwrap_or_abort();
        let locked = expect_group(stream.next_or(span), Bracket).unwrap_or_abort();
        let processed = expect_group(stream.next_or(span), Bracket).unwrap_or_abort();
        let stack = expect_group(stream.next_or(span), Param::Named("stack")).unwrap_or_abort();

        let unprocessed = if let Some(extra) = extra {
            vec![stream, extra]
        } else {
            vec![stream]
        };

        Self {
            span,
            delim,
            mode,
            free: free.into(),
            locked: locked.into(),
            processed: processed.into(),
            stack: Stack::Raw(stack),
            unprocessed,
        }
    }

    pub fn new(span: Span, delim: Delimiter, mode: Mode, stream: TokenStream) -> Self {
        Self {
            span,
            delim,
            mode,
            free: Default::default(),
            locked: Default::default(),
            processed: Default::default(),
            stack: Stack::Empty,
            unprocessed: vec![stream.into_iter()],
        }
    }

    pub fn process(
        mut self,
        found_crate: &str,
    ) -> Result<FullyProccesedState, (State, MacroPath, TokenStream)> {
        while !self.unprocessed.is_empty() || !self.stack.is_empty() {
            if let Some((path, stream)) = self.process_one(found_crate) {
                return Err((self, path, stream));
            }
        }
        Ok(FullyProccesedState(self))
    }

    fn process_one(&mut self, found_crate: &str) -> Option<(MacroPath, TokenStream)> {
        while let Some(unprocessed) = self.unprocessed.last_mut() {
            for tt in unprocessed {
                let g = match tt {
                    TokenTree::Group(g) => g,
                    tt => {
                        self.processed.push(tt);
                        continue;
                    }
                };

                // lookback to see if this group is a macro param
                // (looking especially for mode-switching macros)
                let tm = TrailingMacro::try_new(
                    found_crate,
                    self.processed.as_mut_vec(),
                    self.mode == Mode::Lazy,
                );

                let inner_mode = tm.as_ref().and_then(|tm| tm.mode()).unwrap_or(self.mode);

                #[cfg(feature = "debug")]
                proc_macro_error2::emit_call_site_warning!(
                    "processing with mode {:?}: {}",
                    inner_mode,
                    g
                );

                let stack = State::new(g.span(), g.delimiter(), inner_mode, g.stream());
                let stack = mem::replace(self, stack);
                self.stack = Stack::Processed(Box::new(stack), tm);
                return None;
            }
            self.unprocessed.pop();
        }

        // `stack`` is the caller, `self` is the completed recursive call
        let (mut stack, tm) = match self.stack.take() {
            Stack::Empty => return None,
            Stack::Processed(p, tm) => (*p, tm),
            Stack::Raw(r) if r.stream().is_empty() => return None,
            Stack::Raw(r) => {
                let mut p = Self::decode_from_group(r, None);
                let tm = TrailingMacro::try_new(
                    found_crate,
                    p.processed.as_mut_vec(),
                    self.mode == Mode::Lazy,
                );
                (p, tm)
            }
        };

        // `self` is the caller, `stack` is the completed recurisve call
        mem::swap(self, &mut stack);

        use TrailingMacro::{Exec, ModeSwitch, Unknown};

        match (tm, self.mode) {
            // Handle mode switches
            (Some(ModeSwitch(tm)), _) => {
                match (
                    self.mode,
                    tm.truncate(self.processed.as_mut_vec()),
                    self.locked.is_empty(),
                    stack.locked.is_empty(),
                ) {
                    (_, Mode::Eager, _, true) => {
                        self.processed.append(stack.processed);
                    }
                    (_, Mode::Eager, true, false) => {
                        debug_assert!(self.free.is_empty());
                        self.free.append(self.processed.take());
                        self.free.append(stack.free);
                        self.locked = stack.locked;
                        self.processed.append(stack.processed);
                    }
                    (_, Mode::Eager, false, false) => {
                        self.locked.append(self.processed.take());
                        self.locked.append(stack.locked);
                        self.processed.append(stack.processed);
                    }

                    (Mode::Eager, Mode::Lazy, true, _) => {
                        debug_assert!(self.free.is_empty());
                        self.free.append(self.processed.take());
                        self.free.append(stack.free);
                        self.locked = stack.locked;
                        self.locked.append(stack.processed);
                    }
                    (Mode::Eager, Mode::Lazy, false, _) | (Mode::Lazy, Mode::Lazy, _, _) => {
                        self.locked.append(self.processed.take());
                        self.locked.append(stack.free);
                        self.locked.append(stack.locked);
                        self.locked.append(stack.processed);
                    }
                }
            }
            // Execute known macros
            (Some(Exec(tm)), Mode::Eager) => {
                let macro_type = tm.truncate(self.processed.as_mut_vec());
                let stream = stack
                    .free
                    .into_iter()
                    .chain(stack.locked)
                    .chain(stack.processed);

                macro_type.execute(stack.span, stream, self)
            }
            // Hand over execution to unknown macros if in eager mode
            (Some(Unknown(tm)), Mode::Eager) => {
                let segments = tm.split_off(self.processed.as_mut_vec());
                let path = MacroPath { segments };
                stack.free.append(stack.locked);
                stack.free.append(stack.processed);
                let stream = stack.free.into_stream();
                return Some((path, stream));
            }

            (_, Mode::Lazy) | (None, Mode::Eager) => {
                stack.free.append(stack.locked);
                stack.free.append(stack.processed);
                let stream = stack.free.into_stream();
                let group = Group::new(stack.delim, stream);

                #[cfg(feature = "debug")]
                proc_macro_error2::emit_call_site_warning!("finished_processing {}", group);

                self.processed.push(TokenTree::Group(group));
            }
        }

        None
    }
}
