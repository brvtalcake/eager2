use std::fmt::Write;
use std::path::{self, Path, PathBuf};
use std::str::FromStr;
use std::{env, fs};

use litrs::{BoolLit, Literal as Litrl};
use proc_macro_error2::ResultExt;
use proc_macro_error2::abort;
use proc_macro2::{Group, Ident, Literal, Span, TokenStream, TokenTree, token_stream};

use crate::egroup::EfficientGroupV;
use crate::utils::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ExecutableMacroType {
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

impl ExecutableMacroType {
    pub fn execute(
        self,
        span: Span,
        stream: impl Clone + IntoIterator<Item = TokenTree>,
        processed: &mut EfficientGroupV,
        unprocessed: &mut Vec<token_stream::IntoIter>,
    ) {
        #[cfg(feature = "trace_macros")]
        proc_macro_error2::emit_call_site_warning!(
            "executing {:?}: {}",
            self,
            TokenStream::from_iter(stream.clone())
        );

        match self {
            Self::Concat => {
                execute_concat(span, stream, processed);
            }
            Self::Env => {
                execute_env(span, stream, processed);
            }
            Self::Stringify => {
                execute_stringify(stream, processed);
            }
            Self::Include => {
                execute_include(span, stream, unprocessed);
            }
            Self::IncludeStr => {
                execute_include_str(span, stream, processed);
            }
            Self::CompileError => {
                execute_compile_error(span, stream);
            }

            Self::CCase => {
                execute_ccase(span, stream, processed);
            }
            Self::TokenEq => {
                execute_token_eq(span, stream, processed);
            }
            Self::EagerIf => {
                execute_eager_if(span, stream, unprocessed);
            }
            Self::Unstringify => {
                execute_unstringify(span, stream, unprocessed);
            }
        }
    }
}

pub fn execute_env(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    let mut args = stream.into_iter();
    let (key, _) = expect_string_literal(args.next_or(span), Param::Named("key")).unwrap_or_abort();
    args.next()
        .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());
    let error = args
        .next()
        .map(|tt| expect_string_literal(Ok(tt), Param::Named("error")).unwrap_or_abort());
    if args.next().is_some() {
        abort!(span, "`env!()` takes 1 or 2 arguments")
    }

    let value = match (env::var(&key), error) {
        (Ok(value), _) => value,
        (Err(_), Some((error, _))) => abort!(span, "{}", error),
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

pub fn execute_stringify(
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    let s = TokenStream::from_iter(stream).to_string();
    processed_out.push(TokenTree::Literal(Literal::string(&s)));
}

pub fn execute_concat(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    let mut buffer = String::new();
    let mut args = stream.into_iter();
    while let Some(tt) = args.next() {
        
        let (tt, neg) = match expect_punct(Ok(tt.clone()), '-') {
            Ok(_) => (args.next_or(span), true),
            Err(_) => (Ok(tt), false)
        };

        let (l, span) = expect_literal(tt, Param::Named("arg")).unwrap_or_abort();
        if neg {
            buffer.push('-');
        }

        match (neg, l) {
            (_, Litrl::Byte(_) | Litrl::ByteString(_)) => abort!(span, "cannot concatenate a byte string literal"),
            (false, Litrl::Bool(BoolLit::False)) => buffer.push_str("false"),
            (false, Litrl::Bool(BoolLit::True)) => buffer.push_str("true"),
            (_, Litrl::Float(f)) => {
                for s in f.number_part().split('_') {
                    buffer.push_str(s)
                }
            },
            (false, Litrl::Char(c)) => buffer.push(c.value()),
            (false, Litrl::String(s)) => buffer.push_str(s.value()),
            (_, Litrl::Integer(i)) => {
                use litrs::IntegerBase::*;
                match (i.base(), i.suffix()) {
                    (Decimal, "f32" | "f64" | "" | "u8" | "u16" | "u32" | "u64" | "u128" | "usize" | "i8" | "i16" | "i32" | "i64" | "i128") => {
                        for s in i.raw_main_part().split('_') {
                            buffer.push_str(s)
                        }
                    },
                    (Binary, "f32" | "f64") => abort!(span, "binary float literal is not supported"),
                    (Octal, "f32" | "f64") => abort!(span, "octal float literal is not supported"),

                    (_, "" | "u8" | "u16" | "u32" | "u64" | "u128" | "usize" | "i8" | "i16" | "i32" | "i64" | "i128") => {
                        let Some(v) = i.value::<u128>() else {
                            abort!(span, "integer literal is too large";
                                note = "value exceeds limit of `0xffffffffffffffffffffffffffffffff`")
                        };
                        write!(&mut buffer, "{v}").unwrap();
                    }
                    (_, s) => abort!(span, "invalid suffix `{}` for number literal", s),

                }
            }
            _ => abort!(span, "expected a literal";
                    note = r#"only literals (like `"foo"`, `-42` and `3.14`) can be passed to `concat!()`"#)
        }
        args.next()
            .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());
    }
    processed_out.push(TokenTree::Literal(Literal::string(&buffer)));
}

fn include_helper(span: Span, stream: impl IntoIterator<Item = TokenTree>) -> String {
    let mut args = stream.into_iter();
    let (file, file_span) = expect_string_literal(args.next_or(span), Param::Named("file")).unwrap_or_abort();
    args.next()
        .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());

    let mut path = Path::new(&file);
    if path.is_relative() {
        abort!(
            file_span,
            r#"relative path is not supported here; use `include!(concat!(env!("CARGO_MANIFEST_DIR"), ...))"#
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

pub fn execute_include(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    unprocessed: &mut Vec<token_stream::IntoIter>,
) {
    let content = include_helper(span, stream);
    let parsed = TokenStream::from_str(&content).unwrap();

    #[cfg(feature = "trace_macros")]
    proc_macro_error2::emit_call_site_warning!("include result:{}", parsed);

    unprocessed.push(parsed.into_iter());
}

pub fn execute_include_str(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    let content = include_helper(span, stream);
    let string = Literal::string(&content[1..content.len() - 1]);

    processed_out.push(string.into());
}

pub fn execute_compile_error(span: Span, stream: impl IntoIterator<Item = TokenTree>) {
    let mut args = stream.into_iter();
    let (msg, _) = expect_string_literal(args.next_or(span), Param::Named("msg")).unwrap_or_abort();
    args.next()
        .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());
    if args.next().is_some() {
        abort!(span, "`compile_error!()` takes 1 arguments")
    }

    abort!(span, "{}", msg);
}

pub fn execute_ccase(
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
        let (arg_val, arg_span) = expect_string_literal(args.next_or(span), Param::Named("val")).unwrap_or_abort();
        args.next()
            .map(|tt| expect_punct(Ok(tt), ',').unwrap_or_abort());

        *dest = Some((
            arg_name,
            arg_span,
            arg_val,
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
            let result = conv.convert(&string);
            Literal::string(&result).into()
        }
    };
    processed_out.push(result);
}

pub fn execute_token_eq(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    processed_out: &mut EfficientGroupV,
) {
    let mut args = stream.into_iter();
    struct TtWrapper(TokenTree);
    impl PartialEq for TtWrapper {
        fn eq(&self, other: &Self) -> bool {
            match (
                eat_zero_group(self.0.clone()),
                eat_zero_group(other.0.clone()),
            ) {
                (TokenTree::Group(a), TokenTree::Group(b)) => group_eq(&a, &b),
                (TokenTree::Ident(a), TokenTree::Ident(b)) => a == b,
                (TokenTree::Punct(a), TokenTree::Punct(b)) => a.as_char() == b.as_char(),
                (TokenTree::Literal(a), TokenTree::Literal(b)) => a.to_string() == b.to_string(),
                _ => false,
            }
        }
    }
    fn stream_eq(a: TokenStream, b: TokenStream) -> bool {
        a.is_empty() == b.is_empty()
            && a.into_iter()
                .map(TtWrapper)
                .eq(b.into_iter().map(TtWrapper))
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

pub fn execute_eager_if(
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

pub fn execute_unstringify(
    span: Span,
    stream: impl IntoIterator<Item = TokenTree>,
    unprocessed: &mut Vec<token_stream::IntoIter>,
) {
    let mut args = stream.into_iter();
    let (src, _) = expect_string_literal(args.next_or(span), Param::Named("src")).unwrap_or_abort();
    args.next()
        .map(|v| expect_punct(Ok(v), ',').unwrap_or_abort());

    let unstrung = TokenStream::from_str(&src).unwrap();

    #[cfg(feature = "trace_macros")]
    proc_macro_error2::emit_call_site_warning!("unstringify result:{}", unstrung);

    unprocessed.push(unstrung.into_iter())
}
