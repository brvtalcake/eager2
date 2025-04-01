use std::mem;

use litrs::Literal;
use proc_macro_error2::abort;
use proc_macro_error2::{Diagnostic, ResultExt};
use proc_macro2::{Delimiter, Group, Ident, Spacing, Span, TokenStream, TokenTree, token_stream};
use quote::{ToTokens, TokenStreamExt};

use crate::egroup::{EfficientGroupT, EfficientGroupV};
use crate::exec::ExecutableMacroType;
#[allow(clippy::wildcard_imports)]
use crate::utils::*;

/// Mode is the recordable state of the macro execution environment.
///
/// It can either be eager or lazy, represented by `EAGER_SIGIL` and `LAZY_SIGIL` respectively.
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

#[derive(Debug)]
struct Suspend;

impl TryFrom<&'_ str> for Suspend {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        if s == "suspend_eager" {
            Ok(Self)
        } else {
            Err(())
        }
    }
}

enum MacroType {
    Suspend(Suspend),
    ModeSwitch(Mode),
    Exec(ExecutableMacroType),
    Unknown,
}

impl MacroType {
    fn try_new(found_crate: &str, tokens: &[TokenTree], mode_only: bool) -> Option<Self> {
        fn token_is(tt: &TokenTree, s: &str) -> bool {
            let TokenTree::Ident(i) = tt else {
                unreachable!()
            };
            i == s
        }
        fn tokens_are<'a>(
            tokens: &[TokenTree],
            i: impl IntoIterator<Item = (usize, &'a str)>,
        ) -> bool {
            i.into_iter().all(|(i, s)| token_is(&tokens[i], s))
        }
        fn ident_to_string(tt: &TokenTree) -> String {
            let TokenTree::Ident(i) = tt else {
                unreachable!()
            };
            i.to_string()
        }

        enum PathCrateErr {
            NoCrate,
            DollarCrate,
        }
        use PathCrateErr::*;

        enum IgnoreMode {
            DoNot,
            Prelude,
            Std,
            Core,
            Alloc,
        }

        let crate_i = match tokens.len() {
            // Zero isn't enough
            // One token is just a `!` which isn't enough
            0 | 1 => return None,

            // e.g. [`eager`, `!`]
            2 => Err(NoCrate),

            // e.g. [`eager2`, `:`, `:`, `eager`, `!`]
            5 => Ok(0),
            // e.g. [`:`, `:`, `eager2`, `:`, `:`, `eager`, `!`]
            7 => Ok(2),
            // e.g. [`$`, `crate`, `:`, `:`, `eager2`, `:`, `:`, `eager`, `!`]
            9 if tokens_are(tokens, [(1, "crate"), (4, "eager2")]) => Err(DollarCrate),
            _ if mode_only => return None,
            _ => return Some(Self::Unknown),
        };
        let crate_name = crate_i.map(|i| ident_to_string(&tokens[i]));
        let crate_name = crate_name.as_ref().map(String::as_str);

        let (ignore, exec) = match crate_name {
            // e.g. [`eager`, `!`]
            Err(NoCrate) => (IgnoreMode::Prelude, true),

            // e.g. [`$`, `crate`, `:`, `:`, `eager2`, `:`, `:`, `eager`, `!`]
            Err(DollarCrate) => (IgnoreMode::DoNot, true),

            // e.g. [`eager2`, `:`, `:`, `eager`, `!`]
            // e.g. [`:`, `:`, `eager2`, `:`, `:`, `eager`, `!`]
            Ok("std") => (IgnoreMode::Std, false),
            Ok("core") => (IgnoreMode::Core, false),
            Ok("alloc") => (IgnoreMode::Alloc, false),
            Ok(s) if s == found_crate => (IgnoreMode::DoNot, true),
            Ok(_) => return None,
        };

        let macro_name = ident_to_string(&tokens[tokens.len() - 2]);
        let macro_name = macro_name.as_str();

        // Try mode switches first
        if let Ok(suspend) = macro_name.try_into() {
            return Some(Self::Suspend(suspend));
        }
        if let Ok(mode) = macro_name.try_into() {
            return Some(Self::ModeSwitch(mode));
        }

        // Stop if we're limited to mode only
        if mode_only {
            return None;
        }

        // Try exec
        if exec {
            if let Ok(exec) = macro_name.try_into() {
                return Some(Self::Exec(exec));
            }
        }
        // Try ignore
        match (ignore, macro_name) {
            (
                IgnoreMode::Prelude | IgnoreMode::Std | IgnoreMode::Core,
                "assert" | "assert_eq" | "assert_new" | "debug_assert" | "debug_assert_eq"
                | "debug_assert_ne" | "format_args" | "matches" | "panic" | "todo" | "try"
                | "unimplemented" | "unreachable" | "write" | "writeln",
            )
            | (
                IgnoreMode::Std | IgnoreMode::Core,
                "cfg" | "column" | "compile_error" | "concat" | "env" | "file" | "include"
                | "include_bytes" | "include_str" | "line" | "module_path" | "option_env"
                | "stringify",
            )
            | (
                IgnoreMode::Prelude | IgnoreMode::Std,
                "dbg"
                | "eprint"
                | "eprintln"
                | "is_x86_feature_detected"
                | "print"
                | "println"
                | "thread_local",
            )
            | (IgnoreMode::Prelude | IgnoreMode::Std | IgnoreMode::Alloc, "format" | "vec") => {
                return None;
            }
            _ => {}
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

#[derive(Debug)]
struct SuspendTrailingMacro {
    offset: usize,
}

impl TrailingMacro {
    fn try_new(
        found_crate: &str,
        tokens: &[TokenTree],
        mode_only: bool,
    ) -> Result<Option<Self>, SuspendTrailingMacro> {
        let offset = {
            let mut iter = tokens.iter().enumerate().rev();
            match iter.next() {
                // Exclamation found, continue checking
                Some((_, TokenTree::Punct(p))) if p.as_char() == '!' => {}
                // No exclamation
                None | Some(_) => return Ok(None),
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

        Ok(Some(
            match MacroType::try_new(found_crate, &tokens[offset..], mode_only) {
                None => return Ok(None),
                Some(MacroType::Suspend(_)) => return Err(SuspendTrailingMacro { offset }),
                Some(MacroType::ModeSwitch(mode)) => {
                    TrailingMacro::ModeSwitch(ModeSwitchTrailingMacro { offset, mode })
                }
                Some(MacroType::Exec(exec)) => {
                    TrailingMacro::Exec(ExecutableTrailingMacro { offset, exec })
                }
                Some(MacroType::Unknown) => TrailingMacro::Unknown(UnknownTrailingMacro { offset }),
            },
        ))
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
impl SuspendTrailingMacro {
    fn truncate(self, tokens: &mut Vec<TokenTree>) {
        tokens.truncate(self.offset);
    }
}

pub struct MacroPath {
    pub segments: Vec<TokenTree>,
}
impl ToTokens for MacroPath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(self.segments.iter());
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
        let sigil = Literal::parse(EAGER_CALL_SIGIL).unwrap().into_owned();
        expect_literal(stream.next_or(span), sigil)?;

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
    #[allow(clippy::needless_pass_by_value)]
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
            free: EfficientGroupT::default(),
            locked: EfficientGroupT::default(),
            processed: EfficientGroupV::default(),
            stack: Stack::Empty,
            unprocessed: vec![stream.into_iter()],
        }
    }
    
    #[allow(clippy::result_large_err)]
    pub fn process(
        mut self,
        found_crate: &str,
    ) -> Result<FullyProccesedState, (State, MacroPath, TokenStream)> {
        while !self.unprocessed.is_empty() || !self.stack.is_empty() {
            self.process_tokens(found_crate);
            if let Some((path, stream)) = self.pop_stack(found_crate) {
                return Err((self, path, stream));
            }
        }
        Ok(FullyProccesedState(self))
    }

    fn has_locking(&self) -> bool {
        !self.free.is_empty() || !self.locked.is_empty()
    }

    fn process_tokens(&mut self, found_crate: &str) {
        while let Some(tt) = self.unprocessed.pop_next() {
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

            let tm = match tm {
                Ok(tm) => tm,
                Err(suspend) => {
                    suspend.truncate(self.processed.as_mut_vec());
                    let locked = EfficientGroupT::Processed(g.stream());
                    if self.has_locking() {
                        self.locked.append(self.processed.take());
                        self.locked.append(locked);
                    } else {
                        self.free.append(self.processed.take());
                        self.locked = locked;
                    }
                    continue;
                }
            };

            let inner_mode = tm
                .as_ref()
                .and_then(TrailingMacro::mode)
                .unwrap_or(self.mode);

            #[cfg(feature = "trace_macros")]
            proc_macro_error2::emit_call_site_warning!(
                "processing with mode {:?}: {}",
                inner_mode,
                g
            );

            let stack = State::new(g.span(), g.delimiter(), inner_mode, g.stream());
            let stack = mem::replace(self, stack);
            self.stack = Stack::Processed(Box::new(stack), tm);
        }
    }

    fn pop_stack(&mut self, found_crate: &str) -> Option<(MacroPath, TokenStream)> {
        use TrailingMacro::{Exec, ModeSwitch, Unknown};

        debug_assert!(self.unprocessed.is_empty());

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
                (p, tm.unwrap())
            }
        };

        // `self` is the caller, `stack` is the completed recursive call
        mem::swap(self, &mut stack);

        match (tm, self.mode) {
            // Handle mode switches
            (Some(ModeSwitch(tm)), _) => {
                match (
                    self.mode,
                    tm.truncate(self.processed.as_mut_vec()),
                    self.has_locking(),
                    stack.has_locking(),
                ) {
                    (_, Mode::Eager, _, false) => {
                        self.processed.append(stack.processed);
                    }
                    (_, Mode::Eager, false, true) => {
                        self.free.append(self.processed.take());
                        self.free.append(stack.free);
                        self.locked = stack.locked;
                        self.processed.append(stack.processed);
                    }
                    (_, Mode::Eager, true, true) => {
                        self.locked.append(self.processed.take());
                        self.locked.append(stack.locked);
                        self.processed.append(stack.processed);
                    }

                    (Mode::Eager, Mode::Lazy, false, _) => {
                        self.free.append(self.processed.take());
                        self.free.append(stack.free);
                        self.locked = stack.locked;
                        self.locked.append(stack.processed);
                    }
                    (Mode::Eager, Mode::Lazy, true, _) | (Mode::Lazy, Mode::Lazy, _, _) => {
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

                macro_type.execute(
                    stack.span,
                    stream,
                    &mut self.processed,
                    &mut self.unprocessed,
                );
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

                #[cfg(feature = "trace_macros")]
                proc_macro_error2::emit_call_site_warning!("finished_processing {}", group);

                self.processed.push(TokenTree::Group(group));
            }
        }

        None
    }
}
