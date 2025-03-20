//!
//! This crate contians three macros used to simulate eager macro expansion:
//!
//! 1. `eager!`: Eagerly expands any macro in its body.
//! 2. `eager_macro_rules!`: Used to declare macro that can be eagerly expanded with `eager!`.
//! 3. `lazy!`: Used in `eager!` to revert to lazy macro expansion.
//!
//! See the each macro's documentation for details.
//!
use std::mem;

use dyn_clone::DynClone;
use proc_macro2::{token_stream, Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens, TokenStreamExt};
use proc_macro_crate::{crate_name, FoundCrate};
use proc_macro_error2::proc_macro_error;
use proc_macro_error2::{abort, abort_call_site};

/// Declares [eager!](macro.eager.html)-enabled macros.
///
/// # Usage
///
/// Wraps the usual `macro_rules!` syntax. First an identifier must be given, preceded by '$'.
/// Then any number of macro declarations can be given using the usual `macro_rules!` syntax.
/// Documentation and attributes are also given in the
/// usual way just before each `macro_rules!`, i.e. inside `eager_macro_rules!`.
///
/// Some restrictions apply to the `macro_rules!` declarations:
///
/// * The identifier given at the beginning must not collide with any macro variable name
/// used in any rule in any macro to be declared.
/// * No rules should accept `@eager` as the first token, as this could conflict with the
/// implementation of `eager!`. Wildcards are acceptable, as `eager_macro_rules!` will automatically
/// resolve the ambiguity with the `eager!` implementation.
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

    let found_crate = match crate_name("eager2") {
        Err(e) => abort_call_site!(
            "eager2 is not present in `Cargo.toml`";
            error = "{}", e;
        ),
        Ok(FoundCrate::Itself) => Ident::new("crate", Span::call_site()),
        Ok(FoundCrate::Name(name)) => Ident::new(&name, Span::call_site()),
    };

    let mut stream = TokenStream::from(stream).into_iter().peekable();


    let hidden_ident = match stream.peek() {
        None => return proc_macro::TokenStream::new(),

        Some(TokenTree::Punct(p)) if p.as_char() == '$' => {
            let dollar = stream.next();
            match stream.next() {
                None => abort!(dollar, "expected ident after $"),
                Some(TokenTree::Ident(ident)) => ident,
                Some(_token) => abort!(_token, "expected ident after $"),
            }
        }
        Some(_token) => Ident::new("__eager2_ident_hyR7dMdkMPcptU6h21dioFE3EhoLprgj", Span::call_site()),
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
                        },
                        Some(_token) => abort!(_token, "expected ["),
                    }
                }
                Some(TokenTree::Ident(i)) if i.to_string() == "macro_rules" => break,
                Some(t) => abort!(t, "expected # or macro_rules"),
            }
        }
        let _macro_rules = stream.next().unwrap();
        match stream.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == '!' => {},
            None => abort_call_site!("expected !"),
            Some(t) => abort!(t, "expected !"),
        }
        let macro_name = match stream.next() {
            Some(TokenTree::Ident(i)) => { i },
            None => abort_call_site!("expected ident"),
            Some(t) => abort!(t, "expected ident"),
        };
        let group = match stream.next() {
            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => { g },
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
                Some(TokenTree::Punct(p)) if p.as_char() == '=' && p.spacing() == Spacing::Joint => {},
                None => abort_call_site!("expected ="),
                Some(t) => abort!(t, "expected ="),
            }
            match stream.next() {
                Some(TokenTree::Punct(p)) if p.as_char() == '>' => {},
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
                    rules.push(Rule{grammer, expansion});
                    break;
                },
                Some(TokenTree::Punct(p)) if p.as_char() == ';' => {
                    rules.push(Rule{grammer, expansion});
                },
                Some(t) => abort!(t, "expected ;"),
            }
        }
    
        let eager_rules = rules.iter().map(|Rule { grammer, expansion }| {
            let grammer = grammer.stream();
            let expansion = expansion.stream();
            quote!{
                (
                    @eager[$($#hidden_ident:tt)*]
                    #grammer
                ) => {
                    #found_crate::eager_internal!{
                        @from_macro[$($#hidden_ident)*]
                        #expansion
                    }
                };
            }
        });
        let pure_rules = rules.iter().map(|Rule { grammer, expansion }| {
            let grammer = grammer.stream();
            let expansion = expansion.stream();
            quote!{
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
    proc_macro_error2::emit_call_site_warning!{"eager_macro_rules output: {}", output}
    
    output.into()
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Mode {
    Eager,
    Lazy
}

impl TryFrom<Group> for Mode {
    type Error = (Span, &'static str);
    fn try_from(g: Group) -> Result<Self, Self::Error> {
        debug_assert_eq!(g.delimiter(), Delimiter::Bracket);

        let mut stream = g.stream().into_iter();
        match stream.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == '@' && p.spacing() == Spacing::Alone => {},
            None => return Ok(Self::Eager),
            Some(t) => return Err((t.span(), "expected @")),
        }
        match stream.next() {
            Some(TokenTree::Ident(i)) if i.to_string() == "lazy" => Ok(Self::Lazy),
            None => Err((Span::call_site(), "expected lazy")),
            Some(t) => Err((t.span(), "expected lazy")),
        }
    }
}

impl TryFrom<&'_ [TokenTree; 2]> for Mode {
    type Error = ();
    fn try_from(value: &'_ [TokenTree; 2]) -> Result<Self, Self::Error> {
        let i = match value {
            [TokenTree::Ident(i), TokenTree::Punct(p)] if p.as_char() == '!' => i,
            _ => return Err(())
        };
        match i.to_string().as_str() {
            "eager" => Ok(Mode::Eager),
            "lazy" => Ok(Mode::Lazy),
            _ => Err(())
        }

    }
}
impl ToTokens for Mode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut inner = TokenStream::new();
        if let Mode::Lazy = self {
            inner.append(Punct::new('@', Spacing::Alone));
            inner.append(Ident::new("lazy", Span::call_site()));
        }
        let group = Group::new(Delimiter::Bracket, inner);
        tokens.append(group);
    }
}

struct MacroPath {
    pub segments: Vec<TokenTree>,
}

impl MacroPath {
    fn try_from_end(tokens: &mut Vec<TokenTree>) -> Option<Self> {
        let i = {
            let mut iter = tokens.iter().enumerate().rev();
            match iter.next() {
                // Exclamation found, continue checking
                Some((_, TokenTree::Punct(p))) if p.as_char() == '!' => {},
                // No exclamation
                None | Some(_) => return None,
            }

            loop {
                match iter.next() {
                    // All tokens are valid
                    None => break 0,
                    // Continue checking on ident
                    Some((_, TokenTree::Ident(_))) => {},
                    Some((i, _)) => break i+1,
                }
                match iter.next() {
                    // All tokens are valid
                    None => break 0,
                    // Continue checking on colon
                    Some((_, TokenTree::Punct(p))) if p.as_char() == ':' => {},
                    // Something other than colon, so end and exclude
                    Some((i, _)) => break i+1,
                }
                match iter.next() {
                    // All tokens except the last ':' are valid
                    None => break 1,
                    // Continue checking on second colon
                    Some((_, TokenTree::Punct(p))) if p.as_char() == ':' && p.spacing() == Spacing::Joint => {},
                    // Something other than second colon, so end and exclude with colon
                    Some((i, _)) => break i+2,
                }
                
            }
        };

        // Need ident
        if i == tokens.len() - 1 {
            None
        } else {
            Some(Self{
                segments: tokens.split_off(i)
            })
        }
    }
}

impl ToTokens for MacroPath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(self.segments.iter())
    }
}

/// Emulates eager expansion of macros.
///
/// # Example
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
/// To enable the use of non-`eager!`-enabled macros inside an `eager!` call,
/// a `lazy!` block can be inserted. Everything inside the `lazy!` block will be lazily expanded,
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
/// * Debugging an eagerly expanded macro can be quite difficult. This crate has a`debug` feature
/// which only operates on `nightly`, and can be quite verbose. Contributions to improve this
/// are very welcome.
///
/// * Only `eager!`-enabled macros can be eagerly expanded, so existing macros do not gain much.
/// The `lazy!` block alleviates this a bit, by allowing the use of existing macros in it,
/// while eager expansion can be done around them.
/// Luckily, `eager!`-enabling an existing macro should not be too much
/// trouble using [`eager_macro_rules!`].
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
/// ### Documentation
///
/// To make it clearly visible that a given macro is `eager!`-enabled, its short rustdoc description
/// must start with a pair of brackets, within which a link to the official `eager!` macro documentation
/// must be provided. The link's visible text must be 'eager!' and
/// the brackets must not be part of the link.
///
/// ### Auxiliary variable
///
/// For compatibility with `eager`, an auxiliary variable may be provided to `eager_macro_rules!`
/// but it is not needed.
#[proc_macro]
#[proc_macro_error]
pub fn eager(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!{"eager input: {}", stream}

    let state = EagerState::new(Delimiter::Bracket, Mode::Eager, stream.into());
    let output = match state.process() {
        Err(processed) => quote! { #(#processed)* },
        Ok((state, eager_macro, stream)) => {
            quote! {
                #eager_macro{@eager #state #stream}
            }
        }
    };

    proc_macro_error2::emit_call_site_warning!{"eager output: {}", output}

    output.into()
}


/// [[eager!](macro.eager.html)] Used within an [`eager!`](macro.eager.html) to revert to lazy expansion.
///
/// If this macro is called independently of `eager!`, it is equivalent to `eager!{lazy!{...}}`.
#[proc_macro]
#[proc_macro_error]
pub fn lazy(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!{"lazy input: {}", stream}

    let state = EagerState::new(Delimiter::Bracket, Mode::Lazy, stream.into());
    let output = match state.process() {
        Err(processed) => quote! { #(#processed)* },
        Ok((state, eager_macro, stream)) => {
            quote! {
                #eager_macro{@eager #state #stream}
            }
        }
    };

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!{"lazy output: {}", output}

    output.into()
}

#[doc(hidden)]
#[proc_macro]
#[proc_macro_error]

pub fn eager_internal(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!{"eager_internal input: {}", stream}

    let mut stream = TokenStream::from(stream).into_iter();
    
    match stream.next() {
        Some(TokenTree::Punct(p)) if p.as_char() == '@' && p.spacing() == Spacing::Alone => {},
        None => abort_call_site!("expected @ sigil"),
        Some(t) => abort!(t, "expected @ sigil"),
    }
    match stream.next() {
        Some(TokenTree::Ident(i)) if i.to_string() == "from_macro" => {},
        None => abort_call_site!("expected from_macro"),
        Some(t) => abort!(t, "expected from_macro"),
    };
    let group = match stream.next() {
        Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Bracket => { g },
        None => abort_call_site!("expected ["),
        Some(t) => abort!(t, "expected ["),
    };
    let mut state = match EagerState::try_from(group) {
        Ok(state) => state,
        Err((span, err)) => abort!(span, "{}", err),
    };
    state.inject_expanded(stream);
    let output = match state.process() {
        Err(processed) => quote! { #(#processed)* },
        Ok((state, eager_macro, stream)) => {
            quote! {
                #eager_macro{@eager #state #stream}
            }
        }
    };

    #[cfg(feature = "debug")]
    proc_macro_error2::emit_call_site_warning!{"eager_internal output: {}", output}

    output.into()
}

struct EagerState {
    delim: Delimiter,
    mode: Mode,
    processed: Vec<TokenTree>,
    stack: Option<Box<EagerState>>,
    unprocessed: Box<dyn TokenIterator>,
}

trait TokenIterator: DynClone+Iterator<Item=TokenTree>{}

impl<T: DynClone+Iterator<Item=TokenTree>> TokenIterator for T {}

dyn_clone::clone_trait_object!(TokenIterator);


impl ToTokens for EagerState {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut output = TokenStream::new();
        self.mode.to_tokens(&mut output);
        output.append(Group::new(Delimiter::Bracket, self.processed.iter().cloned().collect()));
        if let Some(stack) = self.stack.as_ref() {
            stack.to_tokens(&mut output);
        } else {
            output.append(Group::new(Delimiter::Bracket, TokenStream::new()));
        }
        output.append_all(dyn_clone::clone_box(&*self.unprocessed));
        
        tokens.append(Group::new(self.delim, output));
    }
}

impl TryFrom<Group> for EagerState {
    type Error = (Span, &'static str);
    fn try_from(g: Group) -> Result<Self, Self::Error> {
        let delim = g.delimiter();

        let mut stream = g.stream().into_iter();
        let mode = match stream.next() {
            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Bracket => Mode::try_from(g),
            None => Err((Span::call_site(), "expected [")),
            Some(t) => Err((t.span(), "expected [")),
        }?;
        let processed = match stream.next() {
            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Bracket => {
                g.stream().into_iter().collect()
            },
            None => return Err((Span::call_site(), "expected [")),
            Some(t) => return Err((t.span(), "expected [")),
        };
        let stack = match stream.next() {
            Some(TokenTree::Group(g)) => {
                if g.stream().is_empty() {
                    None
                } else {
                    Some(Box::new(Self::try_from(g)?))
                }
            },
            None => return Err((Span::call_site(), "expected {{ or [ or (")),
            Some(t) => return Err((t.span(), "expected {{ or [ or (")),
        };

        Ok(Self { delim, mode, processed, stack, unprocessed: Box::new(stream) })
    }
}

impl EagerState {
    fn new(delim: Delimiter, mode: Mode, stream: TokenStream) -> Self {
        Self {
            delim,
            mode,
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
    fn process(mut self) -> Result<(Self, MacroPath, TokenStream), Vec<TokenTree>> {
        if let Some(stack) = self.stack.take() {
            let delim = stack.delim;
            match stack.process() {
                Ok((state, path, stream)) => {
                    self.stack = Some(Box::new(state));
                    return Ok((self, path, stream));
                }
                Err(processed) => {
                    if self.processed.last_chunk::<2>().and_then(|v| Mode::try_from(v).ok()).is_some() {
                        // Erase switching macros
                        self.processed.truncate(self.processed.len() - 2);
                        self.processed.extend_from_slice(&processed);
                    } else {
                        if self.mode == Mode::Lazy {
                        } else if let Some(path) = MacroPath::try_from_end(&mut self.processed) {
                            let stream = processed.into_iter().collect();
                            return Ok((self, path, stream))
                        }
                        let stream = processed.into_iter().collect();
                        self.processed.push(TokenTree::Group(Group::new(delim, stream)));
                    }
                }
            }
        }

        while let Some(token) = self.unprocessed.next() {
            match token {
                TokenTree::Group(g) => {
                    // lookback to see if this group has a new mode
                    let mode = self.processed.last_chunk::<2>().and_then(|v| Mode::try_from(v).ok());
                    let stack = EagerState::new(g.delimiter(), mode.unwrap_or(self.mode), g.stream());

                    match stack.process() {
                        Ok((state, path, stream)) => {
                            self.stack = Some(Box::new(state));
                            return Ok((self, path, stream));
                        }
                        // Ignore mode switching macros
                        Err(processed) if mode.is_some() => {
                            self.processed.truncate(self.processed.len() - 2);
                            self.processed.extend_from_slice(&processed);
                        }
                        // Since we never hit a single macro, we can actually
                        // just use the stream from `g` as is
                        Err(_processed) => {
                            // Turn eager macros into calls
                            if self.mode == Mode::Eager {
                                if let Some(path) = MacroPath::try_from_end(&mut self.processed) {
                                    return Ok((self, path, g.stream()))
                                }
                            }
        
                            self.processed.push(TokenTree::Group(g))
                        }
                    }
                }
                t => self.processed.push(t),
            }
        }

        debug_assert!(self.stack.is_none());
        Err(self.processed)
    }
}
