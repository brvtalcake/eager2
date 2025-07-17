use std::iter::Peekable;

use crate::{
    consts::{
        crate_path, eager_call_sigil, eager_call_sigil_proc_macro, get_eager_2_ident,
        get_eager_2_pm_ident,
    },
    parse::{expect_group, expect_ident, expect_punct, Param},
    pm::{
        Delimiter, Group, Ident, Literal, Punct, Spacing, Span, ToTokens, TokenStream, TokenTree,
    },
    state::State,
    utils::{eager_data, NextOr},
    Error,
};

use proc_macro2::{TokenStream as TokenStream2, Ident as Ident2, Group as Group2, Span as Span2, Delimiter as Delimiter2, TokenTree as TokenTree2};

#[derive(Clone)]
enum MacroKind {
    /// `macro_rules! <macro-name> { ... }`
    DeclMacro1,
    /// `pub? macro <macro-name> ...`
    DeclMacro2 { visibility: TokenStream },
}

impl MacroKind {
    fn is_v1(&self) -> bool {
        matches!(self, Self::DeclMacro1)
    }
    fn is_v2(&self) -> bool {
        matches!(self, Self::DeclMacro2 { .. })
    }

    fn vis_mut(&mut self) -> Option<&mut TokenStream> {
        match self {
            Self::DeclMacro1 => None,
            Self::DeclMacro2 { visibility } => Some(visibility),
        }
    }
}

struct EagerProcMacro {
    eager_sigil: TokenStream2,
    has_proc_macro_attr: bool,
    attributes: TokenStream2,
    visibility: TokenStream2,
    name: Ident2,
    input: Ident2,
    input_type: TokenStream2,
    output_type: TokenStream2,
    body: Group2,
}

#[cfg(feature = "proc-macro-support")]
impl ToTokens for EagerProcMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use quote::quote;

        use crate::{parse::IdentOrString, utils::lifetime};

        let EagerProcMacro {
            input: user_input,
            input_type: user_input_type,
            output_type: user_output_type,
            eager_sigil,
            has_proc_macro_attr,
            attributes,
            visibility,
            name,
            body
        } = self;

        let mut maybe_proc_macro_attr = TokenStream::new();
        if !has_proc_macro_attr {
            maybe_proc_macro_attr.extend(Punct::new('#', Spacing::Alone).into_token_stream());
            maybe_proc_macro_attr.extend(
                Group::new(
                    Delimiter::Bracket,
                    Ident::new("proc_macro", Span::call_site()).into_token_stream(),
                )
                .into_token_stream(),
            );
        }
        let maybe_proc_macro_attr = Into::<TokenStream2>::into(maybe_proc_macro_attr);

        let input = get_eager_2_pm_ident(Some("input"));
        let user_lambda = get_eager_2_pm_ident(Some("user_lambda"));
        let input_copy = get_eager_2_pm_ident(Some("input_copy"));
        let iter = get_eager_2_pm_ident(Some("iter"));
        let eager_checks = Into::<TokenStream2>::into(lifetime(IdentOrString::String("eager_checks".to_owned())));
        let sigil = get_eager_2_pm_ident(Some("sigil"));
        let tmp = get_eager_2_pm_ident(Some("tmp"));
        let eager_group = get_eager_2_pm_ident(Some("eager_group"));
        let res = get_eager_2_pm_ident(Some("res"));
        let tt = get_eager_2_pm_ident(Some("tt"));
        tokens.extend([quote! {
            #maybe_proc_macro_attr
            #attributes
            #visibility fn #name (#input: ::proc_macro::TokenStream) -> ::proc_macro::TokenStream {
                let mut #user_lambda = |#user_input: #user_input_type| -> #user_output_type #body;
                let #input_copy = #input.clone();
                #eager_checks: {
                    let #sigil = #eager_sigil;
                    let mut #iter = #input.into_iter();
                    let mut #eager_group = ::proc_macro::TokenStream::new();
                    let mut #res = ::proc_macro::TokenStream::new();
                    
                    match #iter.next()
                    {
                        Some(#tt @ ::proc_macro::TokenTree::Literal(#tmp)) if #tmp.to_string() == #sigil.to_string() => {
                            #eager_group.extend([::proc_macro::TokenTree::from(#tt.clone())]);
                        },
                        _ => break #eager_checks,
                    }

                    match #iter.next()
                    {
                        Some(#tt @ ::proc_macro::TokenTree::Group(#tmp)) if #tmp.delimiter() == ::proc_macro::Delimiter::Bracket => {
                            #eager_group.extend([::proc_macro::TokenTree::from(#tt.clone())]);
                        },
                        _ => break #eager_checks,
                    }

                    #eager_group.extend([
                        Into::<::proc_macro::TokenStream>::into(
                            #user_lambda(
                                #iter.collect::<::proc_macro::TokenStream>()
                                    .into::<#user_input_type>()
                            )
                        )
                    ]);

                    #res.extend([
                        ::proc_macro::TokenTree::from(
                            ::proc_macro::Punct::new(':', ::proc_macro::Spacing::Joint)
                        ),
                        ::proc_macro::TokenTree::from(
                            ::proc_macro::Punct::new(':', ::proc_macro::Spacing::Joint)
                        ),
                        ::proc_macro::TokenTree::from(
                            ::proc_macro::Ident::new("eager2", ::proc_macro::Span::call_site())
                        ),
                        ::proc_macro::TokenTree::from(
                            ::proc_macro::Punct::new(':', ::proc_macro::Spacing::Joint)
                        ),
                        ::proc_macro::TokenTree::from(
                            ::proc_macro::Punct::new(':', ::proc_macro::Spacing::Joint)
                        ),
                        ::proc_macro::TokenTree::from(
                            ::proc_macro::Ident::new("eager", ::proc_macro::Span::call_site())
                        ),
                        ::proc_macro::TokenTree::from(
                            ::proc_macro::Punct::new('!', ::proc_macro::Spacing::Alone)
                        ),
                        ::proc_macro::TokenTree::from(
                            ::proc_macro::Group::new(::proc_macro::Delimiter::Brace, #eager_group)
                        )
                    ]);

                    return #res;
                }

                Into::<::proc_macro::TokenStream>::into(
                    #user_lambda(
                        #input_copy.into::<#user_input_type>()
                    )
                )
            }
        }.into()] as [TokenStream; _]);
    }
}

#[derive(Clone)]
struct Rule {
    grammar: Group,
    expansion: Group,
}

struct HiddenIdent<'a>(bool, &'a Ident);
impl ToTokens for HiddenIdent<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        Punct::new('$', Spacing::Joint).to_tokens(tokens);
        self.1.to_tokens(tokens);
        if self.0 {
            Punct::new(':', Spacing::Joint).to_tokens(tokens);
            Ident::new("tt", Span::call_site()).to_tokens(tokens);
        }
    }
}

struct HiddenIdentSet<'a>(bool, &'a Ident);
impl ToTokens for HiddenIdentSet<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        Punct::new('$', Spacing::Joint).to_tokens(tokens);
        Group::new(
            Delimiter::Parenthesis,
            HiddenIdent(self.0, self.1).into_token_stream(),
        )
        .to_tokens(tokens);
        Punct::new('*', Spacing::Alone).to_tokens(tokens);
    }
}

fn eager_rule(
    grammar: bool,
    eager_call_sigil: &Literal,
    hidden_ident: &Ident,
    tail: TokenStream,
) -> TokenStream {
    eager_data(
        eager_call_sigil,
        HiddenIdentSet(grammar, hidden_ident).into_token_stream(),
        tail,
    )
}

impl Rule {
    fn make_eager(
        mut self,
        crate_path: &TokenStream,
        eager_call_sigil: &Literal,
        hidden_ident: &Ident,
    ) -> Self {
        self.grammar = Group::new(
            self.grammar.delimiter(),
            eager_rule(true, eager_call_sigil, hidden_ident, self.grammar.stream()),
        );

        let mut expansion = TokenStream::new();
        expansion.extend(crate_path.clone());
        Punct::new(':', Spacing::Joint).to_tokens(&mut expansion);
        Punct::new(':', Spacing::Joint).to_tokens(&mut expansion);
        Ident::new("eager", Span::call_site()).to_tokens(&mut expansion);
        Punct::new('!', Spacing::Alone).to_tokens(&mut expansion);
        Group::new(
            Delimiter::Brace,
            eager_rule(
                false,
                eager_call_sigil,
                hidden_ident,
                self.expansion.stream(),
            ),
        )
        .to_tokens(&mut expansion);

        self.expansion = Group::new(self.expansion.delimiter(), expansion);
        self
    }
}

impl ToTokens for Rule {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.grammar.to_tokens(tokens);
        Punct::new('=', Spacing::Joint).to_tokens(tokens);
        Punct::new('>', Spacing::Alone).to_tokens(tokens);
        self.expansion.to_tokens(tokens);
    }
}
pub struct Rules {
    kind: MacroKind,
    metas: Vec<TokenTree>,
    macro_name: Ident,
    eager: Vec<Rule>,
    pure: Vec<Rule>,
}
impl ToTokens for Rules {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.metas.iter().cloned());

        let separator = match &self.kind {
            MacroKind::DeclMacro1 => {
                Ident::new("macro_rules", Span::call_site()).to_tokens(tokens);
                Punct::new('!', Spacing::Alone).to_tokens(tokens);
                self.macro_name.to_tokens(tokens);
                Punct::new(';', Spacing::Alone)
            }
            MacroKind::DeclMacro2 { visibility } => {
                tokens.extend(visibility.clone());
                Ident::new("macro", Span::call_site()).to_tokens(tokens);
                self.macro_name.to_tokens(tokens);
                Punct::new(',', Spacing::Alone)
            }
        };

        let mut rules = TokenStream::new();
        for rule in &self.eager {
            rule.to_tokens(&mut rules);
            separator.to_tokens(&mut rules);
        }
        // Put the pure version after so eager is always tried first
        for rule in &self.pure {
            rule.to_tokens(&mut rules);
            separator.to_tokens(&mut rules);
        }
        Group::new(Delimiter::Brace, rules).to_tokens(tokens);
    }
}

#[cfg(feature = "proc-macro-support")]
fn expand_proc_macro(
    span: Span2,
    eager_call_sigil: &TokenStream2,
    stream: &mut Peekable<&mut dyn Iterator<Item = TokenTree2>>,
) -> Result<EagerProcMacro, Error> {
    let mut attributes = vec![];
    let mut visibility = None;
    loop {
        match stream.peek().ok_or_else(|| Error {
                    span: span.unwrap(),
                    msg: "unexpected end of input".into(),
                    note: Some(crate::Note {
                        span: None,
                        msg: "while trying to match start of proc-macro definition".into(),
                    }),
                })?
        {
            TokenTree2::Punct(p) if p.as_char() == '#' => {
                let pound = stream.next().unwrap();
                attributes.push(pound);

                let g = stream.next_or(span).map_err(|sp| Error {
                            span: sp.unwrap(),
                            msg: "unexpected end of input".into(),
                            note: Some(crate::Note {
                                span: None,
                                msg: "while trying to match proc-macro attribute".into(),
                            }),
                        })?;
                attributes.push(g.into());
            },
            TokenTree2::Ident(i) if i.to_string() == "pub" => {
                visibility = Some(TokenStream2::new());
                unsafe {visibility.as_mut().unwrap_unchecked().extend([stream.next().unwrap_unchecked()]);}

                match stream.peek()
                {
                    Some(TokenTree2::Group(g)) if g.delimiter() == Delimiter2::Parenthesis => {
                        unsafe {visibility.as_mut().unwrap_unchecked().extend([stream.next().unwrap_unchecked()]);}
                    },
                    _ => continue
                }
            },
            TokenTree2::Ident(i) if i.to_string() == "fn" => {
                let _ = stream.next();
                break;
            },
            _ => {}
        }
    }

    let name = stream.next().ok_or_else(|| Error {
                    span: span.unwrap(),
                    msg: "unexpected end of input".into(),
                    note: Some(crate::Note {
                        span: None,
                        msg: "while trying to match start of proc-macro definition".into(),
                    }),
                }).and_then(|tt| match tt
                {
                    TokenTree2::Ident(i) => Ok(i),
                    other => Err(
                        Error {
                    span: other.span().unwrap(),
                    msg: "unexpected token".into(),
                    note: Some(crate::Note {
                        span: None,
                        msg: "while trying to match name of proc-macro".into(),
                    }),
                }
                    )
                })?;
    todo!()
}
pub fn expand_rules(
    span: Span,
    hidden_ident: &Ident,
    crate_path: &TokenStream,
    eager_call_sigil: &Literal,
    stream: &mut Peekable<&mut dyn Iterator<Item = TokenTree>>,
) -> Result<Rules, Error> {
    let mut metas = vec![];
    let mut kind = MacroKind::DeclMacro1;
    loop {
        match stream.peek() {
            None => {
                return Err(Error {
                    span,
                    msg: "unexpected end of input".into(),
                    note: Some(crate::Note {
                        span: None,
                        msg: "while trying to match start of macro definition".into(),
                    }),
                })
            }
            Some(TokenTree::Punct(p)) if p.as_char() == '#' => {
                let pound = stream.next().unwrap();
                metas.push(pound);

                let g = expect_group(stream.next_or(span), Delimiter::Bracket)?;
                metas.push(g.into());
            }
            Some(TokenTree::Ident(i)) if i.to_string() == "macro_rules" => match &kind {
                MacroKind::DeclMacro1 => break,
                MacroKind::DeclMacro2 { visibility } => {
                    return Err(Error {
                        span: i.span(),
                        msg: "expected ident `macro`".into(),
                        note: Some(crate::Note {
                            span: visibility.clone().into_iter().next().map(|tt| tt.span()),
                            msg: "previously matched a visibility specifier".into(),
                        }),
                    })
                }
            },
            Some(TokenTree::Ident(i)) if i.to_string() == "macro" => {
                if kind.is_v1() {
                    kind = MacroKind::DeclMacro2 {
                        visibility: TokenStream::new(),
                    };
                }
                break;
            }
            Some(TokenTree::Ident(i)) if i.to_string() == "pub" => {
                kind = MacroKind::DeclMacro2 {
                    visibility: TokenStream::new(),
                };
                let visibility = kind.vis_mut().unwrap();

                let p = stream.next().unwrap();
                visibility.extend([p].iter().cloned());

                match stream.peek() {
                    Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => {
                        // TODO: verify that we got valid tokens, e.g. `crate`, `super` or `in <path>`
                        let vis_cont = stream.next().unwrap();
                        visibility.extend([vis_cont].iter().cloned());
                    }
                    Some(TokenTree::Ident(i)) if i.to_string() == "macro" => {
                        continue;
                    }
                    Some(tt) => {
                        return Err(Error {
                            span: tt.span(),
                            msg:
                                "expected visibility specifier (e.g. `pub(crate)`) or ident `macro`"
                                    .into(),
                            note: None,
                        });
                    }
                    None => {
                        return Err(Error {
                            span,
                            msg: "unexpected end of input".into(),
                            note: Some(crate::Note {
                                span: None,
                                msg: "while trying to match start of macro definition".into(),
                            }),
                        })
                    }
                };
            }
            Some(t) => return Err(Error {
                span: t.span(),
                msg:
                    "expected token `#`, ident `macro_rules`, visibility specifier or ident `macro`"
                        .into(),
                note: None,
            }),
        }
    }
    let _macro_rules_or_macro = stream.next().unwrap();
    if kind.is_v1() {
        expect_punct(stream.next_or(span), '!')?;
    }
    let macro_name = expect_ident(stream.next_or(span), Param::Named("macro_name"))?;

    let delim;
    if kind.is_v2() {
        delim = ',';
        if stream.peek().is_some_and(
            |tt| matches!(tt, TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis),
        ) {
            let grammar = expect_group(
                stream.next().ok_or_else(|| unreachable!()),
                Param::Named("grammar"),
            )?;

            let expansion = expect_group(stream.next_or(span), Param::Named("expansion"))?;

            let rule = Rule { grammar, expansion };

            return Ok(Rules {
                kind,
                metas,
                macro_name,
                eager: [rule
                    .clone()
                    .make_eager(crate_path, eager_call_sigil, hidden_ident)]
                .to_vec(),
                pure: [rule].to_vec(),
            });
        }
    } else {
        delim = ';';
    }

    let group = expect_group(stream.next_or(span), Delimiter::Brace)?;

    let mut rules = vec![];
    let span = group.span();
    let mut stream = group.stream().into_iter();
    loop {
        let Some(tt) = stream.next() else { break };
        let grammar = expect_group(Ok(tt), Param::Named("grammar"))?;

        // Arrow
        expect_punct(stream.next_or(span), ('=', Spacing::Joint))?;
        expect_punct(stream.next_or(span), '>')?;

        let expansion = expect_group(stream.next_or(span), Param::Named("expansion"))?;

        rules.push(Rule { grammar, expansion });

        let Some(tt) = stream.next() else { break };
        expect_punct(Ok(tt), delim)?;
    }

    let eager_rules = rules
        .iter()
        .cloned()
        .map(|r| r.make_eager(crate_path, eager_call_sigil, hidden_ident))
        .collect();
    let pure_rules = rules;

    Ok(Rules {
        kind,
        metas,
        macro_name,
        eager: eager_rules,
        pure: pure_rules,
    })
}

pub fn expand_rules_legacy(
    span: Span,
    eager_call_sigil: &Literal,
    stream: &mut dyn Iterator<Item = TokenTree>,
) -> Result<TokenStream, Error> {
    let crate_path = crate_path();

    let mut stream = stream.peekable();

    let hidden_ident = match stream.peek() {
        None => return Ok(TokenStream::new()),
        Some(TokenTree::Punct(p)) if p.as_char() == '$' => {
            let _dollar = stream.next();
            expect_ident(stream.next_or(span), Param::Named("eager_ident"))?
        }
        Some(_token) => get_eager_2_ident(),
    };

    let mut output = TokenStream::new();
    while stream.peek().is_some() {
        let rules = expand_rules(
            span,
            &hidden_ident,
            &crate_path,
            eager_call_sigil,
            &mut stream,
        )?;

        rules.to_tokens(&mut output);
    }
    Ok(output)
}

pub fn eager_macro_rules(stream: TokenStream) -> Result<TokenStream, Error> {
    let eager_call_sigil = eager_call_sigil();

    // As with all eager-macros, first try the sigil.
    // If we find one, then the caller was eager, so we can just
    // pick up where they left off
    let state = State::decode_from_stream(stream.clone(), false, |mut v| {
        Ok(expand_rules_legacy(Span::call_site(), &eager_call_sigil, &mut v)?.into_iter())
    })?;

    let output = match state.map(|s| s.process(&eager_call_sigil)) {
        Some(Ok(processed)) => processed.into_token_stream(),
        Some(Err(err)) => return Err(err),
        None => expand_rules_legacy(
            Span::call_site(),
            &eager_call_sigil,
            &mut stream.into_iter(),
        )?,
    };

    Ok(output)
}

pub fn eager_macro(attr: TokenStream, stream: TokenStream) -> Result<TokenStream, Error> {
    let eager_call_sigil = eager_call_sigil();
    let crate_path = crate_path();
    let span = Span::call_site();

    let hidden_ident = {
        let mut stream = attr.into_iter();
        let hidden_ident = match stream.next() {
            Some(attr) => expect_ident(Ok(attr), Param::Named("eager_ident"))?,
            None => get_eager_2_ident(),
        };
        if stream.next().is_some() {
            return Err(Error {
                span,
                msg: "`eager_macro` only takes 1 input".into(),
                note: None,
            });
        }
        hidden_ident
    };
    let stream: &mut dyn Iterator<Item = TokenTree> = &mut stream.into_iter();

    expand_rules(
        span,
        &hidden_ident,
        &crate_path,
        &eager_call_sigil,
        &mut stream.peekable(),
    )
    .map(ToTokens::into_token_stream)
}

#[cfg(feature = "proc-macro-support")]
pub fn eager_proc_macro(_attr: TokenStream2, stream: TokenStream2) -> Result<TokenStream, Error> {
    let eager_call_sigil = eager_call_sigil_proc_macro();
    let span = Span2::call_site();

    let stream: &mut dyn Iterator<Item = TokenTree2> = &mut stream.into_iter();

    expand_proc_macro(
        span,
        &eager_call_sigil,
        &mut stream.peekable(),
    )
    .map(ToTokens::into_token_stream)
}
