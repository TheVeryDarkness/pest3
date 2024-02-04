use crate::types::_str;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::collections::{BTreeMap, BTreeSet};
use syn::Index;

pub fn pest() -> TokenStream {
    quote! {
        ::pest
    }
}

pub fn generics() -> TokenStream {
    quote! {
        generics
    }
}

pub fn rules_mod() -> TokenStream {
    quote! {rules}
}

pub fn constant_wrappers() -> TokenStream {
    quote! {wrapper}
}

pub(crate) struct Output<'g> {
    content: Vec<TokenStream>,
    wrappers: Vec<TokenStream>,
    wrapper_counter: BTreeMap<&'g str, usize>,
    sequences: BTreeSet<usize>,
    choices: BTreeSet<usize>,
    trivia: Option<TokenStream>,
}
impl<'g> Output<'g> {
    pub fn new() -> Self {
        Self {
            content: Vec::new(),
            wrappers: Vec::new(),
            wrapper_counter: BTreeMap::new(),
            sequences: BTreeSet::new(),
            choices: BTreeSet::new(),
            trivia: None,
        }
    }
    /// Record usage of Seq* generics.
    pub fn record_seq(&mut self, index: usize) {
        self.sequences.insert(index);
    }
    /// Record usage of Choices* generics.
    pub fn record_choice(&mut self, index: usize) {
        self.choices.insert(index);
    }
    /// Used sequences.
    pub fn seq(&self) -> &BTreeSet<usize> {
        &self.sequences
    }
    /// Used choices.
    pub fn choices(&self) -> &BTreeSet<usize> {
        &self.choices
    }
    /// Insert rule struct to rule module.
    pub fn insert_rule_struct(&mut self, tokens: TokenStream) {
        self.content.push(tokens);
    }
    /// Insert a string wrapper to corresponding module.
    /// Return the module path relative to module root.
    pub fn insert_string_wrapper(&mut self, string: &'g str) -> TokenStream {
        let wrapper_mod = constant_wrappers();
        if let Some(index) = self.wrapper_counter.get(string) {
            let s = format_ident!("W{}", index);
            quote! {#wrapper_mod::#s}
        } else {
            let index = self.wrapper_counter.len();
            let s = format_ident!("W{}", index);
            self.wrapper_counter.insert(string, index);
            let doc = format!("A wrapper for `{:?}`.", string);
            let str = _str();
            let this = pest();
            let wrapper = quote! {
                #[doc = #doc]
                #[derive(Clone, Debug, Hash, PartialEq, Eq)]
                pub struct #s;
                impl #this::typed::wrapper::String for #s {
                    const CONTENT: &'static #str = #string;
                }
            };
            self.wrappers.push(wrapper);
            quote! {#wrapper_mod::#s}
        }
    }
    /// Record rule for traivia
    pub fn add_trivia(&mut self, type_name: TokenStream) {
        let previous = self.trivia.replace(type_name);
        assert!(
            previous.is_none(),
            "Trivia can't be defined multiple times."
        );
    }
    /// Collect to final [TokenStream].
    pub fn collect(&self) -> TokenStream {
        let pest = pest();
        let content = &self.content;
        let wrappers = &self.wrappers;
        let wrapper_mod = constant_wrappers();
        let rules = rules_mod();
        let trivia = self
            .trivia
            .clone()
            .unwrap_or(quote! {#pest::typed::template::NONE});
        let generics = {
            let fill = |set: &BTreeSet<usize>,
                        target: &mut Vec<TokenStream>,
                        prefix: &str,
                        mac: &Ident,
                        module: &Ident,
                        seq: bool| {
                for len in set.iter().cloned() {
                    let generics_i = format_ident!("{}{}", prefix, len);
                    let (types, field): (Vec<_>, Vec<_>) = (0..len)
                        .map(|i| {
                            let field = if seq {
                                let i = Index::from(i);
                                let i = format_ident!("field_{}", i);
                                quote! {#i}
                            } else {
                                let i = format_ident!("Choice{}", i);
                                quote! {#i}
                            };
                            (format_ident!("T{}", i), field)
                        })
                        .unzip();
                    // `pest` is already imported, so can be referred directly.
                    if len >= 16 {
                        target.push(quote! {
                            #pest::#mac!(#generics_i, #(#types, #field, )*);
                        });
                    } else {
                        target.push(quote! {
                            pub use #pest::#module::#generics_i;
                        })
                    }
                }
            };
            let mut seq = vec![];
            let mut chs = vec![];
            fill(
                self.seq(),
                &mut seq,
                "Sequence",
                &format_ident!("sequence_type"),
                &format_ident!("sequence"),
                true,
            );
            fill(
                self.choices(),
                &mut chs,
                "Choice",
                &format_ident!("choice_type"),
                &format_ident!("choice"),
                false,
            );

            quote! {
                #[doc = "Used generics."]
                pub mod generics {
                    use #pest::typed::{template, TypedNode};
                    pub use template::{Str, Insens, PeekSlice1, PeekSlice2, Push, CharRange, Positive, Negative, Rep, RepOnce};
                    #(#seq)*
                    #(#chs)*
                }
            }
        };
        quote! {
            impl #pest::typed::RuleType for Rule {
                const EOI: Self = Rule::EOI;
                type Trivia<'i> = #trivia;
            }
            mod #wrapper_mod {
                #(#wrappers)*
            }
            #[doc = "Definitions of statically typed nodes generated by pest-generator."]
            pub mod #rules {
                #(#content)*
            }
            #generics
        }
    }
}
