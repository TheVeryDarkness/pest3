#[allow(non_upper_case_globals)]
const _PEST_GRAMMAR_Parser: [&'static ::core::primitive::str; 1usize] = [include_str!(
    "/Users/huangboyi/Projects/pest3/generator/tests/minimal.pest"
)];
#[allow(
    dead_code,
    missing_docs,
    non_camel_case_types,
    clippy::upper_case_acronyms
)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Rule {
    EOI,
    r#w,
    r#x0,
    r#x1,
    r#x2,
    r#x3,
    r#x4,
    r#minimal(rules::r#minimal::Rule),
}
impl ::pest3::typed::SuperRule<rules::r#minimal::Rule> for Rule {
    fn cvt_from(rule: rules::r#minimal::Rule) -> Self {
        Self::r#minimal(rule)
    }
}
impl ::pest3::typed::RuleType for Rule {
    const EOI: Self = Rule::EOI;
}
#[doc = "Definitions of statically typed nodes generated by pest-generator."]
pub mod rules {
    pub type __OptionalTrivia<'i> = ::pest3::typed::template::Empty;
    pub type __MandatoryTrivia<'i> = ::pest3::typed::template::Empty;
    pub mod minimal {
        #[doc = "A minimal grammar file."]
        #[allow(
            dead_code,
            missing_docs,
            non_camel_case_types,
            clippy::upper_case_acronyms
        )]
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Rule {
            EOI,
            #[doc = "A rule that matches a string \"x\"."]
            r#x,
        }
        impl ::pest3::typed::RuleType for Rule {
            const EOI: Self = Rule::EOI;
        }
        #[doc = "Definitions of statically typed nodes generated by pest-generator."]
        pub mod rules {
            pub type __OptionalTrivia<'i> = ::pest3::typed::template::Empty;
            pub type __MandatoryTrivia<'i> = ::pest3::typed::template::Empty;
            #[doc = "Generated for rule `x`. Grammar: `\"x\"`."]
            #[derive(Clone, Debug, Eq, PartialEq)]
            #[allow(non_camel_case_types)]
            pub struct r#x<'i> {
                #[doc = r" Matched structure."]
                pub content: ::pest3::std::Box<
                    super::super::super::generics::Str<super::super::super::wrapper::W0>,
                >,
                #[doc = r" Matched span."]
                pub span: ::pest3::Span<'i>,
            }
            #[allow(non_camel_case_types)]
            impl<'i> ::pest3::typed::wrapper::Rule for r#x<'i> {
                type Rule = super::Rule;
                const RULE: super::Rule = super::Rule::r#x;
            }
            #[allow(non_camel_case_types)]
            impl<'i> r#x<'i> {}
            #[allow(unused_imports)]
            use pest3::typed::SubRule as _;
            ::pest3::full_rule_struct!(
                r#x,
                (),
                super::Rule,
                super::Rule::r#x,
                super::super::super::generics::Str::<super::super::super::wrapper::W0>,
                ::pest3::std::Box<
                    super::super::super::generics::Str::<super::super::super::wrapper::W0>,
                >,
            );
            #[allow(unused_imports)]
            use pest3::typed::SubRule as _;
            ::pest3::full_rule_struct!(
                r#x,
                (),
                super::super::super::Rule,
                super::Rule::r#x.cvt_into(),
                super::super::super::generics::Str::<super::super::super::wrapper::W0>,
                ::pest3::std::Box<
                    super::super::super::generics::Str::<super::super::super::wrapper::W0>,
                >,
            );
            #[allow(non_camel_case_types)]
            impl<'i> ::pest3::typed::PairContainer<super::Rule> for r#x<'i> {
                fn for_each_child_pair(
                    &self,
                    f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
                ) {
                    self.content.for_self_or_for_each_child_pair(f)
                }
                fn for_self_or_for_each_child_pair(
                    &self,
                    f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
                ) {
                    use pest3::typed::PairTree;
                    f(self.as_pair_tree())
                }
            }
            #[allow(non_camel_case_types)]
            impl<'i> ::pest3::typed::PairTree<super::Rule> for r#x<'i> {
                fn get_rule() -> super::Rule {
                    #[allow(unused_imports)]
                    use pest3::typed::SubRule as _;
                    super::Rule::r#x
                }
                fn get_span(&self) -> (::pest3::std::usize, ::pest3::std::usize) {
                    (self.span.start(), self.span.end())
                }
            }
            #[allow(non_camel_case_types)]
            impl<'i> ::pest3::typed::PairContainer<super::super::super::Rule> for r#x<'i> {
                fn for_each_child_pair(
                    &self,
                    f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::super::super::Rule>),
                ) {
                    self.content.for_self_or_for_each_child_pair(f)
                }
                fn for_self_or_for_each_child_pair(
                    &self,
                    f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::super::super::Rule>),
                ) {
                    use pest3::typed::PairTree;
                    f(self.as_pair_tree())
                }
            }
            #[allow(non_camel_case_types)]
            impl<'i> ::pest3::typed::PairTree<super::super::super::Rule> for r#x<'i> {
                fn get_rule() -> super::super::super::Rule {
                    #[allow(unused_imports)]
                    use pest3::typed::SubRule as _;
                    super::Rule::r#x.cvt_into()
                }
                fn get_span(&self) -> (::pest3::std::usize, ::pest3::std::usize) {
                    (self.span.start(), self.span.end())
                }
            }
        }
    }
    impl ::pest3::typed::SubRule for minimal::Rule {
        type Super = super::Rule;
        fn cvt_into(self) -> Self::Super {
            super::Rule::minimal(self)
        }
    }
    #[doc = "Generated for rule `w`. Grammar: `\"w\"`."]
    #[derive(Clone, Debug, Eq, PartialEq)]
    #[allow(non_camel_case_types)]
    pub struct r#w<'i> {
        #[doc = r" Matched structure."]
        pub content: ::pest3::std::Box<super::generics::Str<super::wrapper::W1>>,
        #[doc = r" Matched span."]
        pub span: ::pest3::Span<'i>,
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::wrapper::Rule for r#w<'i> {
        type Rule = super::Rule;
        const RULE: super::Rule = super::Rule::r#w;
    }
    #[allow(non_camel_case_types)]
    impl<'i> r#w<'i> {}
    #[allow(unused_imports)]
    use pest3::typed::SubRule as _;
    ::pest3::full_rule_struct!(
        r#w,
        (),
        super::Rule,
        super::Rule::r#w,
        super::generics::Str::<super::wrapper::W1>,
        ::pest3::std::Box<super::generics::Str::<super::wrapper::W1>>,
    );
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairContainer<super::Rule> for r#w<'i> {
        fn for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            self.content.for_self_or_for_each_child_pair(f)
        }
        fn for_self_or_for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            use pest3::typed::PairTree;
            f(self.as_pair_tree())
        }
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairTree<super::Rule> for r#w<'i> {
        fn get_rule() -> super::Rule {
            #[allow(unused_imports)]
            use pest3::typed::SubRule as _;
            super::Rule::r#w
        }
        fn get_span(&self) -> (::pest3::std::usize, ::pest3::std::usize) {
            (self.span.start(), self.span.end())
        }
    }
    #[doc = "Generated for rule `x0`. Grammar: `minimal::x`."]
    #[derive(Clone, Debug, Eq, PartialEq)]
    #[allow(non_camel_case_types)]
    pub struct r#x0<'i> {
        #[doc = r" Matched structure."]
        pub content: ::pest3::std::Box<minimal::rules::r#x<'i>>,
        #[doc = r" Matched span."]
        pub span: ::pest3::Span<'i>,
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::wrapper::Rule for r#x0<'i> {
        type Rule = super::Rule;
        const RULE: super::Rule = super::Rule::r#x0;
    }
    #[allow(non_camel_case_types)]
    impl<'i> r#x0<'i> {
        #[doc = "A helper function to access [`x`]."]
        #[allow(non_snake_case)]
        pub fn r#x<'s>(&'s self) -> &'s minimal::rules::r#x<'i> {
            let res = &*self.content;
            res
        }
    }
    #[allow(unused_imports)]
    use pest3::typed::SubRule as _;
    ::pest3::full_rule_struct!(
        r#x0,
        (),
        super::Rule,
        super::Rule::r#x0,
        minimal::rules::r#x::<'i>,
        ::pest3::std::Box<minimal::rules::r#x::<'i>>,
    );
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairContainer<super::Rule> for r#x0<'i> {
        fn for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            self.content.for_self_or_for_each_child_pair(f)
        }
        fn for_self_or_for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            use pest3::typed::PairTree;
            f(self.as_pair_tree())
        }
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairTree<super::Rule> for r#x0<'i> {
        fn get_rule() -> super::Rule {
            #[allow(unused_imports)]
            use pest3::typed::SubRule as _;
            super::Rule::r#x0
        }
        fn get_span(&self) -> (::pest3::std::usize, ::pest3::std::usize) {
            (self.span.start(), self.span.end())
        }
    }
    #[doc = "Generated for rule `x1`. Grammar: `(minimal::x - w)`."]
    #[derive(Clone, Debug, Eq, PartialEq)]
    #[allow(non_camel_case_types)]
    pub struct r#x1<'i> {
        #[doc = r" Matched structure."]
        pub content: ::pest3::std::Box<
            super::generics::Sequence2<
                minimal::rules::r#x<'i>,
                ::pest3::typed::template::Empty,
                r#w<'i>,
                ::pest3::typed::template::Empty,
            >,
        >,
        #[doc = r" Matched span."]
        pub span: ::pest3::Span<'i>,
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::wrapper::Rule for r#x1<'i> {
        type Rule = super::Rule;
        const RULE: super::Rule = super::Rule::r#x1;
    }
    #[allow(non_camel_case_types)]
    impl<'i> r#x1<'i> {
        #[doc = "A helper function to access [`w`]."]
        #[allow(non_snake_case)]
        pub fn r#w<'s>(&'s self) -> &'s r#w<'i> {
            let res = &*self.content;
            {
                let res = &res.field_1;
                res
            }
        }
        #[doc = "A helper function to access [`x`]."]
        #[allow(non_snake_case)]
        pub fn r#x<'s>(&'s self) -> &'s minimal::rules::r#x<'i> {
            let res = &*self.content;
            {
                let res = &res.field_0;
                res
            }
        }
    }
    #[allow(unused_imports)]
    use pest3::typed::SubRule as _;
    ::pest3::full_rule_struct!(
        r#x1,
        (),
        super::Rule,
        super::Rule::r#x1,
        super::generics::Sequence2::<
            minimal::rules::r#x::<'i>,
            ::pest3::typed::template::Empty,
            r#w::<'i>,
            ::pest3::typed::template::Empty,
        >,
        ::pest3::std::Box<
            super::generics::Sequence2::<
                minimal::rules::r#x::<'i>,
                ::pest3::typed::template::Empty,
                r#w::<'i>,
                ::pest3::typed::template::Empty,
            >,
        >,
    );
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairContainer<super::Rule> for r#x1<'i> {
        fn for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            self.content.for_self_or_for_each_child_pair(f)
        }
        fn for_self_or_for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            use pest3::typed::PairTree;
            f(self.as_pair_tree())
        }
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairTree<super::Rule> for r#x1<'i> {
        fn get_rule() -> super::Rule {
            #[allow(unused_imports)]
            use pest3::typed::SubRule as _;
            super::Rule::r#x1
        }
        fn get_span(&self) -> (::pest3::std::usize, ::pest3::std::usize) {
            (self.span.start(), self.span.end())
        }
    }
    #[doc = "Generated for rule `x2`. Grammar: `(minimal::x - minimal::x)`."]
    #[derive(Clone, Debug, Eq, PartialEq)]
    #[allow(non_camel_case_types)]
    pub struct r#x2<'i> {
        #[doc = r" Matched structure."]
        pub content: ::pest3::std::Box<
            super::generics::Sequence2<
                minimal::rules::r#x<'i>,
                ::pest3::typed::template::Empty,
                minimal::rules::r#x<'i>,
                ::pest3::typed::template::Empty,
            >,
        >,
        #[doc = r" Matched span."]
        pub span: ::pest3::Span<'i>,
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::wrapper::Rule for r#x2<'i> {
        type Rule = super::Rule;
        const RULE: super::Rule = super::Rule::r#x2;
    }
    #[allow(non_camel_case_types)]
    impl<'i> r#x2<'i> {
        #[doc = "A helper function to access [`x`]."]
        #[allow(non_snake_case)]
        pub fn r#x<'s>(&'s self) -> (&'s minimal::rules::r#x<'i>, &'s minimal::rules::r#x<'i>) {
            let res = &*self.content;
            {
                let res = (
                    {
                        let res = &res.field_0;
                        res
                    },
                    {
                        let res = &res.field_1;
                        res
                    },
                );
                res
            }
        }
    }
    #[allow(unused_imports)]
    use pest3::typed::SubRule as _;
    ::pest3::full_rule_struct!(
        r#x2,
        (),
        super::Rule,
        super::Rule::r#x2,
        super::generics::Sequence2::<
            minimal::rules::r#x::<'i>,
            ::pest3::typed::template::Empty,
            minimal::rules::r#x::<'i>,
            ::pest3::typed::template::Empty,
        >,
        ::pest3::std::Box<
            super::generics::Sequence2::<
                minimal::rules::r#x::<'i>,
                ::pest3::typed::template::Empty,
                minimal::rules::r#x::<'i>,
                ::pest3::typed::template::Empty,
            >,
        >,
    );
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairContainer<super::Rule> for r#x2<'i> {
        fn for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            self.content.for_self_or_for_each_child_pair(f)
        }
        fn for_self_or_for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            use pest3::typed::PairTree;
            f(self.as_pair_tree())
        }
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairTree<super::Rule> for r#x2<'i> {
        fn get_rule() -> super::Rule {
            #[allow(unused_imports)]
            use pest3::typed::SubRule as _;
            super::Rule::r#x2
        }
        fn get_span(&self) -> (::pest3::std::usize, ::pest3::std::usize) {
            (self.span.start(), self.span.end())
        }
    }
    #[doc = "Generated for rule `x3`. Grammar: `(minimal::x - \"y\")`."]
    #[derive(Clone, Debug, Eq, PartialEq)]
    #[allow(non_camel_case_types)]
    pub struct r#x3<'i> {
        #[doc = r" Matched structure."]
        pub content: ::pest3::std::Box<
            super::generics::Sequence2<
                minimal::rules::r#x<'i>,
                ::pest3::typed::template::Empty,
                super::generics::Str<super::wrapper::W2>,
                ::pest3::typed::template::Empty,
            >,
        >,
        #[doc = r" Matched span."]
        pub span: ::pest3::Span<'i>,
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::wrapper::Rule for r#x3<'i> {
        type Rule = super::Rule;
        const RULE: super::Rule = super::Rule::r#x3;
    }
    #[allow(non_camel_case_types)]
    impl<'i> r#x3<'i> {
        #[doc = "A helper function to access [`x`]."]
        #[allow(non_snake_case)]
        pub fn r#x<'s>(&'s self) -> &'s minimal::rules::r#x<'i> {
            let res = &*self.content;
            {
                let res = &res.field_0;
                res
            }
        }
    }
    #[allow(unused_imports)]
    use pest3::typed::SubRule as _;
    ::pest3::full_rule_struct!(
        r#x3,
        (),
        super::Rule,
        super::Rule::r#x3,
        super::generics::Sequence2::<
            minimal::rules::r#x::<'i>,
            ::pest3::typed::template::Empty,
            super::generics::Str::<super::wrapper::W2>,
            ::pest3::typed::template::Empty,
        >,
        ::pest3::std::Box<
            super::generics::Sequence2::<
                minimal::rules::r#x::<'i>,
                ::pest3::typed::template::Empty,
                super::generics::Str::<super::wrapper::W2>,
                ::pest3::typed::template::Empty,
            >,
        >,
    );
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairContainer<super::Rule> for r#x3<'i> {
        fn for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            self.content.for_self_or_for_each_child_pair(f)
        }
        fn for_self_or_for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            use pest3::typed::PairTree;
            f(self.as_pair_tree())
        }
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairTree<super::Rule> for r#x3<'i> {
        fn get_rule() -> super::Rule {
            #[allow(unused_imports)]
            use pest3::typed::SubRule as _;
            super::Rule::r#x3
        }
        fn get_span(&self) -> (::pest3::std::usize, ::pest3::std::usize) {
            (self.span.start(), self.span.end())
        }
    }
    #[doc = "Generated for rule `x4`. Grammar: `(minimal::x | \"z\")`."]
    #[derive(Clone, Debug, Eq, PartialEq)]
    #[allow(non_camel_case_types)]
    pub struct r#x4<'i> {
        #[doc = r" Matched structure."]
        pub content: ::pest3::std::Box<
            super::generics::Choice2<
                minimal::rules::r#x<'i>,
                super::generics::Str<super::wrapper::W3>,
            >,
        >,
        #[doc = r" Matched span."]
        pub span: ::pest3::Span<'i>,
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::wrapper::Rule for r#x4<'i> {
        type Rule = super::Rule;
        const RULE: super::Rule = super::Rule::r#x4;
    }
    #[allow(non_camel_case_types)]
    impl<'i> r#x4<'i> {
        #[doc = "A helper function to access [`x`]."]
        #[allow(non_snake_case)]
        pub fn r#x<'s>(&'s self) -> ::pest3::std::Option<&'s minimal::rules::r#x<'i>> {
            let res = &*self.content;
            {
                let res = res.choice_0().map(|res| res);
                res
            }
        }
    }
    #[allow(unused_imports)]
    use pest3::typed::SubRule as _;
    ::pest3::full_rule_struct!(
        r#x4,
        (),
        super::Rule,
        super::Rule::r#x4,
        super::generics::Choice2::<
            minimal::rules::r#x::<'i>,
            super::generics::Str::<super::wrapper::W3>,
        >,
        ::pest3::std::Box<
            super::generics::Choice2::<
                minimal::rules::r#x::<'i>,
                super::generics::Str::<super::wrapper::W3>,
            >,
        >,
    );
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairContainer<super::Rule> for r#x4<'i> {
        fn for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            self.content.for_self_or_for_each_child_pair(f)
        }
        fn for_self_or_for_each_child_pair(
            &self,
            f: &mut impl ::pest3::std::FnMut(::pest3::token::Pair<super::Rule>),
        ) {
            use pest3::typed::PairTree;
            f(self.as_pair_tree())
        }
    }
    #[allow(non_camel_case_types)]
    impl<'i> ::pest3::typed::PairTree<super::Rule> for r#x4<'i> {
        fn get_rule() -> super::Rule {
            #[allow(unused_imports)]
            use pest3::typed::SubRule as _;
            super::Rule::r#x4
        }
        fn get_span(&self) -> (::pest3::std::usize, ::pest3::std::usize) {
            (self.span.start(), self.span.end())
        }
    }
}
mod wrapper {
    #[doc = "A wrapper for `\"x\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W0;
    impl ::pest3::typed::wrapper::String for W0 {
        const CONTENT: &'static ::core::primitive::str = "x";
    }
    #[doc = "A wrapper for `\"w\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W1;
    impl ::pest3::typed::wrapper::String for W1 {
        const CONTENT: &'static ::core::primitive::str = "w";
    }
    #[doc = "A wrapper for `\"y\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W2;
    impl ::pest3::typed::wrapper::String for W2 {
        const CONTENT: &'static ::core::primitive::str = "y";
    }
    #[doc = "A wrapper for `\"z\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W3;
    impl ::pest3::typed::wrapper::String for W3 {
        const CONTENT: &'static ::core::primitive::str = "z";
    }
}
pub use pest3::typed::unicode;
#[doc = "Used generics."]
pub mod generics {
    pub use pest3::choice::Choice2;
    pub use pest3::sequence::Sequence2;
    pub use pest3::typed::template::{
        CharRange, Insens, Negative, PeekSlice1, PeekSlice2, Positive, Rep, RepMax, RepMin,
        RepMinMax, RepOnce, Str, ANY as any, ASCII as ascii, ASCII_ALPHA as ascii_alpha,
        ASCII_ALPHANUMERIC as ascii_alphanumeric, ASCII_ALPHA_LOWER as ascii_alpha_lower,
        ASCII_ALPHA_UPPER as ascii_alpha_upper, ASCII_BIN_DIGIT as ascii_bin_digit,
        ASCII_DIGIT as ascii_digit, ASCII_HEX_DIGIT as ascii_hex_digit,
        ASCII_NONZERO_DIGIT as ascii_nonzero_digit, ASCII_OCT_DIGIT as ascii_oct_digit,
        DROP as drop, EOI, EOI as eoi, NEWLINE as newline, PEEK as peek, PEEK_ALL as peek_all,
        POP as pop, POP_ALL as pop_all, PUSH as push, SOI, SOI as soi,
    };
}
