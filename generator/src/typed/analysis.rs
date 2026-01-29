use super::{
    attr::parse_derive,
    config::Config,
    getter::GetterByName,
    module::{ModuleNode, ModuleSystem},
    output::{generics, Output, Tracker},
};
use crate::{
    common::generate_include,
    config::collect_data,
    typed::getter,
    types::{option_type, pest},
};
use pest3_meta::{
    error::rename_meta_rule,
    parser::{
        self, fmt_sep, GrammarModule, Import, ParseExpr, ParseNode, ParseRule, PathArgs, Range,
        Trivia,
    },
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    path::PathBuf,
    rc::Rc,
};
use syn::{DeriveInput, Generics};

/// `'g` refers to grammar.
fn collect_used_rule_without_trivia_into<'g>(
    rule: &'g ParseRule,
    used: &mut BTreeSet<&'g str>,
) -> (bool, bool) {
    let mut nodes: Vec<&'g ParseNode> = vec![];
    let mut res = (false, false);
    let (used_optional_trivia, used_mandatory_trivia) = &mut res;
    let mut mark = |trivia: &Trivia| match trivia {
        Trivia::Mandatory => {
            *used_mandatory_trivia = true;
        }
        Trivia::Optional => {
            *used_optional_trivia = true;
        }
        Trivia::None => (),
    };
    let mut f = |expr: &'g parser::ParseNode, nodes: &mut Vec<&'g ParseNode>| match &expr.expr {
        ParseExpr::Str(_) | ParseExpr::Insens(_) | ParseExpr::Range(_, _) => (),
        ParseExpr::PosPred(node) | ParseExpr::NegPred(node) => nodes.push(node),
        ParseExpr::Seq(lhs, rhs, trivia) => {
            nodes.push(lhs);
            nodes.push(rhs);
            mark(trivia);
        }
        ParseExpr::Choice(lhs, rhs) => {
            nodes.push(lhs);
            nodes.push(rhs);
        }
        ParseExpr::Opt(node)
        | ParseExpr::Rep(node)
        | ParseExpr::RepOnce(node)
        | ParseExpr::RepRange(node, _) => nodes.push(node),
        ParseExpr::Path(path, args) => {
            if let Some(PathArgs::Call(args)) = args {
                // Normally nodes are linked directly.
                nodes.extend(args)
            }
            // Generics from another module is ignored.
            if path.len() == 1 {
                used.insert(&path[0]);
            }
        }
        ParseExpr::Separated(node, trivia) => {
            nodes.push(node);
            mark(trivia);
        }
    };
    f(&rule.node, &mut nodes);
    while let Some(expr) = nodes.pop() {
        if expr == &rule.node {
            continue;
        }
        f(expr, &mut nodes);
    }
    res
}

/// (Whether optional trivia is used, Whether mandatory trivia is used, Used rules except trivias.)
fn collect_used_rule_without_trivia(rule: &'_ ParseRule) -> (bool, bool, BTreeSet<&'_ str>) {
    let mut used = BTreeSet::new();
    let (opt, man) = collect_used_rule_without_trivia_into(rule, &mut used);
    (opt, man, used)
}

/// `'g` refers to grammar.
fn collect_used_rule<'g>(
    rule: &'g ParseRule,
    optional_trivia: Option<&BTreeSet<&'g str>>,
    mandatory_trivia: Option<&BTreeSet<&'g str>>,
    res: &mut BTreeSet<&'g str>,
) {
    let mut nodes: Vec<&'g ParseNode> = vec![];
    let require_trivia = |trivia: Trivia| {
        let expect_trivia = {
            format!(
                "Please define trivia with `{} = \"...\"`. It's used in rule `{}`.",
                trivia, rule.name,
            )
        };
        match trivia {
            Trivia::Mandatory => {
                mandatory_trivia.expect(&expect_trivia);
            }
            Trivia::Optional => {
                optional_trivia.expect(&expect_trivia);
            }
            Trivia::None => (),
        }
    };
    let (opt, man) = collect_used_rule_without_trivia_into(rule, res);
    if opt {
        require_trivia(Trivia::Optional);
    }
    if man {
        require_trivia(Trivia::Mandatory);
    }
}

fn collect_trivia<'g>(
    opt: Option<&'g ParseRule>,
    man: Option<&'g ParseRule>,
) -> (Option<BTreeSet<&'g str>>, Option<BTreeSet<&'g str>>) {
    let mut optional = opt.map(collect_used_rule_without_trivia);
    let mut mandatory = man.map(collect_used_rule_without_trivia);
    if let (Some(optional), Some(mandatory)) = (&mut optional, &mut mandatory) {
        if optional.1 {
            optional.2.extend(mandatory.2.iter());
        }
        if mandatory.0 {
            mandatory.2.extend(optional.2.iter());
        }
    }
    (optional.map(|o| o.2), mandatory.map(|m| m.2))
}

#[cfg(test)]
pub(super) fn collect_used_rules<'s>(rules: &'s [ParseRule]) -> BTreeSet<&'s str> {
    let mut res = BTreeSet::new();
    let optional_trivia = rules.iter().find(|rule| rule.name == "~");
    let mandatory_trivia = rules.iter().find(|rule| rule.name == "^");

    let (optional, mandatory) = collect_trivia(optional_trivia, mandatory_trivia);

    for rule in rules {
        collect_used_rule(rule, optional.as_ref(), mandatory.as_ref(), &mut res);
    }
    res
}

/// Wrap some nodes in [std::boxed::Box] to avoid infinite size struct,
/// which can break the edges in the reference graph,
/// and then collect reachability.
///
/// Rules that are not in map keys are wrapped.
///
/// We won't promise anything on which nodes are boxed.
pub(super) fn collect_reachability(rules: &[ParseRule]) -> BTreeMap<&str, BTreeSet<&str>> {
    let mut res = BTreeMap::new();
    let optional_trivia = rules.iter().find(|rule| rule.name == "~");
    let mandatory_trivia = rules.iter().find(|rule| rule.name == "^");

    let (optional, mandatory) = collect_trivia(optional_trivia, mandatory_trivia);

    for rule in rules {
        let entry = res.entry(rule.name.as_str()).or_default();
        collect_used_rule(rule, optional.as_ref(), mandatory.as_ref(), entry);
    }
    // Length of any path is no more than `rules.len()`.
    for _ in 0..rules.len() {
        // Before the `i`-th iteration,
        // `res[a]` contains all nodes that can be reached from `a`
        // in no more than `i+1` steps.
        for rule in rules {
            let rule_ref = rule.name.as_str();
            if let Some(cur) = res.remove(&rule_ref) {
                let mut new = cur.clone();
                for referenced in cur {
                    if let Some(iter) = res.get(&referenced) {
                        new.extend(iter.iter().cloned());
                    }
                }
                if !new.contains(&rule_ref) {
                    res.insert(rule_ref, new);
                }
            }
        }
    }
    res
}
