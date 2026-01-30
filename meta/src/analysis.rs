use crate::parser::{self, GrammarModule, ParseExpr, ParseNode, ParseRule, PathArgs, Trivia};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct SetMap<'g>(BTreeMap<&'g str, BTreeSet<&'g str>>);

/// Analysis results.
pub struct Analyzed<'g> {
    /// The module to analyze.
    #[expect(dead_code)]
    module: &'g GrammarModule,
    /// Used rules.
    #[expect(dead_code)]
    used: SetMap<'g>,
    /// Reachability.
    #[expect(dead_code)]
    raw: SetMap<'g>,
    /// After randomly choosing some rules to be boxed, which rules can be accessed without
    /// dereferencing a [`Box`].
    containment_after_random_boxing: SetMap<'g>,
}

impl Analyzed<'_> {
    /// Returns whether a rule is selected to be boxed.
    pub fn boxed(&self, rule: &str) -> bool {
        !self.containment_after_random_boxing.0.contains_key(rule)
    }
}

/// Collect used rules in `rule`, except trivia.
///
/// `'g` refers to grammar.
///
/// - Whether optional trivia is used
/// - Whether mandatory trivia is used
fn collect_used_rule_except_trivia_into<'g>(
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

/// Collect used rules in `rule`, except trivia.
///
/// # Returns
///
/// - Whether optional trivia is used
/// - Whether mandatory trivia is used
/// - Used rules except trivia.
fn collect_used_rule_except_trivia(rule: &'_ ParseRule) -> (bool, bool, BTreeSet<&'_ str>) {
    let mut used = BTreeSet::new();
    let (opt, man) = collect_used_rule_except_trivia_into(rule, &mut used);
    (opt, man, used)
}

/// Collect all used rules in `rule`.
///
/// `'g` refers to grammar.
fn collect_used_rule<'g>(
    rule: &'g ParseRule,
    optional_trivia: Option<&BTreeSet<&'g str>>,
    mandatory_trivia: Option<&BTreeSet<&'g str>>,
    res: &mut BTreeSet<&'g str>,
) {
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
    let (opt, man) = collect_used_rule_except_trivia_into(rule, res);
    if opt {
        require_trivia(Trivia::Optional);
    }
    if man {
        require_trivia(Trivia::Mandatory);
    }
}

fn collect_used_rules<'g>(module: &'g GrammarModule) -> SetMap<'g> {
    let mut res = BTreeMap::new();
    let optional_trivia = module.rules.iter().find(|rule| rule.name == "~");
    let mandatory_trivia = module.rules.iter().find(|rule| rule.name == "^");
    let (optional, mandatory) = collect_trivia(optional_trivia, mandatory_trivia);
    for rule in &module.rules {
        let entry = res.entry(rule.name.as_str()).or_default();
        collect_used_rule(rule, optional.as_ref(), mandatory.as_ref(), entry);
    }
    SetMap(res)
}

fn collect_trivia<'g>(
    opt: Option<&'g ParseRule>,
    man: Option<&'g ParseRule>,
) -> (Option<BTreeSet<&'g str>>, Option<BTreeSet<&'g str>>) {
    let mut optional = opt.map(collect_used_rule_except_trivia);
    let mut mandatory = man.map(collect_used_rule_except_trivia);
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
fn collect_all_used_rules<'s>(rules: &'s [ParseRule]) -> BTreeSet<&'s str> {
    let mut res = BTreeSet::new();
    let optional_trivia = rules.iter().find(|rule| rule.name == "~");
    let mandatory_trivia = rules.iter().find(|rule| rule.name == "^");

    let (optional, mandatory) = collect_trivia(optional_trivia, mandatory_trivia);

    for rule in rules {
        collect_used_rule(rule, optional.as_ref(), mandatory.as_ref(), &mut res);
    }
    res
}

fn collect_raw_reachability<'g>(rules: &'g [ParseRule], used: &SetMap<'g>) -> SetMap<'g> {
    let SetMap(mut res) = used.clone();

    // Length of any path is no more than `rules.len()`.
    for _ in 0..rules.len() {
        // Before the `i`-th iteration,
        // `res[a]` contains all nodes that can be reached from `a`
        // in no more than `i+1` steps.
        for rule in rules {
            let rule_ref = rule.name.as_str();
            if let Some(cur) = res.remove(&rule_ref) {
                // `cur` contains all nodes that can be reached from `rule_ref`
                // in no more than `i` steps.
                // We remove it to avoid borrowing `res` mutably and immutably at the same time.
                let mut new = cur.clone();
                for referenced in cur {
                    if referenced != rule_ref {
                        if let Some(iter) = res.get(&referenced) {
                            new.extend(iter.iter().cloned());
                        }
                    }
                }
                res.insert(rule_ref, new);
            }
        }
    }
    SetMap(res)
}

/// Wrap some nodes in [std::boxed::Box] to avoid infinite size struct,
/// which can break the edges in the reference graph,
/// and then collect reachability.
///
/// Rules that are not in map keys are wrapped.
///
/// We won't promise anything on which nodes are boxed.
fn collect_boxed_reachability<'g>(rules: &'g [ParseRule], used: &SetMap<'g>) -> SetMap<'g> {
    let SetMap(mut res) = used.clone();

    // Length of any path is no more than `rules.len()`.
    for _ in 0..rules.len() {
        // Before the `i`-th iteration,
        // `res[a]` contains all nodes that can be reached from `a`
        // in no more than `i+1` steps.
        for rule in rules {
            let rule_ref = rule.name.as_str();
            if let Some(cur) = res.remove(&rule_ref) {
                // `cur` contains all nodes that can be reached from `rule_ref`
                // in no more than `i` steps.
                // We remove it to avoid borrowing `res` mutably and immutably at the same time.
                // And only if it will not reach itself in next step, we insert it back.
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
    SetMap(res)
}

pub fn analyze(module: &GrammarModule) -> Analyzed<'_> {
    let used = collect_used_rules(module);
    let raw = collect_raw_reachability(&module.rules, &used);
    let containment_after_random_boxing = collect_boxed_reachability(&module.rules, &used);

    Analyzed {
        module,
        used,
        raw,
        containment_after_random_boxing,
    }
}

#[cfg(test)]
#[allow(unused)]
mod tests {
    use super::*;
    use crate::parser::{self, GrammarModule};
    use std::{
        string::String,
        sync::{Arc, OnceLock},
    };

    static SYNTAX: OnceLock<String> = OnceLock::new();
    static PARSE_RESULT: OnceLock<Arc<GrammarModule>> = OnceLock::new();

    fn get() -> (&'static String, &'static Arc<GrammarModule>) {
        let syntax = SYNTAX.get_or_init(|| {
            String::from_utf8(std::fs::read("tests/syntax.pest").unwrap()).unwrap()
        });
        let parse_result =
            PARSE_RESULT.get_or_init(|| parser::parse(&syntax, &"tests/syntax.pest").unwrap());
        (syntax, parse_result)
    }

    #[test]
    fn inlined_used_rules() {
        let module = parser::parse(
            r#"
x = a - b
a = "a"*
b = "b"+
"#,
            &file!(),
        )
        .unwrap();
        let GrammarModule { rules, .. } = module.as_ref();
        let used = collect_all_used_rules(&rules);
        assert_eq!(used, BTreeSet::from(["a", "b"]));
    }

    #[test]
    /// Check we can actually break the cycles.
    fn inter_reference() {
        let module = parser::parse(
            &r#"
a = "a" - b*
b = "b" - c?
c = a+
"#,
            &file!(),
        )
        .unwrap();
        let GrammarModule { rules, .. } = module.as_ref();
        let used = collect_all_used_rules(&rules);
        assert_eq!(used, BTreeSet::from(["a", "b", "c"]));
        let graph = collect_boxed_reachability(&rules, &collect_used_rules(&module));
        assert_eq!(
            graph,
            SetMap(BTreeMap::from([("b", BTreeSet::from(["a", "c"]))]))
        );
    }
}
