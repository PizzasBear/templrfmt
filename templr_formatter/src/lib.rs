#![allow(
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::derive_partial_eq_without_eq,
    clippy::doc_markdown,
    clippy::enum_glob_use,
    clippy::items_after_statements,
    clippy::let_underscore_untyped,
    clippy::match_like_matches_macro,
    clippy::match_same_arms,
    clippy::module_name_repetitions,
    clippy::must_use_candidate,
    clippy::needless_pass_by_value,
    clippy::similar_names,
    clippy::too_many_lines,
    clippy::unused_self,
    clippy::vec_init_then_push
)]
#![cfg_attr(all(test, exhaustive), feature(non_exhaustive_omitted_patterns_lint))]

mod algorithm;
mod attr;
mod comments;
mod convenience;
mod data;
mod expr;
mod file;
mod generics;
mod item;
mod iter;
mod lifetime;
mod lit;
mod mac;
mod pat;
mod path;
mod ring;
mod stmt;
mod templ_mac;
mod token;
mod ty;

use crate::algorithm::Printer;
use comments::BeginSpan;
use syn::{visit::Visit, File, Macro};

// Target line width.
const MARGIN: isize = 89;

// Number of spaces increment at each level of block indentation.
const INDENT: isize = 4;

// Every line is allowed at least this much space, even if highly indented.
const MIN_SPACE: isize = 60;

pub fn unparse(file: &File) -> String {
    let mut p = Printer::new(file.begin_span());
    p.file(file);
    p.eof()
}

pub fn unparse_templ(mac: &Macro, init_indent: usize) -> Option<String> {
    let mut p = Printer::new(mac.begin_span());
    p.cbox(init_indent as _);
    match p.templ_macro(mac, false) {
        true => {
            p.end();
            Some(p.eof())
        }
        false => None,
    }
}

pub fn unparse_templ_macros(source: &str) -> syn::Result<String> {
    struct Visitor<'a> {
        macros: Vec<(&'a Macro, String)>,
        source: &'a str,
        lines: &'a [usize],
    }

    impl<'a> Visit<'a> for Visitor<'a> {
        fn visit_macro(&mut self, mac: &'a Macro) {
            let start = mac.begin_span().unwrap().start();
            let line_idx = self.lines[start.line - 1];
            let indent = self.source[line_idx..]
                .chars()
                .map_while(|ch| match ch {
                    ' ' => Some(1),
                    '\t' => Some(4),
                    _ => None,
                })
                .sum();

            if let Some(fmt_s) = unparse_templ(mac, indent) {
                self.macros.push((mac, fmt_s));
            }
        }
    }

    let lines: Vec<_> = std::iter::once(0)
        .chain(source.bytes().enumerate().filter_map(|(i, ch)| match ch {
            b'\n' => Some(i + 1),
            _ => None,
        }))
        .collect();

    let file = syn::parse_file(source)?;
    let mut v = Visitor {
        macros: vec![],
        source,
        lines: &lines,
    };
    v.visit_file(&file);

    let mut output = String::new();

    let loc_to_idx = |loc: proc_macro2::LineColumn| {
        let line_idx = lines[loc.line - 1];
        line_idx
            + source[line_idx..]
                .char_indices()
                .map(|(i, _)| i)
                .nth(loc.column)
                .unwrap()
    };

    let mut i = 0;
    for (mac, fmt) in &v.macros {
        let start = loc_to_idx(mac.begin_span().unwrap().start());
        output.push_str(&source[i..start]);
        output.push_str(&fmt);

        i = loc_to_idx(mac.delimiter.span().close().end());
    }
    output.push_str(&source[i..]);

    Ok(output)
}
