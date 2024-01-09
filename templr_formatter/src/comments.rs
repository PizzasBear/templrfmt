use std::{borrow::Cow, sync::OnceLock};

use proc_macro2::{Ident, Span, TokenTree};
use quote::ToTokens;
use regex::Regex;
use syn::punctuated::Punctuated;

use crate::algorithm::Printer;

impl Printer {
    pub fn flush_comments(
        &mut self,
        span: Span,
        skip_first_newline: bool,
        skip_last_newline: bool,
    ) {
        self.flush_comments_dbg(span, skip_first_newline, skip_last_newline, None);
    }
    pub fn flush_comments_dbg(
        &mut self,
        span: Span,
        skip_first_newline: bool,
        skip_last_newline: bool,
        msg: Option<&str>,
    ) {
        fn find_segment() -> &'static Regex {
            static RG: OnceLock<Regex> = OnceLock::new();
            RG.get_or_init(|| {
                Result::unwrap(Regex::new(
                    r#"(?mx)
                        (?<str>(?:r\#*)?")
                        | (?<newline>\r?\n)
                        | (?<comment>//.*?$)
                        | (?<mlcomment>/\*)
                        | (?<tk>[^\sr/"]+ | /(?<ignore1>[^/*]|$) | r(?<ignore2>[^\#"]|$))
                    "#,
                ))
            })
        }
        let start_span = self.comment_span;
        let Some(mut super_text) = start_span
            .and_then(|start_span| start_span.join(span))
            .and_then(|span| span.source_text())
        else {
            return;
        };
        self.comment_span = Some(span);

        super_text.truncate(super_text.len() - span.source_text().unwrap().len());
        if let Some(msg) = msg {
            eprintln!("{msg}: super_text={super_text:?}");
        }

        let mut comments = vec![];

        let mut i = 0;
        while let Some(capt) = find_segment().captures(&super_text[i..]) {
            if let Some(mch) = capt.name("str") {
                i += mch.end();
                match mch.len() {
                    ..=2 => {
                        fn str_end() -> &'static Regex {
                            static RG: OnceLock<Regex> = OnceLock::new();
                            RG.get_or_init(|| Result::unwrap(Regex::new(r#"[^\\]""#)))
                        }

                        if super_text[i..].starts_with('"') {
                            i += 1;
                        } else {
                            i += str_end().find(&super_text[i..]).unwrap().end();
                        }
                    }
                    3.. => {
                        let hashes = mch.len() - 3;

                        let rg = Result::unwrap(Regex::new(&format!(r##""#{{{hashes}}}"##)));
                        i += rg.find(&super_text[i..]).unwrap().end();
                    }
                }
                comments.push(Cow::Borrowed("tk"));
            } else if let Some(mch) = capt.name("newline") {
                i += mch.end();
                comments.push(Cow::Borrowed("newline"));
            } else if let Some(mch) = capt.name("comment") {
                i += mch.end();
                if !mch.as_str().starts_with("///") {
                    comments.push(mch.as_str().trim().to_owned().into());
                }
            } else if let Some(mch) = capt.name("mlcomment") {
                fn mlcomment_seg() -> &'static Regex {
                    static RG: OnceLock<Regex> = OnceLock::new();
                    RG.get_or_init(|| Result::unwrap(Regex::new(r#"/\*|\*/"#)))
                }

                let start = i + mch.start();
                i += mch.end();
                let mut depth = 1;
                while 0 < depth {
                    let mch = mlcomment_seg().find(&super_text[i..]).unwrap();
                    i += mch.end();
                    match mch.as_str() {
                        "/*" => depth += 1,
                        "*/" => depth -= 1,
                        _ => unreachable!(),
                    }
                }

                if !super_text[start..i].starts_with("/**") {
                    comments.push(super_text[start..i].to_owned().into());
                }
            } else if let Some(mch) = capt.name("tk") {
                i += mch.end();
                i -= capt.name("ignore1").map_or(0, |mch| mch.len());
                i -= capt.name("ignore2").map_or(0, |mch| mch.len());
                comments.push(Cow::Borrowed("tk"));
            } else {
                unreachable!();
            }
        }

        if skip_last_newline && comments.last().is_some_and(|c| c == "newline") {
            comments.truncate(
                comments
                    .iter()
                    .rposition(|c| c != "newline" && c != "tk")
                    .map(|i| i + 2)
                    .unwrap_or(0),
            );
        }

        let last_tk = comments.iter().rposition(|c| c == "tk").unwrap_or(0);
        let mut skip_newline = true;
        let mut newlines = 0;
        let mut consider_newline = false;

        if let Some(msg) = msg {
            eprintln!("{msg}: skip_first_newline={skip_first_newline} skip_last_newline={skip_last_newline} last_tk={last_tk} comments={comments:?}");
        }

        for (i, c) in comments.into_iter().enumerate() {
            match &*c {
                "newline" => {
                    if msg.is_some() {
                        eprintln!(
                            "  newline: i={i} newlines={newlines} skip_newline={skip_newline}"
                        )
                    }
                    if last_tk <= i || consider_newline {
                        newlines += 1;
                        if (1..=2).contains(&newlines) && !skip_newline {
                            self.hardbreak();
                        }
                        skip_newline &= skip_first_newline;
                        consider_newline = false;
                    }
                }
                "tk" => {}
                _ => {
                    self.word(c);
                    skip_newline = false;
                    consider_newline = true;
                    newlines = 0;
                }
            }
        }
        if consider_newline {
            self.space_if_nonempty();
        }
    }
    // #[allow(dead_code)]
    // pub fn preserve_empty_line_between(&mut self, span1: Span, span2: Span) {
    //     let Some(super_text) = span1.join(span2).and_then(|span| span.source_text()) else {
    //         return;
    //     };

    //     let mut loc = span1.start();
    //     let span1_end = span1.end();
    //     let span2_start = span2.start();

    //     let mut text_start = super_text.len();
    //     for (i, ch) in super_text.char_indices() {
    //         if span1_end <= loc {
    //             text_start = i;
    //             break;
    //         }
    //         if ch == '\n' {
    //             loc.line += 1;
    //             loc.column = 0;
    //         } else {
    //             loc.column += 1;
    //         }
    //     }
    //     let mut text_end = super_text.len();
    //     for (i, ch) in super_text[text_start..].char_indices() {
    //         if span2_start <= loc {
    //             text_end = text_start + i;
    //             break;
    //         }
    //         if ch == '\n' {
    //             loc.line += 1;
    //             loc.column = 0;
    //         } else {
    //             loc.column += 1;
    //         }
    //     }
    //     let text = &super_text[text_start..text_end];
    //     if text.trim().is_empty() {
    //         return;
    //     }
    //     if 2 < text.lines().filter(|line| line.trim().is_empty()).count() {
    //         self.hardbreak_if_nonempty();
    //     }
    // }
}

pub fn slow_begin_span(tokenable: &impl ToTokens) -> Option<Span> {
    let tt = tokenable.to_token_stream().into_iter().next()?;
    match tt {
        TokenTree::Group(group) => Some(group.span_open()),
        _ => Some(tt.span()),
    }
}

pub trait BeginSpan {
    fn begin_span(&self) -> Option<Span>;
}

impl BeginSpan for Span {
    fn begin_span(&self) -> Option<Span> {
        Some(*self)
    }
}

impl BeginSpan for Ident {
    fn begin_span(&self) -> Option<Span> {
        Some(self.span())
    }
}

impl BeginSpan for proc_macro2::Group {
    fn begin_span(&self) -> Option<Span> {
        Some(self.span_open())
    }
}

impl BeginSpan for proc_macro2::Punct {
    fn begin_span(&self) -> Option<Span> {
        Some(self.span())
    }
}

impl BeginSpan for proc_macro2::Literal {
    fn begin_span(&self) -> Option<Span> {
        Some(self.span())
    }
}

impl BeginSpan for TokenTree {
    fn begin_span(&self) -> Option<Span> {
        match self {
            TokenTree::Group(group) => group.begin_span(),
            TokenTree::Ident(ident) => ident.begin_span(),
            TokenTree::Punct(punct) => punct.begin_span(),
            TokenTree::Literal(lit) => lit.begin_span(),
        }
    }
}

impl<T: BeginSpan + ?Sized> BeginSpan for &'_ T {
    fn begin_span(&self) -> Option<Span> {
        T::begin_span(self)
    }
}

impl<T: BeginSpan + ?Sized> BeginSpan for Box<T> {
    fn begin_span(&self) -> Option<Span> {
        T::begin_span(self)
    }
}

impl<T: BeginSpan> BeginSpan for Option<T> {
    fn begin_span(&self) -> Option<Span> {
        self.as_ref()?.begin_span()
    }
}

impl<T: BeginSpan> BeginSpan for [T] {
    fn begin_span(&self) -> Option<Span> {
        self.first()?.begin_span()
    }
}

impl<T: BeginSpan> BeginSpan for Vec<T> {
    fn begin_span(&self) -> Option<Span> {
        <[T]>::begin_span(self)
    }
}
impl<T: BeginSpan, P> BeginSpan for Punctuated<T, P> {
    fn begin_span(&self) -> Option<Span> {
        self.first()?.begin_span()
    }
}

impl BeginSpan for syn::Attribute {
    fn begin_span(&self) -> Option<Span> {
        Some(self.pound_token.span)
    }
}

impl BeginSpan for syn::Visibility {
    fn begin_span(&self) -> Option<Span> {
        match self {
            syn::Visibility::Public(tk) => Some(tk.span),
            syn::Visibility::Restricted(tk) => Some(tk.pub_token.span),
            syn::Visibility::Inherited => None,
        }
    }
}

macro_rules! impl_begin_span {
    ($ident:ident, $self:ident, $block:block) => {
        impl BeginSpan for syn::$ident {
            fn begin_span(&$self) -> Option<Span> $block
        }
    };
}
impl_begin_span!(ItemConst, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.const_token.span))
});
impl_begin_span!(ItemEnum, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.enum_token.span))
});
impl_begin_span!(ItemExternCrate, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.extern_token.span))
});
impl_begin_span!(Signature, self, {
    (self.constness.map(|tk| tk.span))
        .or_else(|| Some(self.asyncness?.span))
        .or_else(|| Some(self.unsafety?.span))
        .or_else(|| Some(self.abi.as_ref()?.extern_token.span))
        .or(Some(self.fn_token.span))
});
impl_begin_span!(ItemFn, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or_else(|| self.sig.begin_span())
});
impl_begin_span!(ItemForeignMod, self, {
    (self.attrs.begin_span())
        .or_else(|| Some(self.unsafety?.span))
        .or(Some(self.abi.extern_token.span))
});
impl_begin_span!(ItemImpl, self, {
    (self.attrs.begin_span())
        .or_else(|| Some(self.defaultness?.span))
        .or_else(|| Some(self.unsafety?.span))
        .or(Some(self.impl_token.span))
});
impl_begin_span!(PathSegment, self, { self.ident.begin_span() });
impl_begin_span!(Path, self, {
    self.leading_colon
        .map(|tk| tk.spans[0])
        .or_else(|| self.segments[0].begin_span())
});
impl_begin_span!(Macro, self, { self.path.begin_span() });
impl_begin_span!(ItemMacro, self, {
    (self.attrs.begin_span()).or_else(|| self.mac.begin_span())
});
impl_begin_span!(ItemMod, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or_else(|| Some(self.unsafety?.span))
        .or(Some(self.mod_token.span))
});
impl_begin_span!(ItemStatic, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.static_token.span))
});
impl_begin_span!(ItemStruct, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.struct_token.span))
});
impl_begin_span!(ItemTrait, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or_else(|| Some(self.unsafety?.span))
        .or_else(|| Some(self.auto_token?.span))
        .or_else(|| self.restriction.as_ref().map(|_| unimplemented!()))
        .or(Some(self.trait_token.span))
});
impl_begin_span!(ItemTraitAlias, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.trait_token.span))
});
impl_begin_span!(ItemType, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.type_token.span))
});
impl_begin_span!(ItemUnion, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.union_token.span))
});
impl_begin_span!(ItemUse, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.use_token.span))
});
impl_begin_span!(Item, self, {
    match self {
        syn::Item::Const(slf) => slf.begin_span(),
        syn::Item::Enum(slf) => slf.begin_span(),
        syn::Item::ExternCrate(slf) => slf.begin_span(),
        syn::Item::Fn(slf) => slf.begin_span(),
        syn::Item::ForeignMod(slf) => slf.begin_span(),
        syn::Item::Impl(slf) => slf.begin_span(),
        syn::Item::Macro(slf) => slf.begin_span(),
        syn::Item::Mod(slf) => slf.begin_span(),
        syn::Item::Static(slf) => slf.begin_span(),
        syn::Item::Struct(slf) => slf.begin_span(),
        syn::Item::Trait(slf) => slf.begin_span(),
        syn::Item::TraitAlias(slf) => slf.begin_span(),
        syn::Item::Type(slf) => slf.begin_span(),
        syn::Item::Union(slf) => slf.begin_span(),
        syn::Item::Use(slf) => slf.begin_span(),
        syn::Item::Verbatim(slf) => slow_begin_span(slf),
        _ => slow_begin_span(self),
    }
});
impl_begin_span!(ImplItemConst, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or_else(|| Some(self.defaultness?.span))
        .or(Some(self.const_token.span))
});
impl_begin_span!(ImplItemFn, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or_else(|| Some(self.defaultness?.span))
        .or_else(|| self.sig.begin_span())
});
impl_begin_span!(ImplItemType, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or_else(|| Some(self.defaultness?.span))
        .or(Some(self.type_token.span))
});
impl_begin_span!(ImplItemMacro, self, {
    (self.attrs.begin_span()).or_else(|| self.mac.begin_span())
});
impl_begin_span!(ImplItem, self, {
    match self {
        syn::ImplItem::Const(slf) => slf.begin_span(),
        syn::ImplItem::Fn(slf) => slf.begin_span(),
        syn::ImplItem::Type(slf) => slf.begin_span(),
        syn::ImplItem::Macro(slf) => slf.begin_span(),
        syn::ImplItem::Verbatim(slf) => slow_begin_span(slf),
        _ => slow_begin_span(self),
    }
});
impl_begin_span!(TraitItemConst, self, {
    (self.attrs.begin_span()).or(Some(self.const_token.span))
});
impl_begin_span!(TraitItemFn, self, {
    (self.attrs.begin_span()).or_else(|| self.sig.begin_span())
});
impl_begin_span!(TraitItemType, self, {
    (self.attrs.begin_span()).or(Some(self.type_token.span))
});
impl_begin_span!(TraitItemMacro, self, {
    (self.attrs.begin_span()).or_else(|| self.mac.begin_span())
});
impl_begin_span!(TraitItem, self, {
    match self {
        syn::TraitItem::Const(slf) => slf.begin_span(),
        syn::TraitItem::Fn(slf) => slf.begin_span(),
        syn::TraitItem::Type(slf) => slf.begin_span(),
        syn::TraitItem::Macro(slf) => slf.begin_span(),
        syn::TraitItem::Verbatim(slf) => slow_begin_span(slf),
        _ => slow_begin_span(self),
    }
});
impl_begin_span!(ForeignItemFn, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or_else(|| self.sig.begin_span())
});
impl_begin_span!(ForeignItemStatic, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.static_token.span))
});
impl_begin_span!(ForeignItemType, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or(Some(self.type_token.span))
});
impl_begin_span!(ForeignItemMacro, self, {
    (self.attrs.begin_span()).or_else(|| self.mac.begin_span())
});
impl_begin_span!(ForeignItem, self, {
    match self {
        syn::ForeignItem::Fn(slf) => slf.begin_span(),
        syn::ForeignItem::Static(slf) => slf.begin_span(),
        syn::ForeignItem::Type(slf) => slf.begin_span(),
        syn::ForeignItem::Macro(slf) => slf.begin_span(),
        syn::ForeignItem::Verbatim(slf) => slow_begin_span(slf),
        _ => slow_begin_span(self),
    }
});
impl_begin_span!(Local, self, {
    (self.attrs.begin_span()).or(Some(self.let_token.span))
});
impl_begin_span!(StmtMacro, self, {
    (self.attrs.begin_span()).or_else(|| self.mac.begin_span())
});
impl_begin_span!(ExprArray, self, {
    (self.attrs.begin_span()).or(Some(self.bracket_token.span.open()))
});
impl_begin_span!(ExprAssign, self, {
    (self.attrs.begin_span()).or_else(|| self.left.begin_span())
});
impl_begin_span!(ExprAsync, self, {
    (self.attrs.begin_span()).or(Some(self.async_token.span))
});
impl_begin_span!(ExprAwait, self, {
    (self.attrs.begin_span()).or_else(|| self.base.begin_span())
});
impl_begin_span!(ExprBinary, self, {
    (self.attrs.begin_span()).or_else(|| self.left.begin_span())
});
impl_begin_span!(Lifetime, self, { Some(self.apostrophe) });
impl_begin_span!(Label, self, { self.name.begin_span() });
impl_begin_span!(Block, self, { Some(self.brace_token.span.open()) });
impl_begin_span!(ExprBlock, self, {
    (self.attrs.begin_span())
        .or_else(|| self.label.begin_span())
        .or_else(|| self.block.begin_span())
});
impl_begin_span!(ExprBreak, self, {
    (self.attrs.begin_span()).or(Some(self.break_token.span))
});
impl_begin_span!(ExprCall, self, {
    (self.attrs.begin_span()).or_else(|| self.func.begin_span())
});
impl_begin_span!(ExprCast, self, {
    (self.attrs.begin_span()).or_else(|| self.expr.begin_span())
});
impl_begin_span!(BoundLifetimes, self, { Some(self.for_token.span) });
impl_begin_span!(ExprClosure, self, {
    (self.attrs.begin_span())
        .or_else(|| self.lifetimes.begin_span())
        .or_else(|| Some(self.constness?.span))
        .or_else(|| Some(self.movability?.span))
        .or_else(|| Some(self.asyncness?.span))
        .or_else(|| Some(self.capture?.span))
        .or(Some(self.or1_token.span))
});
impl_begin_span!(ExprConst, self, {
    (self.attrs.begin_span()).or(Some(self.const_token.span))
});
impl_begin_span!(ExprContinue, self, {
    (self.attrs.begin_span()).or(Some(self.continue_token.span))
});
impl_begin_span!(ExprField, self, {
    (self.attrs.begin_span()).or_else(|| self.base.begin_span())
});
impl_begin_span!(ExprForLoop, self, {
    (self.attrs.begin_span())
        .or_else(|| self.label.begin_span())
        .or(Some(self.for_token.span))
});
impl_begin_span!(ExprGroup, self, {
    (self.attrs.begin_span()).or_else(|| self.expr.begin_span())
});
impl_begin_span!(ExprIf, self, {
    (self.attrs.begin_span()).or(Some(self.if_token.span))
});
impl_begin_span!(ExprIndex, self, {
    (self.attrs.begin_span()).or_else(|| self.expr.begin_span())
});
impl_begin_span!(ExprInfer, self, {
    (self.attrs.begin_span()).or(Some(self.underscore_token.span))
});
impl_begin_span!(ExprLet, self, {
    (self.attrs.begin_span()).or(Some(self.let_token.span))
});
impl_begin_span!(ExprLit, self, {
    (self.attrs.begin_span()).or(Some(self.lit.span()))
});
impl_begin_span!(ExprLoop, self, {
    (self.attrs.begin_span())
        .or_else(|| self.label.begin_span())
        .or(Some(self.loop_token.span))
});
impl_begin_span!(ExprMacro, self, {
    (self.attrs.begin_span()).or_else(|| self.mac.begin_span())
});
impl_begin_span!(ExprMatch, self, {
    (self.attrs.begin_span()).or(Some(self.match_token.span))
});
impl_begin_span!(ExprMethodCall, self, {
    (self.attrs.begin_span()).or_else(|| self.receiver.begin_span())
});
impl_begin_span!(ExprParen, self, {
    (self.attrs.begin_span()).or(Some(self.paren_token.span.open()))
});
impl_begin_span!(QSelf, self, { Some(self.lt_token.span) });
impl_begin_span!(ExprPath, self, {
    (self.attrs.begin_span())
        .or_else(|| self.qself.begin_span())
        .or_else(|| self.path.begin_span())
});
impl_begin_span!(RangeLimits, self, {
    match self {
        syn::RangeLimits::HalfOpen(tk) => Some(tk.spans[0]),
        syn::RangeLimits::Closed(tk) => Some(tk.spans[0]),
    }
});
impl_begin_span!(ExprRange, self, {
    (self.attrs.begin_span())
        .or_else(|| self.start.begin_span())
        .or_else(|| self.limits.begin_span())
});
impl_begin_span!(ExprReference, self, {
    (self.attrs.begin_span()).or(Some(self.and_token.span))
});
impl_begin_span!(ExprRepeat, self, {
    (self.attrs.begin_span()).or(Some(self.bracket_token.span.open()))
});
impl_begin_span!(ExprReturn, self, {
    (self.attrs.begin_span()).or(Some(self.return_token.span))
});
impl_begin_span!(ExprStruct, self, {
    (self.attrs.begin_span())
        .or_else(|| self.qself.begin_span())
        .or_else(|| self.path.begin_span())
});
impl_begin_span!(ExprTry, self, {
    (self.attrs.begin_span()).or_else(|| self.expr.begin_span())
});
impl_begin_span!(ExprTryBlock, self, {
    (self.attrs.begin_span()).or(Some(self.try_token.span))
});
impl_begin_span!(ExprTuple, self, {
    (self.attrs.begin_span()).or(Some(self.paren_token.span.open()))
});
impl_begin_span!(UnOp, self, {
    match self {
        syn::UnOp::Deref(tk) => Some(tk.span),
        syn::UnOp::Not(tk) => Some(tk.span),
        syn::UnOp::Neg(tk) => Some(tk.span),
        _ => slow_begin_span(self),
    }
});
impl_begin_span!(ExprUnary, self, {
    (self.attrs.begin_span()).or_else(|| self.op.begin_span())
});
impl_begin_span!(ExprUnsafe, self, {
    (self.attrs.begin_span()).or(Some(self.unsafe_token.span))
});
impl_begin_span!(ExprWhile, self, {
    (self.attrs.begin_span())
        .or_else(|| self.label.begin_span())
        .or(Some(self.while_token.span))
});
impl_begin_span!(ExprYield, self, {
    (self.attrs.begin_span()).or(Some(self.yield_token.span))
});
impl_begin_span!(Expr, self, {
    match self {
        syn::Expr::Array(slf) => slf.begin_span(),
        syn::Expr::Assign(slf) => slf.begin_span(),
        syn::Expr::Async(slf) => slf.begin_span(),
        syn::Expr::Await(slf) => slf.begin_span(),
        syn::Expr::Binary(slf) => slf.begin_span(),
        syn::Expr::Block(slf) => slf.begin_span(),
        syn::Expr::Break(slf) => slf.begin_span(),
        syn::Expr::Call(slf) => slf.begin_span(),
        syn::Expr::Cast(slf) => slf.begin_span(),
        syn::Expr::Closure(slf) => slf.begin_span(),
        syn::Expr::Const(slf) => slf.begin_span(),
        syn::Expr::Continue(slf) => slf.begin_span(),
        syn::Expr::Field(slf) => slf.begin_span(),
        syn::Expr::ForLoop(slf) => slf.begin_span(),
        syn::Expr::Group(slf) => slf.begin_span(),
        syn::Expr::If(slf) => slf.begin_span(),
        syn::Expr::Index(slf) => slf.begin_span(),
        syn::Expr::Infer(slf) => slf.begin_span(),
        syn::Expr::Let(slf) => slf.begin_span(),
        syn::Expr::Lit(slf) => slf.begin_span(),
        syn::Expr::Loop(slf) => slf.begin_span(),
        syn::Expr::Macro(slf) => slf.begin_span(),
        syn::Expr::Match(slf) => slf.begin_span(),
        syn::Expr::MethodCall(slf) => slf.begin_span(),
        syn::Expr::Paren(slf) => slf.begin_span(),
        syn::Expr::Path(slf) => slf.begin_span(),
        syn::Expr::Range(slf) => slf.begin_span(),
        syn::Expr::Reference(slf) => slf.begin_span(),
        syn::Expr::Repeat(slf) => slf.begin_span(),
        syn::Expr::Return(slf) => slf.begin_span(),
        syn::Expr::Struct(slf) => slf.begin_span(),
        syn::Expr::Try(slf) => slf.begin_span(),
        syn::Expr::TryBlock(slf) => slf.begin_span(),
        syn::Expr::Tuple(slf) => slf.begin_span(),
        syn::Expr::Unary(slf) => slf.begin_span(),
        syn::Expr::Unsafe(slf) => slf.begin_span(),
        syn::Expr::While(slf) => slf.begin_span(),
        syn::Expr::Yield(slf) => slf.begin_span(),
        syn::Expr::Verbatim(slf) => slow_begin_span(slf),
        _ => slow_begin_span(self),
    }
});
impl_begin_span!(Stmt, self, {
    match self {
        syn::Stmt::Local(slf) => slf.begin_span(),
        syn::Stmt::Item(slf) => slf.begin_span(),
        syn::Stmt::Expr(slf, _) => slf.begin_span(),
        syn::Stmt::Macro(slf) => slf.begin_span(),
    }
});
impl_begin_span!(File, self, {
    (self.attrs.begin_span()).or_else(|| self.items.begin_span())
});
impl_begin_span!(PatIdent, self, {
    (self.attrs.begin_span())
        .or_else(|| Some(self.by_ref?.span))
        .or_else(|| Some(self.mutability?.span))
        .or(Some(self.ident.span()))
});
impl_begin_span!(PatOr, self, {
    (self.attrs.begin_span())
        .or_else(|| Some(self.leading_vert?.span))
        .or_else(|| self.cases.begin_span())
});
impl_begin_span!(PatParen, self, {
    (self.attrs.begin_span()).or(Some(self.paren_token.span.open()))
});
impl_begin_span!(PatReference, self, {
    (self.attrs.begin_span()).or(Some(self.and_token.span))
});
impl_begin_span!(PatRest, self, {
    (self.attrs.begin_span()).or(Some(self.dot2_token.spans[0]))
});
impl_begin_span!(PatSlice, self, {
    (self.attrs.begin_span()).or(Some(self.bracket_token.span.open()))
});
impl_begin_span!(PatStruct, self, {
    (self.attrs.begin_span())
        .or_else(|| self.qself.begin_span())
        .or_else(|| self.path.begin_span())
});
impl_begin_span!(PatTuple, self, {
    (self.attrs.begin_span()).or(Some(self.paren_token.span.open()))
});
impl_begin_span!(PatTupleStruct, self, {
    (self.attrs.begin_span())
        .or_else(|| self.qself.begin_span())
        .or_else(|| self.path.begin_span())
});
impl_begin_span!(PatType, self, {
    (self.attrs.begin_span()).or_else(|| self.pat.begin_span())
});
impl_begin_span!(PatWild, self, {
    (self.attrs.begin_span()).or(Some(self.underscore_token.span))
});
impl_begin_span!(Pat, self, {
    match self {
        syn::Pat::Const(slf) => slf.begin_span(),
        syn::Pat::Ident(slf) => slf.begin_span(),
        syn::Pat::Lit(slf) => slf.begin_span(),
        syn::Pat::Macro(slf) => slf.begin_span(),
        syn::Pat::Or(slf) => slf.begin_span(),
        syn::Pat::Paren(slf) => slf.begin_span(),
        syn::Pat::Path(slf) => slf.begin_span(),
        syn::Pat::Range(slf) => slf.begin_span(),
        syn::Pat::Reference(slf) => slf.begin_span(),
        syn::Pat::Rest(slf) => slf.begin_span(),
        syn::Pat::Slice(slf) => slf.begin_span(),
        syn::Pat::Struct(slf) => slf.begin_span(),
        syn::Pat::Tuple(slf) => slf.begin_span(),
        syn::Pat::TupleStruct(slf) => slf.begin_span(),
        syn::Pat::Type(slf) => slf.begin_span(),
        syn::Pat::Wild(slf) => slf.begin_span(),
        syn::Pat::Verbatim(slf) => slow_begin_span(slf),
        _ => slow_begin_span(self),
    }
});
impl_begin_span!(FieldMutability, self, {
    match self {
        syn::FieldMutability::None => None,
        _ => None,
    }
});
impl_begin_span!(Field, self, {
    (self.attrs.begin_span())
        .or_else(|| self.vis.begin_span())
        .or_else(|| self.mutability.begin_span())
        .or_else(|| self.ident.begin_span())
        .or_else(|| Some(self.colon_token?.span))
        .or_else(|| self.ty.begin_span())
});
impl_begin_span!(Variant, self, {
    (self.attrs.begin_span()).or_else(|| self.ident.begin_span())
});
impl_begin_span!(TypeArray, self, { Some(self.bracket_token.span.open()) });
impl_begin_span!(TypeBareFn, self, {
    (self.lifetimes.begin_span())
        .or_else(|| Some(self.unsafety?.span))
        .or_else(|| Some(self.abi.as_ref()?.extern_token.span))
        .or(Some(self.fn_token.span))
});
impl_begin_span!(TypeGroup, self, { self.elem.begin_span() });
impl_begin_span!(TypeImplTrait, self, { Some(self.impl_token.span) });
impl_begin_span!(TypeInfer, self, { Some(self.underscore_token.span) });
impl_begin_span!(TypeMacro, self, { self.mac.begin_span() });
impl_begin_span!(TypeNever, self, { Some(self.bang_token.span) });
impl_begin_span!(TypeParen, self, { Some(self.paren_token.span.open()) });
impl_begin_span!(TypePath, self, {
    (self.qself.begin_span()).or_else(|| self.path.begin_span())
});
impl_begin_span!(TypePtr, self, { Some(self.star_token.span) });
impl_begin_span!(TypeReference, self, { Some(self.and_token.span) });
impl_begin_span!(TypeSlice, self, { Some(self.bracket_token.span.open()) });
impl_begin_span!(TraitBoundModifier, self, {
    match self {
        syn::TraitBoundModifier::None => None,
        syn::TraitBoundModifier::Maybe(tk) => Some(tk.span),
    }
});
impl_begin_span!(TraitBound, self, {
    (self.paren_token.map(|tk| tk.span.open()))
        .or_else(|| self.modifier.begin_span())
        .or_else(|| self.lifetimes.begin_span())
        .or_else(|| self.path.begin_span())
});
impl_begin_span!(TypeParamBound, self, {
    match self {
        syn::TypeParamBound::Trait(slf) => slf.begin_span(),
        syn::TypeParamBound::Lifetime(slf) => slf.begin_span(),
        syn::TypeParamBound::Verbatim(slf) => slow_begin_span(slf),
        _ => slow_begin_span(self),
    }
});
impl_begin_span!(TypeTraitObject, self, {
    (self.dyn_token.map(|tk| tk.span)).or_else(|| self.bounds.begin_span())
});
impl_begin_span!(TypeTuple, self, { Some(self.paren_token.span.open()) });
impl_begin_span!(Type, self, {
    match self {
        syn::Type::Array(slf) => slf.begin_span(),
        syn::Type::BareFn(slf) => slf.begin_span(),
        syn::Type::Group(slf) => slf.begin_span(),
        syn::Type::ImplTrait(slf) => slf.begin_span(),
        syn::Type::Infer(slf) => slf.begin_span(),
        syn::Type::Macro(slf) => slf.begin_span(),
        syn::Type::Never(slf) => slf.begin_span(),
        syn::Type::Paren(slf) => slf.begin_span(),
        syn::Type::Path(slf) => slf.begin_span(),
        syn::Type::Ptr(slf) => slf.begin_span(),
        syn::Type::Reference(slf) => slf.begin_span(),
        syn::Type::Slice(slf) => slf.begin_span(),
        syn::Type::TraitObject(slf) => slf.begin_span(),
        syn::Type::Tuple(slf) => slf.begin_span(),
        syn::Type::Verbatim(slf) => slow_begin_span(slf),
        _ => slow_begin_span(self),
    }
});
impl_begin_span!(Receiver, self, {
    (self.attrs.begin_span())
        .or_else(|| Some(self.reference.as_ref()?.0.span))
        .or_else(|| Some(self.mutability?.span))
        .or(Some(self.self_token.span))
});
impl_begin_span!(FnArg, self, {
    match self {
        syn::FnArg::Receiver(slf) => slf.begin_span(),
        syn::FnArg::Typed(slf) => slf.begin_span(),
    }
});
impl_begin_span!(Variadic, self, {
    (self.attrs.begin_span())
        .or_else(|| self.pat.as_ref()?.0.begin_span())
        .or(Some(self.dots.spans[0]))
});
impl_begin_span!(PredicateLifetime, self, { Some(self.lifetime.apostrophe) });
impl_begin_span!(PredicateType, self, {
    (self.lifetimes.begin_span()).or_else(|| self.bounded_ty.begin_span())
});
impl_begin_span!(WherePredicate, self, {
    match self {
        syn::WherePredicate::Lifetime(slf) => slf.begin_span(),
        syn::WherePredicate::Type(slf) => slf.begin_span(),
        _ => slow_begin_span(self),
    }
});
