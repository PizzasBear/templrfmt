use proc_macro2::{Span, TokenStream, TokenTree};
use templr_parser::{
    attrs::HtmlAttrValue, call, element::OpenTag, name::NamePart, Attr, Block as TemplrBlock, Call,
    Doctype, Element, Entity, For, If, Let, Match, Name, Node, Scope, TemplBody, Use,
};

use crate::{
    algorithm::Printer,
    comments::{slow_begin_span, BeginSpan},
    path::PathKind,
    INDENT,
};

impl Printer {
    pub fn templ_macro(&mut self, mac: &syn::Macro, semicolon: bool) -> bool {
        if !(mac.path.segments.last())
            .is_some_and(|segment| segment.ident == "templ" && segment.arguments.is_none())
        {
            return false;
        }
        let body: TemplBody = match syn::parse2(mac.tokens.clone()) {
            Ok(body) => body,
            Err(_err) => {
                return false;
            }
        };

        self.cbox(INDENT);
        self.path(&mac.path, PathKind::Simple);
        self.word("! {");
        self.hardbreak_if_nonempty();
        for u in &body.uses {
            self.templ_use(u);
        }
        if self.templ_nodes(&body.nodes, false) {
            self.hardbreak();
        }
        self.flush_comments(mac.delimiter.span().close(), body.nodes.is_empty(), true);
        self.offset(-INDENT);
        self.word("}");
        self.end();

        if semicolon {
            self.word(";");
        }

        true
    }

    fn templ_use(&mut self, u: &Use) {
        if let Some(span) = u.begin_span() {
            self.flush_comments(span, false, false);
        }

        match u {
            Use::Context(templr_parser::UseContext {
                as_pat, colon_ty, ..
            }) => {
                self.ibox(0);
                self.word("#use context");
                if let Some((_, pat)) = as_pat {
                    self.word(" as ");
                    self.pat(&pat);
                }
                if let Some((_, ty)) = colon_ty {
                    self.word(": ");
                    self.ty(ty);
                }
                self.end();
                self.word(";");
                self.hardbreak();
            }
            Use::Children(templr_parser::UseChildren { as_pat, .. }) => {
                self.ibox(0);
                self.word("#use children");
                if let Some((_, pat)) = as_pat {
                    self.word(" as ");
                    self.pat(&pat);
                }
                self.end();
                self.word(";");
                self.hardbreak();
            }
        }
    }

    fn templ_nodes(&mut self, nodes: &[Node], in_group: bool) -> bool {
        let mut interpolated_text = in_group;
        let mut space = false;
        for node in nodes {
            match (
                interpolated_text,
                matches!(
                    node,
                    Node::RawText(_)
                        | Node::Expr(_)
                        | Node::Entity(_)
                        | Node::Paren(_, _)
                        | Node::Bracket(_, _)
                ),
            ) {
                (false, true) => {
                    space = false;
                    interpolated_text = true;
                    self.ibox(0);
                }
                (true, false) => {
                    interpolated_text = false;
                    self.end();
                    self.hardbreak();
                }
                _ => {}
            }
            match node {
                Node::Entity(entity) => {
                    if space {
                        self.space();
                    }
                    space = true;
                    self.templ_entity(entity);
                }
                Node::Doctype(doctype) => self.templ_doctype(doctype),
                Node::Element(el) => self.templ_element(el),
                Node::RawText(raw_text) => {
                    space = self.templ_raw_text(raw_text.tokens.clone(), space);
                }
                Node::Paren(_, nodes) => {
                    if space {
                        self.space();
                    }
                    space = true;
                    self.word("(");
                    if !self.templ_nodes(nodes, true) {
                        self.ibox(0);
                    }
                    self.word(")");
                }
                Node::Bracket(_, nodes) => {
                    if space {
                        self.space();
                    }
                    space = true;
                    self.word("[");
                    if !self.templ_nodes(nodes, true) {
                        self.ibox(0);
                    }
                    self.word("]");
                    // self.cbox(INDENT);
                    // self.word("[");
                    // self.zerobreak();
                    // self.ibox(0);
                    // self.templ_nodes(nodes);
                    // self.end();
                    // self.zerobreak();
                    // self.offset(-INDENT);
                    // self.word("]");
                    // self.end();
                }
                Node::Expr(block) => {
                    if space {
                        self.space();
                    }
                    space = false;
                    self.templ_block(block, true);
                }
                Node::If(stmt) => {
                    self.templ_if(stmt, |slf, nodes| {
                        if !nodes.is_empty() {
                            slf.hardbreak();
                        }
                        if slf.templ_nodes(nodes, false) {
                            slf.hardbreak();
                        }
                    });
                    self.hardbreak();
                }
                Node::Match(stmt) => {
                    self.templ_match(stmt, |slf, nodes| {
                        if !nodes.is_empty() {
                            slf.hardbreak();
                        }
                        if slf.templ_nodes(nodes, false) {
                            slf.hardbreak();
                        }
                    });
                    self.hardbreak();
                }
                Node::For(stmt) => {
                    self.templ_for(stmt, |slf, nodes| {
                        if !nodes.is_empty() {
                            slf.hardbreak();
                        }
                        if slf.templ_nodes(nodes, false) {
                            slf.hardbreak();
                        }
                    });
                    self.hardbreak();
                }
                Node::Scope(Scope { brace, body, .. }) => {
                    self.cbox(INDENT);
                    self.word("#{");
                    if !body.is_empty() {
                        self.hardbreak();
                        if self.templ_nodes(body, false) {
                            self.hardbreak();
                        }
                        self.flush_comments(brace.span.close(), body.is_empty(), true);
                        self.offset(-INDENT);
                    }
                    self.end();
                    self.word("}");
                    self.hardbreak();
                }
                Node::Let(stmt) => {
                    self.templ_let(stmt);
                    self.hardbreak();
                }
                Node::Call(call) => {
                    self.templ_call(call);
                }
            }
        }
        if interpolated_text && !in_group {
            self.end();
        }
        interpolated_text
    }

    fn templ_call(&mut self, call: &Call) {
        if let Some(span) = call.begin_span() {
            self.flush_comments(span, false, false);
        }
        self.ibox(0);
        self.word("#");
        match &call.end {
            call::End::Semi(_) => {
                self.expr(&call.expr);
                self.word(";");
            }
            call::End::Children(brace, body) => {
                self.neverbreak();
                self.wrap_exterior_struct(&call.expr);
                self.word("{");
                self.neverbreak();
                self.cbox(INDENT);
                self.hardbreak_if_nonempty();
                for u in &body.uses {
                    self.templ_use(u);
                }
                if !body.uses.is_empty() {
                    self.hardbreak_if_nonempty();
                }
                if self.templ_nodes(&body.nodes, false) {
                    self.hardbreak();
                }
                self.flush_comments(brace.span.close(), body.nodes.is_empty(), true);
                self.offset(-INDENT);
                self.end();
                self.word("}");
            }
        }
        self.end();
        self.hardbreak();
    }

    fn templ_element(&mut self, element: &Element) {
        if let Some(span) = element.begin_span() {
            self.flush_comments(span, false, false);
        }
        self.cbox(INDENT);
        if self.templ_open_tag(&element.open) {
            self.end();
            self.hardbreak();
            return;
        }
        self.zerobreak();
        if self.templ_nodes(&element.nodes, false) {
            self.zerobreak();
        }
        if let Some(span) = element.close.begin_span() {
            self.flush_comments(span, element.nodes.is_empty(), true);
        }
        self.offset(-INDENT);
        self.word("</");
        self.templ_name(&element.close.as_ref().unwrap().name);
        self.word(">");
        self.end();
        self.hardbreak();
    }

    fn templ_open_tag(&mut self, open: &OpenTag) -> bool {
        self.cbox(0);
        self.word("<");
        self.templ_name(&open.name);

        for attr in &open.attrs {
            self.templ_attr(attr);
        }

        if open.slash.is_some() {
            self.space();
            self.offset(-INDENT);
            self.word("/>");
            self.end();
            true
        } else {
            self.zerobreak();
            self.offset(-INDENT);
            self.word(">");
            self.end();
            false
        }
    }

    fn templ_attr(&mut self, attr: &Attr) {
        match attr {
            Attr::Html(_) | Attr::Spread(_) => self.space(),
            Attr::If(_) | Attr::Match(_) | Attr::Scope(_) | Attr::Let(_) => self.hardbreak(),
        }
        if let Some(span) = attr.begin_span() {
            self.flush_comments(span, false, false);
        }
        match attr {
            Attr::Html(attr) => {
                self.templ_name(&attr.name);
                match &attr.value {
                    HtmlAttrValue::Ident(_, ident) => self.word(format!("={ident}")),
                    HtmlAttrValue::Float(_, lit) => self.word(format!("={lit}")),
                    HtmlAttrValue::Int(_, lit) => self.word(format!("={lit}")),
                    HtmlAttrValue::Str(_, lit) => {
                        self.word("=");
                        self.lit_str(lit);
                    }
                    HtmlAttrValue::Block(toggle, _, block) => {
                        match toggle {
                            Some(_) => self.word("?="),
                            None => self.word("="),
                        }
                        self.templ_block(block, false);
                    }
                    HtmlAttrValue::None => {}
                }
            }
            Attr::Spread(block) => {
                self.templ_block(block, false);
            }
            Attr::If(stmt) => self.templ_if(stmt, |slf, attrs| {
                for attr in attrs {
                    slf.templ_attr(attr);
                }
                if !attrs.is_empty() {
                    slf.hardbreak();
                }
            }),
            Attr::Match(stmt) => {
                self.templ_match(stmt, |slf, attrs| {
                    for attr in attrs {
                        slf.templ_attr(attr);
                    }
                    if !attrs.is_empty() {
                        slf.hardbreak();
                    }
                });
            }
            Attr::Scope(Scope { brace, body, .. }) => {
                self.cbox(INDENT);
                self.word("#{");
                if !body.is_empty() {
                    for attr in body {
                        self.templ_attr(attr);
                    }
                    self.flush_comments(brace.span.close(), body.is_empty(), true);
                    self.hardbreak();
                    self.offset(-INDENT);
                }
                self.end();
                self.word("}");
            }
            Attr::Let(stmt) => {
                self.templ_let(stmt);
            }
        }
    }

    fn templ_let(&mut self, stmt: &Let) {
        if let Some(span) = stmt.begin_span() {
            self.flush_comments(span, false, false);
        }
        self.ibox(0);
        self.word("#let ");
        self.pat(&stmt.pat);
        if let Some(init) = &stmt.init {
            self.word(" = ");
            self.neverbreak();
            self.expr(&init.expr);
        }
        self.end();
        self.word(";");
    }

    fn templ_match<T>(&mut self, stmt: &Match<T>, mut items: impl FnMut(&mut Self, &[T])) {
        if let Some(span) = stmt.begin_span() {
            self.flush_comments(span, false, false);
        }
        self.ibox(0);
        self.word("#match ");
        self.wrap_exterior_struct(&stmt.expr);
        self.word("{");
        self.neverbreak();
        self.cbox(INDENT);
        self.hardbreak_if_nonempty();
        for arm in &stmt.arms {
            if let Some(span) = arm.begin_span() {
                self.flush_comments(span, false, false);
            }
            self.ibox(0);
            self.pat(&arm.pat);
            if let Some((_if_token, guard)) = &arm.guard {
                self.word(" if ");
                self.expr(guard);
            }
            self.word(" => {");
            self.neverbreak();
            self.cbox(INDENT);
            items(self, &arm.body);
            self.flush_comments(arm.brace.span.close(), arm.body.is_empty(), true);
            self.offset(-INDENT);
            self.end();
            self.word("}");
            self.end();
            self.hardbreak();
        }
        self.flush_comments(stmt.brace.span.close(), stmt.arms.is_empty(), true);
        self.offset(-INDENT);
        self.end();
        self.word("}");
        self.end();
    }

    fn templ_for<T>(&mut self, stmt: &For<T>, items: impl FnOnce(&mut Self, &[T])) {
        if let Some(span) = stmt.begin_span() {
            self.flush_comments(span, false, false);
        }
        self.ibox(0);
        self.word("#for ");
        self.pat(&stmt.pat);
        self.word(" in ");
        self.neverbreak();
        self.wrap_exterior_struct(&stmt.expr);
        self.word("{");
        self.neverbreak();
        self.cbox(INDENT);
        items(self, &stmt.body);
        self.flush_comments(stmt.brace.span.close(), stmt.body.is_empty(), true);
        self.offset(-INDENT);
        self.end();
        self.word("}");
        self.end();
    }

    fn templ_if<T>(&mut self, stmt: &If<T>, mut items: impl FnMut(&mut Self, &[T])) {
        if let Some(span) = stmt.begin_span() {
            self.flush_comments(span, false, false);
        }
        self.cbox(INDENT);
        self.word("#if ");
        self.cbox(-INDENT);
        self.wrap_exterior_struct(&stmt.cond);
        self.end();
        self.word("{");
        if stmt.body.is_empty() && (!stmt.else_if_branches.is_empty() || stmt.else_branch.is_some())
        {
            self.hardbreak();
        } else {
            self.neverbreak();
        }
        items(self, &stmt.body);
        self.flush_comments(stmt.brace.span.close(), stmt.body.is_empty(), true);
        self.offset(-INDENT);
        self.word("}");
        for branch in &stmt.else_if_branches {
            self.word(" else if ");
            self.cbox(-INDENT);
            self.wrap_exterior_struct(&branch.cond);
            self.end();
            self.word("{");
            if branch.body.is_empty() {
                self.hardbreak();
            } else {
                self.neverbreak();
            }
            items(self, &branch.body);
            self.flush_comments(branch.brace.span.close(), branch.body.is_empty(), true);
            self.offset(-INDENT);
            self.word("}");
        }
        if let Some(branch) = &stmt.else_branch {
            self.word(" else {");
            if branch.body.is_empty() {
                self.hardbreak();
            } else {
                self.neverbreak();
            }
            items(self, &branch.body);
            self.flush_comments(branch.brace.span.close(), branch.body.is_empty(), true);
            self.offset(-INDENT);
            self.word("}");
        }
        self.end();
    }

    fn templ_block(&mut self, block: &TemplrBlock, endbreak: bool) {
        match block {
            TemplrBlock::Valid(block) => match &*block.stmts {
                [syn::Stmt::Expr(
                    syn::Expr::Lit(syn::ExprLit {
                        attrs,
                        lit: syn::Lit::Str(lit),
                    }),
                    None,
                )] if attrs.is_empty() => {
                    self.word("{");
                    self.lit_str(lit);
                    self.word("}");
                    if endbreak {
                        self.space();
                    }
                }
                _ => {
                    self.cbox(INDENT);
                    self.small_block(block, &[]);
                    if endbreak {
                        self.space();
                        self.offset(-INDENT);
                    }
                    self.end();
                }
            },
            TemplrBlock::Invalid { body, .. } => {
                if !body.is_empty() {
                    self.word("{");
                    self.cbox(INDENT);
                    self.space();
                    self.ibox(0);
                    self.macro_rules_tokens(body.clone(), false);
                    self.end();
                    self.space();
                    self.offset(-INDENT);
                    self.end();
                    self.word("}");
                    if endbreak {
                        self.space();
                        self.offset(-INDENT);
                    }
                }
            }
        }
    }

    fn templ_name(&mut self, name: &Name) {
        match name {
            Name::Str(s) => self.lit_str(s),
            Name::Parts(parts) => {
                self.ibox(INDENT);
                for part in parts {
                    match part {
                        NamePart::Ident(ident) => self.ident(ident),
                        NamePart::Int(int) => self.lit(&syn::Lit::Int(int.clone())),
                        NamePart::Float(float) => self.lit(&syn::Lit::Float(float.clone())),
                        NamePart::Dot(_, _) => self.word("."),
                        NamePart::Hyphen(_, _) => self.word("-"),
                        NamePart::Colon(_, _) => self.word(":"),
                        NamePart::At(_, _) => self.word("@"),
                        NamePart::Question(_, _) => self.word("?"),
                    }
                }
                self.end();
            }
        }
    }

    fn templ_raw_text(&mut self, tokens: TokenStream, mut space: bool) -> bool {
        for token in tokens {
            match token {
                TokenTree::Punct(p)
                    if matches!(p.as_char(), '.' | ',' | ':' | ';' | '!' | '?' | '%') =>
                {
                    space = true;
                    self.word(p.as_char().to_string());
                }
                TokenTree::Punct(p) if matches!(p.as_char(), '$' | '#' | '¿' | '¡') => {
                    if space {
                        space = false;
                        self.space();
                    }
                    self.word(p.as_char().to_string());
                }
                _ => {
                    if space {
                        self.space();
                    }
                    space = true;
                    self.single_token(token.into(), |_, _| unreachable!());
                }
            }
        }
        space
    }

    fn templ_entity(&mut self, entity: &Entity) {
        self.word(entity.to_string());
    }

    fn templ_doctype(&mut self, doctype: &Doctype) {
        if let Some(span) = doctype.begin_span() {
            self.flush_comments(span, false, false);
        }
        self.cbox(INDENT);
        self.word("<!DOCTYPE");
        self.space();
        self.templ_name(&doctype.name);
        self.zerobreak();
        self.offset(-INDENT);
        self.word(">");
        self.end();
        self.hardbreak();
    }
}

macro_rules! impl_begin_span {
    ($($path_sef:ident)::+$(::<$($ty:ident),+>)?, $self:ident, $block:block) => {
        impl$(<$($ty),+>)? BeginSpan for templr_parser::$($path_sef)::+$(<$($ty),+>)? {
            fn begin_span(&$self) -> Option<Span> $block
        }
    };
}
impl_begin_span!(UseContext, self, { Some(self.pound.span) });
impl_begin_span!(UseChildren, self, { Some(self.pound.span) });
impl_begin_span!(Use, self, {
    match self {
        Use::Context(slf) => slf.begin_span(),
        Use::Children(slf) => slf.begin_span(),
    }
});
impl_begin_span!(entity::NamedEntity, self, { Some(self.amp.span) });
impl_begin_span!(entity::DecimalEntity, self, { Some(self.amp.span) });
impl_begin_span!(entity::HexEntity, self, { Some(self.amp.span) });
impl_begin_span!(Entity, self, {
    match self {
        Entity::Named(slf) => slf.begin_span(),
        Entity::Decimal(slf) => slf.begin_span(),
        Entity::Hex(slf) => slf.begin_span(),
    }
});
impl_begin_span!(Doctype, self, { Some(self.lt.span) });
impl_begin_span!(element::OpenTag, self, { Some(self.lt.span) });
impl_begin_span!(element::CloseTag, self, { Some(self.lt.span) });
impl_begin_span!(Element, self, { self.open.begin_span() });
impl_begin_span!(RawText, self, { slow_begin_span(self) });
impl_begin_span!(Block, self, {
    match self {
        Self::Valid(slf) => slf.begin_span(),
        Self::Invalid { brace, .. } => Some(brace.span.open()),
    }
});
impl_begin_span!(If::<T>, self, { Some(self.pound.span) });
impl_begin_span!(match_stmt::Arm::<T>, self, { self.pat.begin_span() });
impl_begin_span!(Match::<T>, self, { Some(self.pound.span) });
impl_begin_span!(For::<T>, self, { Some(self.pound.span) });
impl_begin_span!(Scope::<T>, self, { Some(self.pound.span) });
impl_begin_span!(Let, self, { Some(self.pound.span) });
impl_begin_span!(Call, self, { Some(self.pound.span) });
impl_begin_span!(Node, self, {
    match self {
        Node::Entity(slf) => slf.begin_span(),
        Node::Doctype(slf) => slf.begin_span(),
        Node::Element(slf) => slf.begin_span(),
        Node::RawText(slf) => slf.begin_span(),
        Node::Paren(paren, _) => Some(paren.span.open()),
        Node::Bracket(bracket, _) => Some(bracket.span.open()),
        Node::Expr(slf) => slf.begin_span(),
        Node::If(slf) => slf.begin_span(),
        Node::Match(slf) => slf.begin_span(),
        Node::For(slf) => slf.begin_span(),
        Node::Scope(slf) => slf.begin_span(),
        Node::Let(slf) => slf.begin_span(),
        Node::Call(slf) => slf.begin_span(),
    }
});
impl_begin_span!(name::NamePart, self, {
    match self {
        NamePart::Ident(slf) => Some(slf.span()),
        NamePart::Int(slf) => Some(slf.span()),
        NamePart::Float(slf) => Some(slf.span()),
        NamePart::Dot(slf, _) => Some(slf.span),
        NamePart::Hyphen(slf, _) => Some(slf.span),
        NamePart::Colon(slf, _) => Some(slf.span),
        NamePart::At(slf, _) => Some(slf.span),
        NamePart::Question(slf, _) => Some(slf.span),
    }
});
impl_begin_span!(Name, self, {
    match self {
        Name::Str(lit) => Some(lit.span()),
        Name::Parts(parts) => parts.begin_span(),
    }
});
impl_begin_span!(attrs::HtmlAttr, self, { self.name.begin_span() });
impl_begin_span!(Attr, self, {
    match self {
        Attr::Html(slf) => slf.begin_span(),
        Attr::If(slf) => slf.begin_span(),
        Attr::Match(slf) => slf.begin_span(),
        Attr::Scope(slf) => slf.begin_span(),
        Attr::Let(slf) => slf.begin_span(),
        Attr::Spread(slf) => slf.begin_span(),
    }
});
