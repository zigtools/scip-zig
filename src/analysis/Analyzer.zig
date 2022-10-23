const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;
const scip = @import("../scip.zig");
const utils = @import("utils.zig");
const offsets = @import("offsets.zig");
const DocumentStore = @import("DocumentStore.zig");

const logger = std.log.scoped(.analyzer);

const Analyzer = @This();

allocator: std.mem.Allocator,
handle: *DocumentStore.Handle,
scopes: std.ArrayListUnmanaged(Scope) = .{},

/// Occurrences recorded at occurrence site
recorded_occurrences: std.AutoHashMapUnmanaged(Ast.TokenIndex, void) = .{},

symbols: std.ArrayListUnmanaged(scip.SymbolInformation) = .{},
occurrences: std.ArrayListUnmanaged(scip.Occurrence) = .{},

local_counter: usize = 0,

post_resolves: std.ArrayListUnmanaged(PostResolve) = .{},
const PostResolve = struct { scope_idx: usize, node_idx: Ast.Node.Index };

// TODO: Make scope map to avoid scope duplication, decrease lookup times

pub fn init(analyzer: *Analyzer) !void {
    logger.info("Initializing file {s}", .{analyzer.handle.path});
    try analyzer.newContainerScope(null, 0, "root");
}

pub const SourceRange = std.zig.Token.Loc;

pub const Scope = struct {
    pub const Data = union(enum) {
        container: struct {
            descriptor: []const u8,
            node_idx: Ast.Node.Index,
            fields: std.StringHashMapUnmanaged(Field) = .{},
        }, // .tag is ContainerDecl or Root or ErrorSetDecl
        function: struct {
            descriptor: []const u8,
            node_idx: Ast.Node.Index,
        }, // .tag is FnProto
        block: Ast.Node.Index, // .tag is Block
        // TODO: Is this really the most efficient way?
        import: []const u8,
        other,
    };

    node_idx: zig.Ast.Node.Index,
    parent_scope_idx: ?usize,
    range: SourceRange,
    decls: std.StringHashMapUnmanaged(Declaration) = .{},
    // tests: std.ArrayListUnmanaged(Ast.Node.Index) = .{},
    children: std.ArrayListUnmanaged(Ast.Node.Index) = .{},
    data: Data,

    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        self.decls.deinit(allocator);
        // self.tests.deinit(allocator);
        self.children.deinit(allocator);
    }

    pub fn toNodeIndex(self: Scope) ?Ast.Node.Index {
        return switch (self.data) {
            .container, .function, .block => |idx| idx,
            else => null,
        };
    }
};

// pub const ResolveAndMarkResult = struct {
//     analyzer: *Analyzer,
//     scope_idx: usize,
//     declaration: ?Declaration = null,
// };

pub const TrueScopeIndexResult = struct {
    /// Analyzer where scope is located
    analyzer: *Analyzer,
    /// Scope
    scope_idx: usize,
};

pub fn resolveTrueScopeIndex(
    analyzer: *Analyzer,
    scope_idx: usize,
) anyerror!?TrueScopeIndexResult {
    const scope = analyzer.scopes.items[scope_idx];
    return switch (scope.data) {
        .import => |i| TrueScopeIndexResult{
            // NOTE: This really seems dangerous... but it works so like can't complain (yet)
            .analyzer = &((try analyzer.handle.document_store.resolveImportHandle(analyzer.handle, i)) orelse return null).analyzer,
            .scope_idx = 0,
        },
        else => TrueScopeIndexResult{ .analyzer = analyzer, .scope_idx = scope_idx },
    };
}

pub const DeclarationWithAnalyzer = struct {
    analyzer: *Analyzer,
    declaration: ?Declaration = null,
    scope_idx: usize,
};

pub fn getDeclFromScopeByName(
    analyzer: *Analyzer,
    scope_idx: usize,
    name: []const u8,
) anyerror!DeclarationWithAnalyzer {
    var ts = (try analyzer.resolveTrueScopeIndex(scope_idx)) orelse return DeclarationWithAnalyzer{ .analyzer = analyzer, .scope_idx = scope_idx };
    if (ts.analyzer.scopes.items.len == 0) return DeclarationWithAnalyzer{ .analyzer = ts.analyzer, .scope_idx = ts.scope_idx };
    return DeclarationWithAnalyzer{
        .analyzer = ts.analyzer,
        .declaration = ts.analyzer.scopes.items[ts.scope_idx].decls.get(name),
        .scope_idx = ts.scope_idx,
    };
}

pub fn formatSubSymbol(analyzer: *Analyzer, symbol: []const u8) []const u8 {
    _ = analyzer;
    return if (std.mem.startsWith(u8, symbol, "@\"")) symbol[2 .. symbol.len - 1] else symbol;
}

pub fn resolveAndMarkDeclarationIdentifier(
    analyzer: *Analyzer,
    foreign_analyzer: *Analyzer,
    scope_idx: usize,
    token_idx: Ast.TokenIndex,
) anyerror!DeclarationWithAnalyzer {
    const tree = analyzer.handle.tree;
    // const scope = analyzer.scopes.items[scope_idx];

    var dwa = try foreign_analyzer.getDeclFromScopeByName(scope_idx, tree.tokenSlice(token_idx));
    if (dwa.declaration == null)
        dwa =
            if (dwa.scope_idx > foreign_analyzer.scopes.items.len)
            return DeclarationWithAnalyzer{ .analyzer = foreign_analyzer, .scope_idx = scope_idx }
        else r: {
            const maybe_rtsi = try foreign_analyzer.resolveTrueScopeIndex(scope_idx);
            if (maybe_rtsi) |rtsi| {
                if (rtsi.analyzer.scopes.items[rtsi.scope_idx].parent_scope_idx) |psi| {
                    break :r try analyzer.resolveAndMarkDeclarationIdentifier(rtsi.analyzer, psi, token_idx);
                }
            }

            return DeclarationWithAnalyzer{ .analyzer = foreign_analyzer, .scope_idx = if (maybe_rtsi) |m| m.scope_idx else 0 };
        };

    if (dwa.declaration) |decl| {
        if ((try analyzer.recorded_occurrences.fetchPut(analyzer.allocator, token_idx, {})) == null) {
            try analyzer.occurrences.append(analyzer.allocator, .{
                .range = analyzer.rangeArray(token_idx),
                .symbol = decl.symbol,
                .symbol_roles = 0,
                .override_documentation = .{},
                .syntax_kind = .identifier,
                .diagnostics = .{},
            });
        }
    }

    return dwa;
}

pub fn resolveAndMarkDeclarationComplex(
    analyzer: *Analyzer,
    foreign_analyzer: *Analyzer,
    scope_idx: usize,
    node_idx: Ast.Node.Index,
) anyerror!DeclarationWithAnalyzer {
    const tree = analyzer.handle.tree;
    const tags = tree.nodes.items(.tag);
    const data = tree.nodes.items(.data);

    if (node_idx >= tags.len) std.log.err("BRUH {d}", .{node_idx});
    return switch (tags[node_idx]) {
        .identifier => analyzer.resolveAndMarkDeclarationIdentifier(foreign_analyzer, scope_idx, tree.nodes.items(.main_token)[node_idx]),
        .field_access => {
            const curr_name_idx = data[node_idx].rhs;
            const prev_node_idx = data[node_idx].lhs;

            // const scope_decl = () orelse return .{ .analyzer = analyzer };
            var result = try analyzer.resolveAndMarkDeclarationComplex(foreign_analyzer, scope_idx, prev_node_idx);
            if (result.declaration) |decl|
                switch (result.analyzer.handle.tree.nodes.items(.tag)[decl.node_idx]) {
                    .global_var_decl,
                    .local_var_decl,
                    .aligned_var_decl,
                    .simple_var_decl,
                    => {
                        const var_decl = utils.varDecl(result.analyzer.handle.tree, decl.node_idx).?;

                        switch (result.analyzer.handle.tree.nodes.items(.tag)[var_decl.ast.init_node]) {
                            .identifier, .field_access => {
                                result = try result.analyzer.resolveAndMarkDeclarationComplex(result.analyzer, result.scope_idx, var_decl.ast.init_node);
                            },
                            else => {},
                        }
                    },
                    else => {},
                };
            if (result.declaration) |scope_decl| {
                for (result.analyzer.scopes.items) |scope, idx| {
                    if (scope.node_idx == scope_decl.data.variable.ast.init_node) {
                        const maybe_decl = try analyzer.resolveAndMarkDeclarationIdentifier(result.analyzer, idx, curr_name_idx);
                        if (maybe_decl.declaration == null) logger.warn("Lookup failure while searching for {s} from {s}", .{ tree.getNodeSource(node_idx), analyzer.handle.path });
                        return maybe_decl;
                    }
                }
            }

            return DeclarationWithAnalyzer{ .analyzer = analyzer, .scope_idx = scope_idx };
        },
        else => {
            // logger.info("HUH! {any}", .{tags[node_idx]});
            return DeclarationWithAnalyzer{ .analyzer = analyzer, .scope_idx = scope_idx };
        },
    };
}

pub const Declaration = struct {
    node_idx: Ast.Node.Index,
    symbol: []const u8 = "",
    data: union(enum) {
        none,
        function: Ast.full.FnProto,
        variable: Ast.full.VarDecl,
        param: Ast.full.FnProto.Param,
    },
};

pub const Field = struct {
    node_idx: Ast.Node.Index,
    data: Ast.full.ContainerField,
};

pub fn getDescriptor(analyzer: *Analyzer, maybe_scope_idx: ?usize) ?[]const u8 {
    if (maybe_scope_idx) |scope_idx| {
        var scope = analyzer.scopes.items[scope_idx];
        return switch (scope.data) {
            .container => |container| container.descriptor,
            .function => |function| function.descriptor,
            else => null,
        };
    } else return null;
}

pub fn rangeArray(analyzer: *Analyzer, token: zig.Ast.TokenIndex) [4]i32 {
    var range_orig = offsets.tokenToRange(analyzer.handle.tree, token);
    return [4]i32{
        @intCast(i32, range_orig.start.line),
        @intCast(i32, range_orig.start.character),
        @intCast(i32, range_orig.end.line),
        @intCast(i32, range_orig.end.character),
    };
}

pub fn addSymbol(
    analyzer: *Analyzer,
    node_idx: zig.Ast.Node.Index,
    symbol_name: []const u8,
) !void {
    const tree = analyzer.handle.tree;
    const name_token = utils.getDeclNameToken(tree, node_idx) orelse @panic("Cannot find decl name token");

    if (try analyzer.recorded_occurrences.fetchPut(analyzer.allocator, name_token, {})) |_| {
        logger.err("Encountered reoccuring entry symbol {s} @ token {d}", .{ symbol_name, name_token });
        // @panic("Reoccuring entry!");
        return error.Reocc;
        // return;
    }

    var comments = try utils.getDocComments(analyzer.allocator, tree, node_idx);
    // logger.info("{s}", .{symbol_name});
    try analyzer.symbols.append(analyzer.allocator, .{
        .symbol = symbol_name,
        .documentation = comments orelse .{},
        .relationships = .{},
    });

    try analyzer.occurrences.append(analyzer.allocator, .{
        .range = analyzer.rangeArray(name_token),
        .symbol = symbol_name,
        .symbol_roles = 0x1,
        .override_documentation = .{},
        .syntax_kind = .identifier,
        .diagnostics = .{},
    });
}

pub fn generateSymbol(
    analyzer: *Analyzer,
    scope_idx: usize,
    declaration: Declaration,
    name: []const u8,
) ![]const u8 {
    const scope = &analyzer.scopes.items[scope_idx];

    return switch (scope.data) {
        .block => switch (declaration.data) {
            .variable => v: {
                analyzer.local_counter += 1;
                break :v try std.fmt.allocPrint(analyzer.allocator, "local {d}", .{analyzer.local_counter});
            },
            else => @panic("never gonna let you down"),
        },
        .container => switch (declaration.data) {
            .function => try std.mem.concat(analyzer.allocator, u8, &.{ scope.data.container.descriptor, "`", analyzer.formatSubSymbol(name), "`", "()." }),
            else => try std.mem.concat(analyzer.allocator, u8, &.{ scope.data.container.descriptor, "`", analyzer.formatSubSymbol(name), "`", "#" }),
        },
        else => @panic("never gonna give you up."),
    };
}

pub fn addDeclaration(
    analyzer: *Analyzer,
    scope_idx: usize,
    declaration: Declaration,
    /// If name is not specified, default will method will be used
    provided_name: ?[]const u8,
) !void {
    const name = provided_name orelse utils.getDeclName(analyzer.handle.tree, declaration.node_idx) orelse @panic("Cannot find decl name for declaration");
    const scope = &analyzer.scopes.items[scope_idx];

    if (try scope.decls.fetchPut(
        analyzer.allocator,
        name,
        declaration,
    )) |curr| {
        _ = curr;
        @panic("This shouldn't happen!");
    } else {
        try analyzer.addSymbol(declaration.node_idx, declaration.symbol);
    }
}

pub fn newContainerScope(
    analyzer: *Analyzer,
    maybe_parent_scope_idx: ?usize,
    node_idx: Ast.Node.Index,
    scope_name: ?[]const u8,
) !void {
    const tree = analyzer.handle.tree;

    for (tree.nodes.items(.tag)) |tag, i| {
        switch (tag) {
            .builtin_call,
            .builtin_call_comma,
            .builtin_call_two,
            .builtin_call_two_comma,
            => {
                var buffer: [2]Ast.Node.Index = undefined;
                const params = utils.builtinCallParams(tree, @intCast(Ast.Node.Index, i), &buffer).?;

                if (std.mem.eql(u8, tree.tokenSlice(tree.nodes.items(.main_token)[i]), "@import")) {
                    if (params.len == 0) continue;
                    const import_param = params[0];
                    if (tree.nodes.items(.tag)[import_param] != .string_literal) continue;

                    const import_str = tree.tokenSlice(tree.nodes.items(.main_token)[import_param]);
                    _ = try analyzer.handle.document_store.resolveImportHandle(analyzer.handle, import_str[1 .. import_str.len - 1]);
                }
            },
            else => {},
        }
    }

    var scope = try analyzer.scopes.addOne(analyzer.allocator);
    scope.* = .{
        .node_idx = node_idx,
        .parent_scope_idx = maybe_parent_scope_idx,
        .range = nodeSourceRange(tree, node_idx),
        .data = .{
            .container = .{
                .descriptor = if (node_idx == 0)
                    try std.fmt.allocPrint(analyzer.allocator, "file . {s} unversioned {s}", .{ analyzer.handle.package, analyzer.handle.formatter() })
                else
                    (if (analyzer.getDescriptor(maybe_parent_scope_idx)) |desc|
                        try std.mem.concat(analyzer.allocator, u8, &.{ desc, "`", analyzer.formatSubSymbol(scope_name orelse @panic("amogus")), "`", "#" })
                    else
                        try std.fmt.allocPrint(analyzer.allocator, "file . {s} unversioned ", .{analyzer.handle.package})),
                .node_idx = node_idx,
            },
        },
    };

    const scope_idx = analyzer.scopes.items.len - 1;

    var buffer: [2]Ast.Node.Index = undefined;
    const members = utils.declMembers(tree, node_idx, &buffer);

    const tags = tree.nodes.items(.tag);

    for (members) |member| {
        const name = utils.getDeclName(tree, member) orelse continue;

        const maybe_container_field: ?zig.Ast.full.ContainerField = switch (tags[member]) {
            .container_field => tree.containerField(member),
            .container_field_align => tree.containerFieldAlign(member),
            .container_field_init => tree.containerFieldInit(member),
            else => null,
        };

        if (maybe_container_field) |container_field| {
            const field = Field{
                .node_idx = member,
                .data = container_field,
            };

            if (try analyzer.scopes.items[scope_idx].data.container.fields.fetchPut(
                analyzer.allocator,
                name,
                field,
            )) |curr| {
                std.log.info("Duplicate field, handling regardless: {any}", .{curr});
            } else {
                try analyzer.addSymbol(member, try std.mem.concat(analyzer.allocator, u8, &.{ analyzer.scopes.items[scope_idx].data.container.descriptor, analyzer.formatSubSymbol(utils.getDeclName(tree, member) orelse @panic("Cannot create declaration name")), "." }));
            }
        } else {
            try analyzer.scopeIntermediate(scope_idx, member, name);
        }
    }
}

pub fn postResolves(analyzer: *Analyzer) !void {
    for (analyzer.post_resolves.items) |pr| {
        _ = try analyzer.resolveAndMarkDeclarationComplex(analyzer, pr.scope_idx, pr.node_idx);
    }
}

pub fn scopeIntermediate(
    analyzer: *Analyzer,
    scope_idx: usize,
    node_idx: Ast.Node.Index,
    scope_name: ?[]const u8,
) anyerror!void {
    const tree = analyzer.handle.tree;
    const tags = tree.nodes.items(.tag);
    const data = tree.nodes.items(.data);
    const main_tokens = tree.nodes.items(.main_token);
    // const token_tags = tree.tokens.items(.tag);

    // logger.info("{any}", .{tags[node_idx]});

    // std.log.info("BBBBBBBBBBBBB {d}, {d}", .{ analyzer.scopes.items.len, analyzer.scopes.items[analyzer.scopes.items.len - 1].node_idx });

    if (analyzer.scopes.items.len != 1 and analyzer.scopes.items[analyzer.scopes.items.len - 1].node_idx == 0) return error.abc;

    switch (tags[node_idx]) {
        // .identifier => {
        // _ = try analyzer.resolveAndMarkDeclarationIdentifier(analyzer, scope_idx, main_tokens[node_idx]);
        // },
        .identifier, .field_access => {
            try analyzer.post_resolves.append(analyzer.allocator, .{ .scope_idx = scope_idx, .node_idx = node_idx });
            // _ = try analyzer.resolveAndMarkDeclarationComplex(analyzer, scope_idx, node_idx);
        },
        .container_decl,
        .container_decl_trailing,
        .container_decl_arg,
        .container_decl_arg_trailing,
        .container_decl_two,
        .container_decl_two_trailing,
        .tagged_union,
        .tagged_union_trailing,
        .tagged_union_two,
        .tagged_union_two_trailing,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        .root,
        .error_set_decl,
        => {
            try analyzer.newContainerScope(scope_idx, node_idx, scope_name);
        },
        .global_var_decl,
        .local_var_decl,
        .aligned_var_decl,
        .simple_var_decl,
        => {
            const var_decl = utils.varDecl(tree, node_idx).?;

            var decl = Declaration{
                .node_idx = node_idx,
                .data = .{ .variable = var_decl },
            };

            decl.symbol = try analyzer.generateSymbol(scope_idx, decl, utils.getDeclName(tree, node_idx).?);

            try analyzer.addDeclaration(scope_idx, decl, null);

            if (var_decl.ast.type_node != 0) {
                try analyzer.scopeIntermediate(scope_idx, var_decl.ast.type_node, scope_name);
            }

            if (var_decl.ast.init_node != 0) {
                try analyzer.scopeIntermediate(scope_idx, var_decl.ast.init_node, scope_name);
            }
        },
        .fn_proto,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_proto_multi,
        .fn_decl,
        => |fn_tag| {
            var buf: [1]Ast.Node.Index = undefined;
            const func = utils.fnProto(tree, node_idx, &buf) orelse @panic("Cannot create fnProto");

            var decl = Declaration{
                .node_idx = node_idx,
                .data = .{ .function = func },
            };
            decl.symbol = try analyzer.generateSymbol(scope_idx, decl, utils.getDeclName(analyzer.handle.tree, node_idx) orelse return);

            try analyzer.addDeclaration(scope_idx, decl, null);

            const func_scope_name = if (node_idx == 0)
                try std.fmt.allocPrint(analyzer.allocator, "file . {s} unversioned {s}", .{ analyzer.handle.package, analyzer.handle.formatter() })
            else
                (if (analyzer.getDescriptor(scope_idx)) |desc|
                    try std.mem.concat(analyzer.allocator, u8, &.{ desc, analyzer.formatSubSymbol(scope_name orelse @panic("amogus")), "()." })
                else
                    try std.fmt.allocPrint(analyzer.allocator, "file . {s} unversioned {d} ", .{ analyzer.handle.package, 69 }));

            var scope = try analyzer.scopes.addOne(analyzer.allocator);
            scope.* = .{
                .node_idx = node_idx,
                .parent_scope_idx = scope_idx,
                .range = nodeSourceRange(tree, node_idx),
                .data = .{
                    .function = .{
                        .descriptor = func_scope_name,
                        .node_idx = node_idx,
                    },
                },
            };

            const func_scope_idx = analyzer.scopes.items.len - 1;

            var it = func.iterate(&tree);
            while (it.next()) |param| {
                // Add parameter decls
                if (param.name_token) |name_token| {
                    _ = name_token;
                    // try analyzer.addDeclaration(scope_idx, .{ .node_idx = node_idx, .data = .{ .param = param } }, tree.tokenSlice(name_token));
                }
                // Visit parameter types to pick up any error sets and enum
                //   completions
                if (param.type_expr != 0)
                    try analyzer.scopeIntermediate(scope_idx, param.type_expr, scope_name);
            }

            // TODO: Fix scoping issue here
            // _ = fn_tag;
            if (fn_tag == .fn_decl) blk: {
                if (data[node_idx].lhs == 0) break :blk;
                const return_type_node = data[data[node_idx].lhs].rhs;

                // Visit the return type
                if (return_type_node != 0)
                    try analyzer.scopeIntermediate(func_scope_idx, return_type_node, scope_name);
            }

            // Visit the function body
            if (data[node_idx].rhs != 0)
                try analyzer.scopeIntermediate(func_scope_idx, data[node_idx].rhs, scope_name);
        },
        .block,
        .block_semicolon,
        .block_two,
        .block_two_semicolon,
        => {
            const first_token = tree.firstToken(node_idx);
            const last_token = utils.lastToken(tree, node_idx);

            _ = first_token;
            _ = last_token;

            // if labeled block
            // if (token_tags[first_token] == .identifier) {
            //     const scope = try scopes.addOne(allocator);
            //     scope.* = .{
            //         .loc = .{
            //             .start = offsets.tokenToIndex(tree, main_tokens[node_idx]),
            //             .end = offsets.tokenToLoc(tree, last_token).start,
            //         },
            //         .data = .other,
            //     };
            //     // TODO: try scope.decls.putNoClobber(allocator, tree.tokenSlice(first_token), .{ .label_decl = first_token });
            // }

            var scope = try analyzer.scopes.addOne(analyzer.allocator);
            scope.* = .{
                .node_idx = node_idx,
                .parent_scope_idx = scope_idx,
                .range = nodeSourceRange(tree, node_idx),
                .data = .{
                    .block = node_idx,
                },
            };

            const block_scope_idx = analyzer.scopes.items.len - 1;

            var buffer: [2]Ast.Node.Index = undefined;
            const statements = utils.blockStatements(tree, node_idx, &buffer) orelse return;

            for (statements) |idx| {
                // TODO:
                // if (tags[idx] == .@"usingnamespace") {
                //     try scopes.items[scope_idx].uses.append(allocator, idx);
                //     continue;
                // }

                try analyzer.scopeIntermediate(block_scope_idx, idx, null);
            }

            return;
        },
        .call,
        .call_comma,
        .call_one,
        .call_one_comma,
        .async_call,
        .async_call_comma,
        .async_call_one,
        .async_call_one_comma,
        => {
            var buf: [1]Ast.Node.Index = undefined;
            const call = utils.callFull(tree, node_idx, &buf) orelse return;

            if (call.ast.fn_expr != 0)
                try analyzer.scopeIntermediate(scope_idx, call.ast.fn_expr, scope_name);
            for (call.ast.params) |param|
                if (param != 0)
                    try analyzer.scopeIntermediate(scope_idx, param, scope_name);
        },
        .equal_equal,
        .bang_equal,
        .less_than,
        .greater_than,
        .less_or_equal,
        .greater_or_equal,
        .assign_mul,
        .assign_div,
        .assign_mod,
        .assign_add,
        .assign_sub,
        .assign_shl,
        .assign_shr,
        .assign_bit_and,
        .assign_bit_xor,
        .assign_bit_or,
        .assign_mul_wrap,
        .assign_add_wrap,
        .assign_sub_wrap,
        .assign_mul_sat,
        .assign_add_sat,
        .assign_sub_sat,
        .assign_shl_sat,
        .assign,
        .merge_error_sets,
        .mul,
        .div,
        .mod,
        .array_mult,
        .mul_wrap,
        .mul_sat,
        .add,
        .sub,
        .array_cat,
        .add_wrap,
        .sub_wrap,
        .add_sat,
        .sub_sat,
        .shl,
        .shl_sat,
        .shr,
        .bit_and,
        .bit_xor,
        .bit_or,
        .@"orelse",
        .bool_and,
        .bool_or,
        .array_type,
        .array_access,
        .error_union,
        => {
            if (data[node_idx].lhs != 0)
                try analyzer.scopeIntermediate(scope_idx, data[node_idx].lhs, scope_name);
            if (data[node_idx].rhs != 0)
                try analyzer.scopeIntermediate(scope_idx, data[node_idx].rhs, scope_name);
        },
        .@"return",
        .@"resume",
        .@"suspend",
        .deref,
        .@"try",
        .@"await",
        .optional_type,
        .@"comptime",
        .@"nosuspend",
        .bool_not,
        .negation,
        .bit_not,
        .negation_wrap,
        .address_of,
        .grouped_expression,
        .unwrap_optional,
        .@"usingnamespace",
        => {
            if (data[node_idx].lhs != 0)
                try analyzer.scopeIntermediate(scope_idx, data[node_idx].lhs, scope_name);
        },
        .@"if",
        .if_simple,
        => {
            // TODO: Handle payload, create scopes
            const if_node = utils.ifFull(tree, node_idx);

            if (if_node.ast.cond_expr != 0)
                try analyzer.scopeIntermediate(scope_idx, if_node.ast.cond_expr, scope_name);

            try analyzer.scopeIntermediate(scope_idx, if_node.ast.then_expr, scope_name);
            if (if_node.ast.else_expr != 0)
                try analyzer.scopeIntermediate(scope_idx, if_node.ast.else_expr, scope_name);
        },
        .@"while",
        .while_simple,
        .while_cont,
        .@"for",
        .for_simple,
        => {
            // TODO: Handle payload, create scopes

            const while_node = utils.whileAst(tree, node_idx).?;
            const is_for = tags[node_idx] == .@"for" or tags[node_idx] == .for_simple;
            _ = is_for;

            if (while_node.ast.cond_expr != 0)
                try analyzer.scopeIntermediate(scope_idx, while_node.ast.cond_expr, scope_name);
            try analyzer.scopeIntermediate(scope_idx, while_node.ast.then_expr, scope_name);
            if (while_node.ast.else_expr != 0)
                try analyzer.scopeIntermediate(scope_idx, while_node.ast.else_expr, scope_name);
        },
        .builtin_call,
        .builtin_call_comma,
        .builtin_call_two,
        .builtin_call_two_comma,
        => {
            var buffer: [2]Ast.Node.Index = undefined;
            const params = utils.builtinCallParams(tree, node_idx, &buffer).?;
            const call_name = tree.tokenSlice(main_tokens[node_idx]);

            for (params) |p|
                try analyzer.scopeIntermediate(scope_idx, p, scope_name);

            if (std.mem.eql(u8, call_name, "@import")) {
                const import_param = params[0];
                const import_str = tree.tokenSlice(main_tokens[import_param]);

                try analyzer.scopes.append(analyzer.allocator, .{
                    .node_idx = node_idx,
                    .parent_scope_idx = scope_idx,
                    .range = nodeSourceRange(tree, node_idx),
                    .data = .{
                        .import = import_str[1 .. import_str.len - 1],
                    },
                });

                // _ = analyzer.resolveAndMarkDeclarationComplex(analyzer, scope_idx, node_idx);
                // _ = try analyzer.handle.document_store.resolveImportHandle(analyzer.handle, import_str[1 .. import_str.len - 1]);
            }
        },
        else => {},
    }
}

fn nodeSourceRange(tree: Ast, node: Ast.Node.Index) SourceRange {
    const loc_start = utils.tokenLocation(tree, tree.firstToken(node));
    const loc_end = utils.tokenLocation(tree, tree.lastToken(node));

    return SourceRange{
        .start = loc_start.start,
        .end = loc_end.end,
    };
}
