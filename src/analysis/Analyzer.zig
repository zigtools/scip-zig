const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;
const scip = @import("../scip.zig");
const utils = @import("utils.zig");
const offsets = @import("offsets.zig");
const DocumentStore = @import("DocumentStore.zig");

const Analyzer = @This();

allocator: std.mem.Allocator,
handle: *DocumentStore.Handle,
scopes: std.ArrayListUnmanaged(Scope) = .{},

/// Occurences recorded at occurence site
recorded_occurrences: std.AutoHashMapUnmanaged(Ast.Node.Index, void) = .{},

symbols: std.ArrayListUnmanaged(scip.SymbolInformation) = .{},
occurrences: std.ArrayListUnmanaged(scip.Occurrence) = .{},

pub fn init(analyzer: *Analyzer) !void {
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
        function: Ast.Node.Index, // .tag is FnProto
        block: Ast.Node.Index, // .tag is Block
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

pub const Declaration = struct {
    node_idx: Ast.Node.Index,
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
        return if (scope.data != .container)
            null
        else
            scope.data.container.descriptor;
    } else return null;
}

pub fn addSymbol(
    analyzer: *Analyzer,
    node_idx: zig.Ast.Node.Index,
    symbol_name: []const u8,
) !void {
    const tree = analyzer.handle.tree;

    try analyzer.recorded_occurrences.put(analyzer.allocator, node_idx, {});

    var comments = try utils.getDocComments(analyzer.allocator, tree, node_idx);
    std.log.info("{s}", .{symbol_name});
    try analyzer.symbols.append(analyzer.allocator, .{
        .symbol = symbol_name,
        .documentation = comments orelse .{},
        .relationships = .{},
    });

    // TODO: Look into weird inclusion of suffix ":"s and " "s
    var range_orig = offsets.tokenToRange(tree, utils.getDeclNameToken(tree, node_idx) orelse @panic("Cannot find decl name token"));
    var range = std.ArrayListUnmanaged(i32){};
    try range.appendSlice(analyzer.allocator, &.{
        @intCast(i32, range_orig.start.line),
        @intCast(i32, range_orig.start.character),
        @intCast(i32, range_orig.end.line),
        @intCast(i32, range_orig.end.character),
    });

    try analyzer.occurrences.append(analyzer.allocator, .{
        .range = range,
        .symbol = symbol_name,
        .symbol_roles = 0x1,
        .override_documentation = .{},
        .syntax_kind = .identifier,
        .diagnostics = .{},
    });
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
        try analyzer.addSymbol(declaration.node_idx, switch (scope.data) {
            .block => switch (declaration.data) {
                .variable => "local testing",
                else => @panic("never gonna let you down"),
            },
            .container => switch (declaration.data) {
                .function => try std.mem.concat(analyzer.allocator, u8, &.{ scope.data.container.descriptor, name, "()." }),
                else => try std.mem.concat(analyzer.allocator, u8, &.{ scope.data.container.descriptor, name, "#" }),
            },
            else => @panic("never gonna give you up."),
        });
    }
}

pub fn newContainerScope(
    analyzer: *Analyzer,
    maybe_parent_scope_idx: ?usize,
    node_idx: Ast.Node.Index,
    scope_name: ?[]const u8,
) !void {
    const tree = analyzer.handle.tree;

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
                        try std.mem.concat(analyzer.allocator, u8, &.{ desc, scope_name orelse @panic("amogus"), "#" })
                    else
                        try std.fmt.allocPrint(analyzer.allocator, "file . {s} unversioned {d} ", .{ analyzer.handle.package, 69 })),
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
                _ = curr;
                @panic("This shouldn't happen!");
            } else {
                try analyzer.addSymbol(member, try std.mem.concat(analyzer.allocator, u8, &.{ analyzer.scopes.items[scope_idx].data.container.descriptor, utils.getDeclName(tree, member) orelse @panic("Cannot create declaration name"), "." }));
            }
        } else {
            try analyzer.scopeIntermediate(scope_idx, member, name);
        }
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
    // const token_tags = tree.tokens.items(.tag);

    switch (tags[node_idx]) {
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

            try analyzer.addDeclaration(scope_idx, .{ .node_idx = node_idx, .data = .{ .variable = var_decl } }, null);

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

            try analyzer.addDeclaration(scope_idx, .{ .node_idx = node_idx, .data = .{ .function = func } }, null);

            const func_scope_name = if (node_idx == 0)
                try std.fmt.allocPrint(analyzer.allocator, "file . {s} unversioned {s}", .{ analyzer.handle.package, analyzer.handle.formatter() })
            else
                (if (analyzer.getDescriptor(scope_idx)) |desc|
                    try std.mem.concat(analyzer.allocator, u8, &.{ desc, scope_name orelse @panic("amogus"), "()." })
                else
                    try std.fmt.allocPrint(analyzer.allocator, "file . {s} unversioned {d} ", .{ analyzer.handle.package, 69 }));

            var scope = try analyzer.scopes.addOne(analyzer.allocator);
            scope.* = .{
                .node_idx = node_idx,
                .parent_scope_idx = scope_idx,
                .range = nodeSourceRange(tree, node_idx),
                .data = .{
                    .container = .{
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
                try analyzer.scopeIntermediate(scope_idx, param.type_expr, scope_name);
            }

            if (fn_tag == .fn_decl) blk: {
                if (data[node_idx].lhs == 0) break :blk;
                const return_type_node = data[data[node_idx].lhs].rhs;

                // Visit the return type
                try analyzer.scopeIntermediate(func_scope_idx, return_type_node, func_scope_name);
            }

            // Visit the function body
            try analyzer.scopeIntermediate(func_scope_idx, data[node_idx].rhs, func_scope_name);
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
            const statements = utils.blockStatements(tree, node_idx, &buffer).?;

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
