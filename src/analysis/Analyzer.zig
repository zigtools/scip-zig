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
        }, // .tag is ContainerDecl or Root or ErrorSetDecl
        function: Ast.Node.Index, // .tag is FnProto
        block: Ast.Node.Index, // .tag is Block
        other,
    };

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
    },
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

pub fn newContainerScope(
    analyzer: *Analyzer,
    maybe_parent_scope_idx: ?usize,
    node_idx: Ast.Node.Index,
    scope_name: ?[]const u8,
) !void {
    const tree = analyzer.handle.tree;

    var scope = try analyzer.scopes.addOne(analyzer.allocator);
    scope.* = .{
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
    _ = scope_idx;

    var buffer: [2]Ast.Node.Index = undefined;
    const members = utils.declMembers(tree, node_idx, &buffer);

    const tags = tree.nodes.items(.tag);

    for (members) |member| {
        const name = utils.getDeclName(tree, member) orelse continue;

        const container_field = switch (tags[member]) {
            .container_field => tree.containerField(member),
            .container_field_align => tree.containerFieldAlign(member),
            .container_field_init => tree.containerFieldInit(member),
            else => null,
        };

        if (container_field == null) {
            const declaration = Declaration{
                .node_idx = member,
                .data = switch (tags[member]) {
                    .global_var_decl,
                    .local_var_decl,
                    .aligned_var_decl,
                    .simple_var_decl,
                    => .{
                        .variable = utils.varDecl(tree, member).?,
                    },
                    .fn_proto,
                    .fn_proto_one,
                    .fn_proto_simple,
                    .fn_proto_multi,
                    .fn_decl,
                    => z: {
                        var buf: [1]Ast.Node.Index = undefined;
                        break :z .{
                            .function = utils.fnProto(tree, member, &buf).?,
                        };
                    },
                    else => @panic("The death of a bachelor"),
                },
            };

            if (try analyzer.scopes.items[scope_idx].decls.fetchPut(
                analyzer.allocator,
                name,
                declaration,
            )) |curr| {
                _ = curr;
                @panic("This shouldn't happen!");
            } else {
                try analyzer.recorded_occurrences.put(analyzer.allocator, member, {});

                const symbol = switch (declaration.data) {
                    .function => try std.mem.concat(analyzer.allocator, u8, &.{ scope.data.container.descriptor, name, "()." }),
                    else => try std.mem.concat(analyzer.allocator, u8, &.{ scope.data.container.descriptor, name, "#" }),
                };

                var comments = try utils.getDocComments(analyzer.allocator, tree, member);
                std.log.info("{s}", .{symbol});
                try analyzer.symbols.append(analyzer.allocator, .{
                    .symbol = symbol,
                    .documentation = comments orelse .{},
                    .relationships = .{},
                });

                // TODO: Look into weird inclusion of suffix ":"s and " "s
                var range_orig = offsets.tokenToRange(tree, utils.getDeclNameToken(tree, member).?);
                var range = std.ArrayListUnmanaged(i32){};
                try range.appendSlice(analyzer.allocator, &.{
                    @intCast(i32, range_orig.start.line),
                    @intCast(i32, range_orig.start.character),
                    @intCast(i32, range_orig.end.line),
                    @intCast(i32, range_orig.end.character),
                });

                try analyzer.occurrences.append(analyzer.allocator, .{
                    .range = range,
                    .symbol = symbol,
                    .symbol_roles = 0x1,
                    .override_documentation = .{},
                    .syntax_kind = .unspecified_syntax_kind,
                    .diagnostics = .{},
                });
            }

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
            if (var_decl.ast.type_node != 0) {
                try analyzer.scopeIntermediate(scope_idx, var_decl.ast.type_node, scope_name);
            }

            if (var_decl.ast.init_node != 0) {
                try analyzer.scopeIntermediate(scope_idx, var_decl.ast.init_node, scope_name);
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
