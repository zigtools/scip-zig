const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;
const utils = @import("utils.zig");
const DocumentStore = @import("DocumentStore.zig");

const Analyzer = @This();

allocator: std.mem.Allocator,
handle: *DocumentStore.Handle,
scopes: std.ArrayListUnmanaged(Scope) = .{},

pub fn init(analyzer: *Analyzer) !void {
    try analyzer.newContainerScope(0);
}

pub const SourceRange = std.zig.Token.Loc;

pub const Scope = struct {
    pub const Data = union(enum) {
        container: Ast.Node.Index, // .tag is ContainerDecl or Root or ErrorSetDecl
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

pub const Declaration = union(enum) {
    variable: Ast.full.VarDecl,
};

pub fn newContainerScope(analyzer: *Analyzer, node_idx: Ast.Node.Index) !void {
    const tree = analyzer.handle.tree;

    var scope = try analyzer.scopes.addOne(analyzer.allocator);
    scope.* = .{
        .range = nodeSourceRange(tree, node_idx),
        .data = .{ .container = node_idx },
    };
    const scope_idx = analyzer.scopes.items.len - 1;
    _ = scope_idx;

    var buffer: [2]Ast.Node.Index = undefined;
    const members = utils.declMembers(tree, node_idx, &buffer);

    for (members) |member| {
        const name = utils.getDeclName(tree, member) orelse continue;
        if (try analyzer.scopes.items[scope_idx].decls.fetchPut(analyzer.allocator, name, .{ .variable = utils.varDecl(tree, member).? })) |curr| {
            // TODO: Handle this
            _ = curr;
        }

        // std.log.info("{d}", .{member});
    }
}

pub fn scopeIntermediate(analyzer: *Analyzer, scope_idx: usize, node_idx: Ast.Node.Index) !void {
    const tree = analyzer.handle.tree;
    const tags = tree.nodes.items(.tag);

    switch (tags[node_idx]) {
        .global_var_decl,
        .local_var_decl,
        .aligned_var_decl,
        .simple_var_decl,
        => {
            const var_decl = utils.varDecl(tree, node_idx).?;
            if (var_decl.ast.type_node != 0) {
                try scopeIntermediate(analyzer.allocator, scope_idx, var_decl.ast.type_node);
            }

            if (var_decl.ast.init_node != 0) {
                try scopeIntermediate(analyzer.allocator, scope_idx, var_decl.ast.init_node);
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
