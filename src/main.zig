const std = @import("std");
const zls = @import("zls");
const scip = @import("scip.zig");
const protobruh = @import("protobruh.zig");
// const StoreToScip = @import("StoreToScip.zig");

const ArgState = enum {
    none,
    add_package_name,
    add_package_path,
    root_name,
    root_path,
};

pub fn main() !void {
    // TODO: Use GPA once memory improves; see issue #1
    const allocator = std.heap.page_allocator;

    var config = zls.Config{};
    var version: ?zls.ZigVersionWrapper = null;

    try zls.configuration.configChanged(&config, &version, allocator, null);

    var server = try zls.Server.create(allocator, &config, null, false, false);
    defer server.destroy();

    const handle = try server.document_store.createDocumentFromPath(try allocator.dupe(u8, "C:/Programming/Zig/scip-zig/src/main.zig"), true);
    const scopes = handle.?.document_scope.scopes;

    const locs = scopes.items(.loc);
    const decls_list = scopes.items(.decls);

    for (locs, decls_list) |loc, decls| {
        var it = decls.iterator();
        while (it.next()) |decl| {
            switch (decl.value_ptr.*) {
                .ast_node => |bruh| {
                    std.log.info("scope {any} | decl {s} | {s}", .{ loc, decl.key_ptr.*, handle.?.tree.getNodeSource(bruh) });
                },
                else => {
                    std.log.info("scope {any} | decl {s} | {any}", .{ loc, decl.key_ptr.*, decl });
                },
            }
        }
    }
}
