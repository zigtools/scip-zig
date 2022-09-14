const std = @import("std");
const DocumentStore = @import("analysis/DocumentStore.zig");

pub fn main() !void {
    const big_loris = "C:\\Programming\\Zig\\scip-zig\\test\\loris.zig";

    var doc_store = DocumentStore{ .allocator = std.heap.page_allocator };
    _ = try doc_store.load(big_loris);

    const the_big_loris = try doc_store.getOrCreateHandle(big_loris);
    var iterator = the_big_loris.analyzer.scopes.items[0].decls.iterator();

    while (iterator.next()) |entry|
        std.log.info("{s}: {any}", .{ entry.key_ptr.*, entry.value_ptr.* });
}
