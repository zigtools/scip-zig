const std = @import("std");
const scip = @import("scip.zig");
const protobruh = @import("protobruh.zig");
const StoreToScip = @import("StoreToScip.zig");
const DocumentStore = @import("analysis/DocumentStore.zig");

pub fn main() !void {
    var doc_store = DocumentStore{ .allocator = std.heap.page_allocator };
    // try doc_store.createPackage("root", "C:\\Programming\\Zig\\scip-zig\\test\\luuk.zig");
    try doc_store.createPackage("std", "C:\\Programming\\Zig\\zig-windows-install\\lib\\std\\std.zig");
    // _ = try doc_store.load(big_loris);

    // _ = try doc_store.getOrCreateHandle(big_loris);
    // var iterator = the_big_loris.analyzer.scopes.items[0].decls.iterator();

    // while (iterator.next()) |entry|
    //     std.log.info("{s}: {any}", .{ entry.key_ptr.*, entry.value_ptr.* });

    var my_test_index = try std.fs.cwd().createFile("my_test_index.scip", .{ .read = true });

    var documents = try StoreToScip.storeToScip(std.heap.page_allocator, &doc_store);

    std.log.info("{any}", .{documents});

    try protobruh.encode(scip.Index{
        .metadata = .{
            .version = .unspecified_protocol_version,
            .tool_info = .{
                .name = "scip-zig",
                .version = "unversioned",
                .arguments = .{},
            },
            // .project_root = "file:///mnt/c/Programming/Zig/scip-zig/test",
            .project_root = "file:///mnt/c/Programming/Zig/zig-windows-install/lib/std",
            .text_document_encoding = .utf8,
        },
        .documents = documents,
        .external_symbols = .{},
    }, my_test_index.writer());

    my_test_index.close();

    var my_test_index2 = try std.fs.cwd().openFile("my_test_index.scip", .{});

    var z = try protobruh.decode(scip.Index, std.heap.page_allocator, my_test_index2.reader());
    std.log.info("AAA: {any}", .{z.documents.items});

    // for (z.documents.items) |docu| {
    //     for (docu.symbols.items) |symbi| {
    //         try std.io.getStdOut().writer().print("{s}\n", .{symbi.symbol});
    //     }
    // }
}
