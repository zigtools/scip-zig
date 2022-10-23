const std = @import("std");
const scip = @import("scip.zig");
const builtin = @import("builtin");
const DocumentStore = @import("analysis/DocumentStore.zig");

const analysis_utils = @import("analysis/utils.zig");

// TODO: Support external type info
pub fn storeToScip(allocator: std.mem.Allocator, store: *DocumentStore, pkg: []const u8) !std.ArrayListUnmanaged(scip.Document) {
    var documents = std.ArrayListUnmanaged(scip.Document){};

    var docit = store.packages.get(pkg).?.handles.iterator();
    while (docit.next()) |entry| {
        var handle = entry.value_ptr.*;
        var document = try documents.addOne(allocator);

        document.* = .{
            .language = "zig",
            .relative_path = try std.mem.replaceOwned(u8, allocator, entry.key_ptr.*, "\\", "/"),
            .occurrences = handle.analyzer.occurrences,
            .symbols = handle.analyzer.symbols,
        };
    }

    return documents;
}
