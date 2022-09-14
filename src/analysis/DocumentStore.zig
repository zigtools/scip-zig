const std = @import("std");
const Analyzer = @import("Analyzer.zig");

const DocumentStore = @This();

allocator: std.mem.Allocator,
handles: std.StringHashMapUnmanaged(*Handle) = .{},

pub const Handle = struct {
    path: []const u8,
    text: [:0]const u8,
    tree: std.zig.Ast,
    analyzer: Analyzer,
    imports: std.ArrayListUnmanaged([]const u8),
};

pub fn load(store: *DocumentStore, path: []const u8) !*Handle {
    var handle = try store.allocator.create(Handle);
    errdefer store.allocator.destroy(handle);

    var file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    const text = try file.readToEndAllocOptions(
        store.allocator,
        std.math.maxInt(usize),
        null,
        @alignOf(u8),
        0,
    );
    errdefer store.allocator.free(text);

    var tree = try std.zig.parse(store.allocator, text);
    errdefer tree.deinit(store.allocator);

    handle.* = .{
        .path = path,
        .text = text,
        .tree = tree,
        .analyzer = .{ .allocator = store.allocator, .handle = handle },
        .imports = .{},
    };

    try handle.*.analyzer.init();

    return handle;
}

pub fn resolveImport(store: *DocumentStore, handle: *Handle, import: []const u8) ![]const u8 {
    _ = store;
    _ = handle;

    // TODO: Stop doing this hack :(
    if (std.mem.eql(u8, import, "std")) return "C:\\Programming\\Zig\\zig-windows-install\\lib\\std\\std.zig";
    @panic("Import for non-std not implemented");
}

pub fn getOrCreateHandle(store: *DocumentStore, path: []const u8) !*Handle {
    var gop = try store.handles.getOrPut(store.allocator, path);
    return if (gop.found_existing)
        gop.value_ptr.*
    else
        try store.load(path);
}
