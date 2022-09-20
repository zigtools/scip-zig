const std = @import("std");
const Analyzer = @import("Analyzer.zig");

const DocumentStore = @This();

allocator: std.mem.Allocator,
/// Root -> Package
packages: std.StringHashMapUnmanaged(Package) = .{},

pub const Package = struct {
    root: []const u8,
    /// Relative path -> Handle
    handles: std.StringHashMapUnmanaged(*Handle) = .{},
};

pub const Handle = struct {
    package: []const u8,
    /// Relative to package root
    path: []const u8,
    text: [:0]const u8,
    tree: std.zig.Ast,
    analyzer: Analyzer,

    pub const PathFormatter = struct {
        handle: Handle,

        pub fn format(
            self: PathFormatter,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            var splitit = std.mem.split(u8, self.handle.path, &.{std.fs.path.sep});
            while (splitit.next()) |segment| {
                if (std.mem.indexOfAny(u8, segment, ".") != null)
                    try writer.print("`{s}`", .{segment})
                else
                    try writer.writeAll(segment);

                try writer.writeByte('/');
            }
        }
    };

    pub fn formatter(handle: Handle) PathFormatter {
        return .{ .handle = handle };
    }
};

pub fn createPackage(store: *DocumentStore, package: []const u8, root: []const u8) !void {
    try store.packages.put(store.allocator, package, .{
        .root = try store.allocator.dupe(u8, root),
    });

    _ = try store.loadFile(package, std.fs.path.basename(root));
}

pub fn loadFile(store: *DocumentStore, package: []const u8, path: []const u8) !*Handle {
    std.debug.assert(!std.fs.path.isAbsolute(path)); // use relative path

    const package_entry = store.packages.getEntry(package).?;
    const path_duped = try store.allocator.dupe(u8, path);

    var concat_path = try std.fs.path.join(store.allocator, &.{ std.fs.path.dirname(package_entry.value_ptr.*.root).?, path });
    defer store.allocator.free(concat_path);

    var file = try std.fs.openFileAbsolute(concat_path, .{});
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

    var handle = try store.allocator.create(Handle);
    errdefer store.allocator.destroy(handle);

    handle.* = .{
        .package = package_entry.key_ptr.*,
        .path = path,
        .text = text,
        .tree = tree,
        .analyzer = .{ .allocator = store.allocator, .handle = handle },
    };

    try handle.analyzer.init();

    try store.packages.getEntry(package).?.value_ptr.handles.put(store.allocator, path_duped, handle);

    return handle;
}

// pub fn resolveImport(store: *DocumentStore, handle: *Handle, import: []const u8) ![]const u8 {
//     _ = store;
//     _ = handle;

//     // TODO: Stop doing this hack :(
//     if (std.mem.eql(u8, import, "std")) return "C:\\Programming\\Zig\\zig-windows-install\\lib\\std\\std.zig";
//     @panic("Import for non-std not implemented");
// }

// pub fn getOrCreateHandle(store: *DocumentStore, path: []const u8) !*Handle {
//     var gop = try store.handles.getOrPut(store.allocator, try store.allocator.dupe(u8, path));
//     return if (gop.found_existing)
//         gop.value_ptr.*
//     else
//         try store.load(path);
// }
