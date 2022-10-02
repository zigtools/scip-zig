const std = @import("std");
const Analyzer = @import("Analyzer.zig");

const logger = std.log.scoped(.store);

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
    document_store: *DocumentStore,
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
    if (store.packages.contains(package)) return;

    try store.packages.put(store.allocator, try store.allocator.dupe(u8, package), .{
        .root = try store.allocator.dupe(u8, root),
    });

    _ = try store.loadFile(package, std.fs.path.basename(root));
}

pub fn loadFile(store: *DocumentStore, package: []const u8, path: []const u8) !*Handle {
    std.log.info("Loading {s}", .{path});
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
        .document_store = store,
        .package = package_entry.key_ptr.*,
        .path = path_duped,
        .text = text,
        .tree = tree,
        .analyzer = .{ .allocator = store.allocator, .handle = handle },
    };

    try store.packages.getEntry(package).?.value_ptr.handles.put(store.allocator, path_duped, handle);

    try handle.analyzer.init();

    return handle;
}

pub fn getOrLoadFile(store: *DocumentStore, package: []const u8, path: []const u8) anyerror!*Handle {
    return store.packages.get(package).?.handles.get(path) orelse store.loadFile(package, path);
}

pub fn resolveImportHandle(store: *DocumentStore, handle: *Handle, import: []const u8) anyerror!?*Handle {
    if (std.mem.endsWith(u8, import, ".zig")) {
        var rel = try std.fs.path.resolve(store.allocator, &[_][]const u8{ std.fs.path.dirname(store.packages.get(handle.package).?.root).?, handle.path, "..", import });
        defer store.allocator.free(rel);

        // logger.info("Importing {s} @ {s} (basename {?s}, rel {s}) within package {s}", .{ import, handle.path, std.fs.path.dirname(handle.path), rel, handle.package });

        return try store.getOrLoadFile(handle.package, rel[std.fs.path.dirname(store.packages.get(handle.package).?.root).?.len + 1 ..]);
    } else {
        // TODO: Custom packages

        if (std.mem.eql(u8, import, "std")) {
            try store.createPackage("std", "C:\\Programming\\Zig\\zig-windows-install\\lib\\std\\std.zig");
            return store.packages.get("std").?.handles.get("std.zig").?;
        }

        return null;
    }
    //return store.packages.getEntry(import).?.value_ptr.handles.get(key: K)
}

// pub fn resolveImportPath(store: *DocumentStore, handle: *Handle, import: []const u8) ![]const u8 {
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
