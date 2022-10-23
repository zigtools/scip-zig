const std = @import("std");
const scip = @import("scip.zig");
const protobruh = @import("protobruh.zig");
const StoreToScip = @import("StoreToScip.zig");
const DocumentStore = @import("analysis/DocumentStore.zig");
const utils = @import("analysis/utils.zig");

const ArgState = enum {
    none,
    add_package_name,
    add_package_path,
    root_name,
};

pub fn main() !void {
    // TODO: Use GPA once memory improves; see issue #1
    const allocator = std.heap.page_allocator;

    var doc_store = DocumentStore{ .allocator = allocator };

    var root_name: ?[]const u8 = null;
    var package_name: ?[]const u8 = null;

    var arg_state: ArgState = .none;
    var arg_iterator = try std.process.ArgIterator.initWithAllocator(allocator);
    defer arg_iterator.deinit();

    while (arg_iterator.next()) |arg| {
        switch (arg_state) {
            .none => {
                if (std.mem.eql(u8, arg, "--pkg")) arg_state = .add_package_name;
                if (std.mem.eql(u8, arg, "--root")) arg_state = .root_name;
            },
            .add_package_name => {
                package_name = arg;
                arg_state = .add_package_path;
            },
            .add_package_path => {
                try doc_store.createPackage(package_name.?, arg);
                arg_state = .none;
            },
            .root_name => {
                if (root_name != null) std.log.err("Multiple roots detected; this invocation may not behave as expected!", .{});
                root_name = arg;
                arg_state = .none;
            },
        }
    }

    if (root_name == null) {
        std.log.err("Please specify a root package name with --root!", .{});
        return;
    }

    var it = (doc_store.packages.get(root_name.?) orelse {
        std.log.err("Root package not found!", .{});
        return;
    }).handles.iterator();
    while (it.next()) |i| {
        try i.value_ptr.*.analyzer.postResolves();
    }

    var index = try std.fs.cwd().createFile("index.scip", .{});
    defer index.close();

    var documents = try StoreToScip.storeToScip(allocator, &doc_store, root_name.?);

    var arg_reiterator = try std.process.ArgIterator.initWithAllocator(allocator);
    defer arg_reiterator.deinit();

    try protobruh.encode(scip.Index{
        .metadata = .{
            .version = .unspecified_protocol_version,
            .tool_info = .{
                .name = "scip-zig",
                .version = "unversioned",
                .arguments = args: {
                    var arguments = std.ArrayListUnmanaged([]const u8){};
                    while (arg_reiterator.next()) |arg| try arguments.append(allocator, arg);
                    break :args arguments;
                },
            },
            .project_root = try utils.fromPath(allocator, std.fs.path.basename(doc_store.packages.get(root_name.?).?.root)),
            .text_document_encoding = .utf8,
        },
        .documents = documents,
        .external_symbols = .{},
    }, index.writer());
}
