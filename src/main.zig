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
    root_path,
};

pub fn main() !void {
    // TODO: Use GPA once memory improves; see issue #1
    const allocator = std.heap.page_allocator;

    var doc_store = DocumentStore{
        .allocator = allocator,
        .root_path = "",
    };

    var cwd_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;

    var root_path: []const u8 = try std.os.getcwd(&cwd_buf);
    var root_name: ?[]const u8 = null;
    var package_name: ?[]const u8 = null;

    var arg_state: ArgState = .none;
    var arg_iterator = try std.process.ArgIterator.initWithAllocator(allocator);
    defer arg_iterator.deinit();

    doc_store.root_path = root_path;

    while (arg_iterator.next()) |arg| {
        switch (arg_state) {
            .none => {
                if (std.mem.eql(u8, arg, "--pkg")) arg_state = .add_package_name;
                if (std.mem.eql(u8, arg, "--root-pkg")) arg_state = .root_name;
                if (std.mem.eql(u8, arg, "--root-path")) arg_state = .root_path;
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
            .root_path => {
                if (root_name != null) std.log.err("Multiple roots detected; this invocation may not behave as expected!", .{});
                root_path = arg;
                doc_store.root_path = root_path;
                arg_state = .none;
            },
        }
    }

    if (root_name == null) {
        std.log.err("Please specify a root package name with --root-pkg!", .{});
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

    var bufw = std.io.bufferedWriter(index.writer());

    const project_root = try utils.fromPath(allocator, root_path);
    std.log.info("Using project root {s}", .{project_root});

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
            .project_root = project_root,
            .text_document_encoding = .utf8,
        },
        .documents = documents,
        .external_symbols = .{},
    }, bufw.writer());

    try bufw.flush();
}
