const std = @import("std");
const utils = @import("utils.zig");
const Ast = std.zig.Ast;

pub const Position = struct {
    line: u32,
    character: u32,
};

pub const Range = struct {
    start: Position,
    end: Position,
};

pub const Loc = std.zig.Token.Loc;

pub fn indexToPosition(text: []const u8, index: usize) Position {
    const last_line_start = if (std.mem.lastIndexOf(u8, text[0..index], "\n")) |line| line + 1 else 0;
    const line_count = std.mem.count(u8, text[0..last_line_start], "\n");

    return .{
        .line = @intCast(u32, line_count),
        .character = @intCast(u32, countCodeUnits(text[last_line_start..index])),
    };
}

pub fn positionToIndex(text: []const u8, position: Position) usize {
    var line: u32 = 0;
    var line_start_index: usize = 0;
    for (text) |c, i| {
        if (line == position.line) break;
        if (c == '\n') {
            line += 1;
            line_start_index = i;
        }
    }
    std.debug.assert(line == position.line);

    const line_text = std.mem.sliceTo(text[line_start_index..], '\n');
    const line_byte_length = getNCodeUnitByteCount(line_text, position.character);

    return line_start_index + line_byte_length;
}

pub fn tokenToIndex(tree: Ast, token_index: Ast.TokenIndex) usize {
    return tree.tokens.items(.start)[token_index];
}

pub fn tokenToLoc(tree: Ast, token_index: Ast.TokenIndex) Loc {
    const start = tree.tokens.items(.start)[token_index];
    const tag = tree.tokens.items(.tag)[token_index];

    // Many tokens can be determined entirely by their tag.
    if (tag.lexeme()) |lexeme| {
        return .{
            .start = start,
            .end = start + lexeme.len,
        };
    }

    // For some tokens, re-tokenization is needed to find the end.
    var tokenizer: std.zig.Tokenizer = .{
        .buffer = tree.source,
        .index = start,
        .pending_invalid_token = null,
    };

    // Maybe combine multi-line tokens?
    const token = tokenizer.next();
    // A failure would indicate a corrupted tree.source
    std.debug.assert(token.tag == tag);
    return token.loc;
}

pub fn tokenToSlice(tree: Ast, token_index: Ast.TokenIndex) []const u8 {
    return locToSlice(tree.source, tokenToLoc(tree, token_index));
}

pub fn tokenToPosition(tree: Ast, token_index: Ast.TokenIndex) Position {
    const start = tokenToIndex(tree, token_index);
    return indexToPosition(tree.source, start);
}

pub fn tokenToRange(tree: Ast, token_index: Ast.TokenIndex) Range {
    const start = tokenToPosition(tree, token_index);
    const loc = tokenToLoc(tree, token_index);

    return .{
        .start = start,
        .end = advancePosition(tree.source, start, loc.start, loc.end),
    };
}

pub fn locLength(text: []const u8, loc: Loc) usize {
    return countCodeUnits(text[loc.start..loc.end]);
}

pub fn tokenLength(tree: Ast, token_index: Ast.TokenIndex) usize {
    const loc = tokenToLoc(tree, token_index);
    return locLength(tree.source, loc);
}

pub fn rangeLength(text: []const u8, range: Range) usize {
    const loc: Loc = .{
        .start = positionToIndex(text, range.start),
        .end = positionToIndex(text, range.end),
    };
    return locLength(text, loc);
}

pub fn tokenIndexLength(text: [:0]const u8, index: usize) usize {
    const loc = tokenIndexToLoc(text, index);
    return locLength(text, loc);
}

pub fn tokenIndexToLoc(text: [:0]const u8, index: usize) Loc {
    var tokenizer: std.zig.Tokenizer = .{
        .buffer = text,
        .index = index,
        .pending_invalid_token = null,
    };

    const token = tokenizer.next();
    return .{ .start = token.loc.start, .end = token.loc.end };
}

pub fn tokenPositionToLoc(text: [:0]const u8, position: Position) Loc {
    const index = positionToIndex(text, position);
    return tokenIndexToLoc(text, index);
}

pub fn tokenIndexToSlice(text: [:0]const u8, index: usize) []const u8 {
    return locToSlice(text, tokenIndexToLoc(text, index));
}

pub fn tokenPositionToSlice(text: [:0]const u8, position: Position) []const u8 {
    return locToSlice(text, tokenPositionToLoc(text, position));
}

pub fn tokenIndexToRange(text: [:0]const u8, index: usize) Range {
    const start = indexToPosition(text, index);
    const loc = tokenIndexToLoc(text, index);

    return .{
        .start = start,
        .end = advancePosition(text, start, loc.start, loc.end),
    };
}

pub fn tokenPositionToRange(text: [:0]const u8, position: Position) Range {
    const index = positionToIndex(text, position);
    const loc = tokenIndexToLoc(text, index);

    return .{
        .start = position,
        .end = advancePosition(text, position, loc.start, loc.end),
    };
}

pub fn locToSlice(text: []const u8, loc: Loc) []const u8 {
    return text[loc.start..loc.end];
}

pub fn locToRange(text: []const u8, loc: Loc) Range {
    std.debug.assert(loc.start <= loc.end and loc.end <= text.len);
    const start = indexToPosition(text, loc.start);
    return .{
        .start = start,
        .end = advancePosition(text, start, loc.start, loc.end),
    };
}

pub fn nodeToLoc(tree: Ast, node: Ast.Node.Index) Loc {
    return .{ .start = tokenToIndex(tree, tree.firstToken(node)), .end = tokenToLoc(tree, utils.lastToken(tree, node)).end };
}

pub fn nodeToSlice(tree: Ast, node: Ast.Node.Index) []const u8 {
    return locToSlice(tree.source, nodeToLoc(tree, node));
}

pub fn nodeToRange(tree: Ast, node: Ast.Node.Index) Range {
    return locToRange(tree.source, nodeToLoc(tree, node));
}

pub fn lineLocAtIndex(text: []const u8, index: usize) Loc {
    return .{
        .start = if (std.mem.lastIndexOfScalar(u8, text[0..index], '\n')) |idx| idx else 0,
        .end = std.mem.indexOfScalarPos(u8, text, index, '\n') orelse text.len,
    };
}

pub fn lineSliceAtIndex(text: []const u8, index: usize) []const u8 {
    return locToSlice(text, lineLocAtIndex(text, index));
}

pub fn lineLocAtPosition(text: []const u8, position: Position) Loc {
    return lineLocAtIndex(text, positionToIndex(text, position));
}

pub fn lineSliceAtPosition(text: []const u8, position: Position) []const u8 {
    return locToSlice(text, lineLocAtPosition(text, position));
}

pub fn lineLocUntilIndex(text: []const u8, index: usize) Loc {
    return .{
        .start = if (std.mem.lastIndexOfScalar(u8, text[0..index], '\n')) |idx| idx else 0,
        .end = index,
    };
}

pub fn lineSliceUntilIndex(text: []const u8, index: usize) []const u8 {
    return locToSlice(text, lineLocUntilIndex(text, index));
}

pub fn lineLocUntilPosition(text: []const u8, position: Position) Loc {
    return lineLocUntilIndex(text, positionToIndex(text, position));
}

pub fn lineSliceUntilPosition(text: []const u8, position: Position) []const u8 {
    return locToSlice(text, lineLocUntilPosition(text, position));
}

// Helper functions

/// advance `position` which starts at `from_index` to `to_index` accounting for line breaks
pub fn advancePosition(text: []const u8, position: Position, from_index: usize, to_index: usize) Position {
    var line = position.line;

    for (text[from_index..to_index]) |c| {
        if (c == '\n') {
            line += 1;
        }
    }

    const line_loc = lineLocUntilIndex(text, to_index);

    return .{
        .line = line,
        .character = @intCast(u32, locLength(text, line_loc)),
    };
}

/// returns the number of code units in `text`
pub fn countCodeUnits(text: []const u8) usize {
    return text.len;
}

/// returns the number of (utf-8 code units / bytes) that represent `n` code units in `text`
pub fn getNCodeUnitByteCount(text: []const u8, n: usize) usize {
    _ = text;
    return n;
}
