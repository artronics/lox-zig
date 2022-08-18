const std = @import("std");
const Allocator = std.mem.Allocator;
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const expr = @import("expr.zig");
const warn = std.log.warn;

pub var hadError = false;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        _ = gpa.deinit();
    }

    var a = try std.process.argsAlloc(allocator);
    defer allocator.free(a);
    if (a.len > 1) {
        try runFile(allocator, a[1]);
    }

    if (hadError) {
        warn("ERROR", .{});
    } else {
        warn("SUCCESS", .{});
    }
}

fn runFile(allocator: Allocator, path: []const u8) !void {
    var pathBuf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    var file: std.fs.File = undefined;

    if (std.fs.path.isAbsolute(path)) {
        file = try std.fs.openFileAbsolute(path, .{});
    } else {
        const absPath = try std.fs.cwd().realpath(path, pathBuf[0..]);
        file = try std.fs.openFileAbsolute(absPath, .{});
    }

    const content = try file.readToEndAlloc(allocator, try file.getEndPos());
    defer {
        file.close();
        allocator.free(content);
    }

    try run(allocator, content);
}

fn run(allocator: Allocator, content: []const u8) !void {
    var sc = try scanner.Scanner.init(allocator, content);
    defer sc.deinit();
    const tokens = try sc.scanTokens();
    for (tokens) |token| {
        warn("token {s}", .{@tagName(token.tokenType)});
    }
    defer allocator.free(tokens);
    var p = parser.Parser.init(allocator, tokens);
    defer p.deinit();
    const e = try p.parse();
    defer e.deinit(allocator);
}

fn reportError(line: usize, message: []const u8) void {
    warn("[line {d}] Error: {s}", .{ line, message });
}

const testing = std.testing;

test "run file" {
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const path = try createTestFile(tmp_dir, "test.lox", "a program", buffer[0..]);
    warn("test file {s}", .{path});

    try runFile(testing.allocator, path);
}

test "error" {
    reportError(4, "bad token");
}

fn createTestFile(dir: testing.TmpDir, name: []const u8, content: []const u8, outPath: []u8) ![]u8 {
    var f = try dir.dir.createFile(name, .{ .read = true });
    defer f.close();

    const buf: []const u8 = content;
    try f.writeAll(buf);

    return try dir.dir.realpath(name, outPath);
}
