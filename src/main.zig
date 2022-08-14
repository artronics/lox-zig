const std = @import("std");
const warn = std.log.warn;

var hadError = false;

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
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
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

    try run(content);
}

fn run(content: []const u8) !void {
    const scanner = Scanner.init(content);
    const tokens = try scanner.scanTokens();
    var it = tokens.first;
    while (it) |token| : (it = token.next) {
        warn("token {s}", .{token.data.value});
    }
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
