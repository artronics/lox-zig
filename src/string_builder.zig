const std = @import("std");
const Allocator = std.mem.Allocator;
const scanner = @import("scanner.zig");
const warn = std.log.warn;
const StringBuilderError = error{} || std.fmt.BufPrintError || std.fmt.AllocPrintError || Allocator.Error;

pub const StringBuilder = struct {
    allocator: Allocator,
    buffer: []u8,
    index: usize,

    const Self = @This();

    pub fn init(comptime capacity: usize, allocator: Allocator) !Self {
        const buffer = try allocator.alloc(u8, capacity);
        return Self{ .allocator = allocator, .buffer = buffer, .index = 0 };
    }

    pub fn deinit(self: Self) void {
        self.allocator.free(self.buffer);
    }

    pub fn string(self: Self) []const u8 {
        return self.buffer[0..self.index];
    }

    /// Deprecated
    pub fn append(self: *Self, str: []const u8) StringBuilderError!void {
        if (self.buffer.len - self.index < str.len) {
            self.buffer = try self.allocator.realloc(self.buffer, 2 * self.buffer.len);
        }
        const s = try std.fmt.bufPrint(self.buffer[self.index..], "{s}", .{str});
        self.index += s.len;
    }
    
    // TODO: replace append with this one
    pub fn append2(self: *Self, comptime fmt: []const u8, args: anytype) StringBuilderError!void {
        const str = try std.fmt.allocPrint(self.allocator, fmt, args);
        defer self.allocator.free(str);
        try self.append(str);
    }
};

const expect = std.testing.expect;

test "StringBuilder" {
    const a = std.testing.allocator;
    {
        var sb = try StringBuilder.init(7, a);
        defer sb.deinit();

        try sb.append("foo");
        try sb.append(" bar");

        try expect(std.mem.eql(u8, sb.string(), "foo bar"));
    }
    { // realloc memory
        var sb = try StringBuilder.init(5, a);
        defer sb.deinit();

        try sb.append("foo");
        try sb.append(" bar");

        try expect(std.mem.eql(u8, sb.string(), "foo bar"));
    }
}