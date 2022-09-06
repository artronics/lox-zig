const std = @import("std");
const Allocator = std.mem.Allocator;
const scanner = @import("scanner.zig");
const warn = std.log.warn;

const StringBuilderError = error{} || std.fmt.BufPrintError || Allocator.Error;

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

    pub fn append(self: *Self, str: []const u8) StringBuilderError!void {
        if (self.buffer.len - self.index < str.len) {
            self.buffer = try self.allocator.realloc(self.buffer, 2 * self.buffer.len);
        }
        const s = try std.fmt.bufPrint(self.buffer[self.index..], "{s}", .{str});
        self.index += s.len;
    }
};
test "StringBuilder" {
    {
        var sb = try StringBuilder.init(7, alloc);
        defer sb.deinit();

        try sb.append("foo");
        try sb.append(" bar");

        try expect(std.mem.eql(u8, sb.string(), "foo bar"));
    }
    { // realloc memory
        var sb = try StringBuilder.init(5, alloc);
        defer sb.deinit();

        try sb.append("foo");
        try sb.append(" bar");

        try expect(std.mem.eql(u8, sb.string(), "foo bar"));
    }
}

pub const BinExpr = struct {
    l: *Expr,
    r: *Expr,

    const Self = @This();
    fn eql(self: *const Self, other: *const BinExpr) bool {
        return self.l.eql(other.l) and self.r.eql(other.r);
    }

    fn print(self: *const Self, op: []const u8, sb: *StringBuilder) StringBuilderError!void {
        var buf: [10]u8 = undefined;
        const op_str = try std.fmt.bufPrint(&buf, "({s} ", .{op});

        try sb.append(op_str);
        try self.l.print(sb);
        try sb.append(" ");
        try self.r.print(sb);
        try sb.append(")");
    }
};

pub const Expr = union(enum) {
    // binary
    equal: BinExpr,
    not_equal: BinExpr,
    greater: BinExpr,
    greater_equal: BinExpr,
    less: BinExpr,
    less_equal: BinExpr,
    add: BinExpr,
    sub: BinExpr,
    mul: BinExpr,
    div: BinExpr,
    // unary
    not: *Expr,
    minus: *Expr,
    // literal
    literal_nil,
    literal_true,
    literal_false,
    literal_string: []const u8,
    literal_number: f64,
    // Grouping
    grouping: *Expr,

    const Self = @This();
    const stringBuf = struct { str: []u8, idx: usize };

    pub fn deinit(self: Self, a: Allocator) void {
        switch (self) {
            // binary
            .equal, .not_equal, .greater, .greater_equal, .less, .less_equal, .add, .sub, .mul, .div => |v| {
                v.l.deinit(a);
                a.destroy(v.l);
                v.r.deinit(a);
                a.destroy(v.r);
            },
            // unary & grouping
            .not, .minus, .grouping => |v| {
                v.deinit(a);
                a.destroy(v);
            },
            // literal
            .literal_nil, .literal_false, .literal_true, .literal_number, .literal_string => {},
        }
    }

    pub fn eql(self: *const Self, other: *const Expr) bool {
        if (@enumToInt(self.*) != @enumToInt(other.*)) {
            return false;
        }
        // We established that tags are equal. Now we just need to check values if any.
        switch (self.*) {
            .equal, .not_equal, .greater, .greater_equal, .less, .less_equal, .add, .sub, .mul, .div => |v| {
                return v.eql(@ptrCast(*const BinExpr, other));
            },
            .not => |v| {
                return v.eql(other.not);
            },
            .minus => |v| {
                return v.eql(other.minus);
            },
            .grouping => |v| {
                return v.eql(other);
            },
            .literal_nil, .literal_false, .literal_true => {
                return true;
            },
            .literal_number => |v| {
                return v == other.literal_number;
            },
            .literal_string => |v| {
                return std.mem.eql(u8, v, other.literal_string);
            },
        }
        return true;
    }
    pub fn print(self: *const Self, sb: *StringBuilder) StringBuilderError!void {
        switch (self.*) {
            .grouping => |v| {
                try sb.append("(");
                try v.print(sb);
                try sb.append(")");
            },
            .literal_number => |v| {
                var buf: [128]u8 = undefined; // TODO: should it be allocated? reduce the risk of stack overflow
                const s = try std.fmt.bufPrint(&buf, "{d}", .{v});
                try sb.append(s);
            },
            .literal_false => {
                try sb.append("false");
            },
            .literal_true => {
                try sb.append("true");
            },
            .literal_nil => {
                try sb.append("nil");
            },
            .literal_string => |v| {
                // var buf: [v.len]u8 = undefined; // TODO: should it be allocated? reduce the risk of stack overflow
                var buf = try sb.allocator.alloc(u8, v.len + 2);
                defer sb.allocator.free(buf);
                const s = try std.fmt.bufPrint(buf, "\"{s}\"", .{v});
                try sb.append(s);
            },
            .not => |v| {
                try sb.append("!");
                try v.print(sb);
            },
            .minus => |v| {
                try sb.append("-");
                try v.print(sb);
            },
            .equal => |v| {
                try v.print("==", sb);
            },
            .not_equal => |v| {
                try v.print("!=", sb);
            },
            .greater => |v| {
                try v.print(">", sb);
            },
            .greater_equal => |v| {
                try v.print(">=", sb);
            },
            .less => |v| {
                try v.print("<", sb);
            },
            .less_equal => |v| {
                try v.print("<=", sb);
            },
            .add => |v| {
                try v.print("+", sb);
            },
            .sub => |v| {
                try v.print("-", sb);
            },
            .mul => |v| {
                try v.print("*", sb);
            },
            .div => |v| {
                try v.print("/", sb);
            },
        }
    }

    pub fn allocPrint(self: *const Self, allocator: Allocator) ![]const u8 {
        var sb = try StringBuilder.init(2000, allocator);
        defer sb.deinit();

        try self.print(&sb);
        const s = sb.string();

        const dst = try allocator.alloc(u8, s.len);
        std.mem.copy(u8, dst, s);

        return dst;
    }
};

const testing = std.testing;
const expect = std.testing.expect;
const alloc = std.testing.allocator;

test "expr" {
    const expr = try makeTestExpr(alloc);
    defer alloc.destroy(expr);
    defer expr.*.deinit(alloc);

    try expect(expr.*.add.l.*.literal_number == 5.2);
}

test "Expr equality" {
    const a = try makeTestExpr(alloc);
    defer alloc.destroy(a);
    defer a.deinit(alloc);

    const b = try makeTestExpr(alloc);
    defer alloc.destroy(b);
    defer b.deinit(alloc);

    try expect(a.eql(b));
}

test "Expr print" {
    var sb = try StringBuilder.init(100, alloc);
    defer sb.deinit();
    const expr = try makeTestExpr(alloc);
    defer alloc.destroy(expr);
    defer expr.deinit(alloc);
    try expr.print(&sb);

    try expect(std.mem.eql(u8, "(+ 5.2 (* !true \"foo\"))", sb.string()));
}

fn makeTestExpr(a: Allocator) !*Expr {
    // (+ 5.2 (* !true "foo"))
    const lit2 = try a.create(Expr);
    lit2.* = Expr.literal_true;

    const not_lit2 = try a.create(Expr);
    not_lit2.* = Expr{ .not = lit2 };

    const lit3 = try a.create(Expr);
    lit3.* = Expr{ .literal_string = "foo" };

    const mul = try a.create(Expr);
    mul.* = Expr{ .mul = .{ .l = not_lit2, .r = lit3 } };

    const lit5 = try a.create(Expr);
    lit5.* = Expr{ .literal_number = 5.2 };

    const expr = try a.create(Expr);
    expr.* = Expr{ .add = .{ .l = lit5, .r = mul } };

    return expr;
}
