const std = @import("std");
const Allocator = std.mem.Allocator;
const scanner = @import("scanner.zig");
const warn = std.log.warn;

const StringBuilderError = error{} || std.fmt.BufPrintError || Allocator.Error;

const StringBuilder = struct {
    allocator: Allocator,
    buffer: []u8,
    index: usize,

    const Self = @This();

    fn init(comptime capacity: usize, allocator: Allocator) !Self {
        const buffer = try allocator.alloc(u8, capacity);
        return Self{ .allocator = allocator, .buffer = buffer, .index = 0 };
    }

    fn deinit(self: Self) void {
        self.allocator.free(self.buffer);
    }

    fn string(self: Self) []const u8 {
        return self.buffer[0..self.index];
    }

    fn append(self: *Self, str: []const u8) StringBuilderError!void {
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

    fn print(self: *const Self, sb: *StringBuilder) StringBuilderError!void {
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
};

const testing = std.testing;
const expect = std.testing.expect;

test "expr" {
    const expr = try makeTestExpr(alloc);
    defer alloc.destroy(expr);
    defer expr.*.deinit(alloc);

    try expect(expr.*.add.l.*.literal_number == 5.2);
}

const alloc = std.testing.allocator;

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
