const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const alloc = std.testing.allocator;
const warn = std.log.warn;

const Tree = struct {
    value: ?i32,
    right: ?*Tree,
    left: ?*Tree,
};

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

const BinExpr = struct {
    l: *Expr,
    r: *Expr,

    const Self = @This();
    fn print(self: *const Self, op: []const u8, sb: *StringBuilder) StringBuilderError!void {
        var buf: [10]u8 = undefined;
        const op_str = try std.fmt.bufPrint(&buf, " ({s}", .{op});

        try sb.append(op_str);
        try self.l.print(sb);
        try self.r.print(sb);
        try sb.append(")");
    }
};

fn deinit3(expr: *const Expr) void {
    std.testing.allocator.destroy(expr);
}

const Expr = union(enum) {
    literal_number: f64,
    // add: struct { l: *Expr, r: *Expr },
    // mul: struct { l: *Expr, r: *Expr },
    add: BinExpr,
    mul: BinExpr,

    const Self = @This();

    fn deinit2(self: *const Self, allocator: Allocator) void {
        _ = allocator;
        self.traverse(deinit3);
    }
    fn deinit(self: *const Self, allocator: Allocator) void {
        switch (self.*) {
            .literal_number => {},
            .add, .mul => |v| {
                v.l.deinit(allocator);
                allocator.destroy(v.l);
                v.r.deinit(allocator);
                allocator.destroy(v.r);
            },
        }
    }

    fn traverse(self: *const Self, f: fn (*const Expr) void) void {
        switch (self.*) {
            .literal_number => {},
            .add, .mul => |v| {
                v.l.traverse(f);
                f(v.l);
                v.r.traverse(f);
                f(v.r);
            },
        }
    }

    fn print(self: *const Self, sb: *StringBuilder) StringBuilderError!void {
        switch (self.*) {
            .literal_number => |v| {
                var buf: [128]u8 = undefined; // TODO: should it be allocated? reduce the risk of stack overflow
                const s = try std.fmt.bufPrint(&buf, " {d}", .{v});
                try sb.append(s);
            },
            .add => |v| {
                try v.print("+", sb);
            },
            .mul => |v| {
                try v.print("*", sb);
            },
        }
    }
};

test "expr print" {
    var sb = try StringBuilder.init(100, alloc);
    defer sb.deinit();
    const expr = try makeTestExpr(alloc);
    defer alloc.destroy(expr);
    defer expr.deinit(alloc);
    try expr.print(&sb);

    warn("expr print {s}", .{sb.string()});
    // try expect(std.mem.eql(u8, "yo", sb.string()));
}
test "expr" {
    const expr = try makeTestExpr(alloc);
    defer alloc.destroy(expr);
    defer expr.*.deinit(alloc);

    try expect(expr.*.add.l.*.literal_number == 5);
}

fn makeTestExpr(a: Allocator) !*Expr {
    // (+ 5 (* 2 3))
    const lit2 = try a.create(Expr);
    lit2.* = Expr{ .literal_number = 2 };

    const lit3 = try a.create(Expr);
    lit3.* = Expr{ .literal_number = 3 };

    const mul = try a.create(Expr);
    mul.* = Expr{ .mul = .{ .l = lit2, .r = lit3 } };

    const lit5 = try a.create(Expr);
    lit5.* = Expr{ .literal_number = 5 };

    const expr = try a.create(Expr);
    expr.* = Expr{ .add = .{ .l = lit5, .r = mul } };

    return expr;
}

test "tree" {
    // const v = try alloc.create(i32);
    // v.* = 23;
    const v = 23;
    const r = try alloc.create(Tree);
    defer alloc.destroy(r);
    r.* = Tree{ .value = v, .right = null, .left = null };
    const l = try alloc.create(Tree);
    defer alloc.destroy(l);
    l.* = Tree{ .value = v, .right = null, .left = null };

    const root = Tree{ .value = null, .right = r, .left = l };

    try expect(root.left.?.*.value.? == 23);
}
