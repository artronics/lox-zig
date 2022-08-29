const std = @import("std");
const Allocator = std.mem.Allocator;
const scanner = @import("scanner.zig");
const warn = std.log.warn;

pub const BinaryOperator = enum {
    equality,
};

pub const BinaryExpr = struct {
    right: Expr,
    left: Expr,
    fn print(self: *BinaryExpr, buf: []u8, op: []const u8) ![]const u8 {
        const l = try self.*.left.print(buf);
        warn("left binExpr {s} {s}", .{ op, l });
        const r = try self.*.right.print(buf[l.len..]);
        warn("right binExpr {s} {s}", .{ op, r });

        warn("({s} buf: {s})", .{ op, buf });

        return try std.fmt.bufPrint(buf[l.len + r.len ..], "({s} {s} {s})", .{ op, l, r });
    }

    fn print2(self: *BinaryExpr, buf: []u8, buf_index: usize, op: []const u8) !Expr.stringBuf {
        const l = try self.*.left.print2(buf, buf_index);
        warn("left binExpr {s} {s} {d}", .{ op, l.str , buf_index});
        const r = try self.*.right.print2(buf, l.len + buf_index);
        warn("right binExpr {s} {s} {d}", .{ op, r.str , buf_index});

        warn("({s} buf: {s})", .{ op, buf });

        return try std.fmt.bufPrint(buf[l.len + r.len + buf_index ..], "({s} {s} {s})", .{ op, l.str, r.str });
    }
    fn allocPrint(self: *BinaryExpr, allocator: Allocator, op: []const u8) ![]const u8 {
        const l = try self.*.left.allocPrint(allocator);
        const r = try self.*.right.allocPrint(allocator);

        return std.fmt.allocPrint(allocator, "({s} {s} {s})", .{ op, l, r });
    }
};

pub const LiteralExpr = union(enum) {
    literal_true,
    literal_false,
    literal_string: []const u8,
    literal_number: f64,
};

pub const Expr = union(enum) {
    value: f64, // TODO: for testing - delete it
    // binary
    equal: *BinaryExpr,
    not_equal: *BinaryExpr,
    greater: *BinaryExpr,
    greater_equal: *BinaryExpr,
    less: *BinaryExpr,
    less_equal: *BinaryExpr,
    add: *BinaryExpr,
    sub: *BinaryExpr,
    mul: *BinaryExpr,
    div: *BinaryExpr,
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

    const stringBuf = struct {
        str: []u8,
        idx: usize
    };

    pub fn deinit(self: Expr, a: Allocator) void {
        switch (self) {
            .value => {},
            // binary
            .equal, .not_equal, .greater, .greater_equal, .less, .less_equal, .add, .sub, .mul, .div => |v| {
                v.*.left.deinit(a);
                v.*.right.deinit(a);
                a.destroy(v);
            },
            // unary
            .not, .minus => |v| a.destroy(v),
            // literal
            .literal_nil, .literal_false, .literal_true, .literal_number, .literal_string => {},
            // grouping
            .grouping => |v| a.destroy(v),
        }
    }

    const exp_str = "(+ true (* -10  \"hello\"))";
    pub fn print(self: *const Expr, buf: []u8) std.fmt.BufPrintError![]const u8 {
        const p = std.fmt.bufPrint;
        return switch (self.*) {
            .literal_number => |v| p(buf, "{d}", .{v}),
            .literal_true => "true",
            .literal_false => "false",
            .literal_string => |v| p(buf, "\"{s}\"", .{v}),
            .mul => |v| v.print(buf, "*"),
            .add => |v| v.print(buf, "+"),
            .minus => |v| {
                const s = try v.print(buf);
                return p(buf[s.len..], "-{s}", .{s});
            },
            else => return p(buf, " end", .{}),
        };
        // return "(+ foo (* -10  \"hello\"))";
    }

    pub fn print2(self: *const Expr, buf: []u8, buf_index: usize) std.fmt.BufPrintError!Expr.stringBuf {
        const p = std.fmt.bufPrint;
        return switch (self.*) {
            .literal_number => |v| p(buf[buf_index..], "{d}", .{v}),
            .literal_true => p(buf[buf_index..], "true", .{}),
            .literal_false => p(buf[buf_index..], "false", .{}),
            .literal_string => |v| p(buf[buf_index..], "\"{s}\"", .{v}),
            .mul => |v| v.print2(buf, buf_index, "*"),
            .add => |v| v.print2(buf, buf_index, "+"),
            .minus => |v| {
                const s = try v.print2(buf, buf_index);
                return p(buf[buf_index + s.len ..], "-{s}", .{s.str});
            },
            else => return Expr.stringBuf{.str = try p(buf[buf_index..], " end", .{}), .idx = 0},
        };
        // return "(+ foo (* -10  \"hello\"))";
    }

    pub fn allocPrint(self: *const Expr, allocator: Allocator) std.fmt.AllocPrintError![]const u8 {
        const p = std.fmt.allocPrint;
        return switch (self.*) {
            .literal_number => |v| p(allocator, "{d}", .{v}),
            .literal_true => "true",
            .literal_false => "false",
            .literal_string => |v| p(allocator, "\"{s}\"", .{v}),
            .mul => |v| v.allocPrint(allocator, "*"),
            .add => |v| v.allocPrint(allocator, "+"),
            .minus => |v| p(allocator, "-{s}", .{try v.allocPrint(allocator)}),
            else => p(allocator, " end", .{}),
        };
        // return "(+ foo (* -10  \"hello\"))";
    }
};

const testing = std.testing;
const expect = std.testing.expect;
test "exp test" {
    var a = testing.allocator;
    const eql = try a.create(BinaryExpr);
    eql.* = BinaryExpr{ .left = Expr{ .value = 23 }, .right = Expr{ .value = 23 } };
    const e = Expr{ .equal = eql };
    _ = e;

    e.deinit(a);
}

pub const Expr2 = union(enum) {
    value: i32,
    eql: [2]*const Expr2,
    sum: [2]*const Expr2,
    sub: [2]*const Expr2,
    mul: [2]*const Expr2,
    div: [2]*const Expr2,
};

fn eval(e: Expr2) i32 {
    return switch (e) {
        .value => |x| x,
        .eql => |es| eval(es[0].*) + eval(es[1].*), // FIXME: return type does not match
        .sum => |es| eval(es[0].*) + eval(es[1].*),
        .sub => |es| eval(es[0].*) - eval(es[1].*),
        .mul => |es| eval(es[0].*) * eval(es[1].*),
        .div => |es| @divFloor(eval(es[0].*), eval(es[1].*)),
    };
}

const alloc = std.testing.allocator;

test "(2+3)*(4*5) == 100" {
    const two = Expr2{ .value = 2 };
    const three = Expr2{ .value = 3 };
    const four = Expr2{ .value = 4 };
    const five = Expr2{ .value = 5 };

    const plus = Expr2{ .sum = .{ &two, &three } };
    const mul = Expr2{ .mul = .{ &four, &five } };
    const result = Expr2{ .mul = .{ &plus, &mul } };

    try expect(eval(result) == 100);
}

test "Expr print" {
    const exp_str = "(+ true (* -10  \"hello\"))";

    const lit_10 = try makeExpr(Expr{ .literal_number = 10 });
    const unary = Expr{ .minus = lit_10 };

    const mul = Expr{ .mul = try makeBinExpr(unary, Expr{ .literal_string = "hello" }) };
    const add = Expr{ .add = try makeBinExpr(Expr.literal_true, mul) };
    defer add.deinit(alloc);

    const buf = try alloc.alloc(u8, 50);
    _ = buf;
    defer alloc.free(buf);

    const act_str = try add.print2(buf, 0);
    // const act_str = try add.allocPrint(alloc);
    // defer alloc.free(act_str);
    warn("actual: {s}", .{act_str.str});
    try expect(std.mem.eql(u8, exp_str, act_str.str));
    // try expect(unary.minus.*.literal_number == 10);
}

fn makeExpr(e: Expr) !*Expr {
    const v = try alloc.create(Expr);
    v.* = e;
    return v;
}
fn makeBinExpr(l: Expr, r: Expr) !*BinaryExpr {
    const v = try alloc.create(BinaryExpr);
    v.* = BinaryExpr{ .left = l, .right = r };
    return v;
}
