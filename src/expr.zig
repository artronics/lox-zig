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
};

pub const Expr = union(enum) {
    value: f64,
    eql: *BinaryExpr,
    not_eql: *BinaryExpr,

    pub fn deinit(self: Expr, a: Allocator) void {
        switch (self) {
            .value => {},
            .eql, .not_eql => |v| {
                v.*.left.deinit(a);
                v.*.right.deinit(a);
                a.destroy(v);
            },
        }
    }
};

const testing = std.testing;
const expect = std.testing.expect;
test "exp test" {
    var a = testing.allocator;
    const eql = try a.create(BinaryExpr);
    eql.* = BinaryExpr{.left = Expr{.value = 23}, .right = Expr{.value =23}};
    const e = Expr{ .eql = eql };
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
