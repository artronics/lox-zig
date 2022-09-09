const std = @import("std");
const Allocator = std.mem.Allocator;
const warn = std.log.warn;
const expr = @import("expr.zig");

pub const Error = error{
    expr_eval,
    value_error,
};

pub const LoxValue = union(enum) {
    val_nil,
    val_bool: bool,
    val_number: f64,
    val_string: []const u8,

    const Self = @This();

    pub fn add(self: Self, other: LoxValue) Error!LoxValue {
        return switch (self) {
            // FIXME: val_string should support concatenation but, it requires buffer allocation.
            .val_nil, .val_bool, .val_string => Error.value_error,
            .val_number => |v| {
                if (@enumToInt(self) != @enumToInt(other)) {
                    return Error.value_error; // TODO distinguish between rhs and lhs
                }
                return LoxValue{ .val_number = v + other.val_number };
            },
        };
    }

    pub fn sub(self: Self, other: LoxValue) Error!LoxValue {
        return switch (self) {
            .val_nil, .val_bool, .val_string => Error.value_error,
            .val_number => |v| {
                if (@enumToInt(self) != @enumToInt(other)) {
                    return Error.value_error; // TODO distinguish between rhs and lhs
                }
                return LoxValue{ .val_number = v - other.val_number };
            },
        };
    }

    pub fn mul(self: Self, other: LoxValue) Error!LoxValue {
        return switch (self) {
            .val_nil, .val_bool, .val_string => Error.value_error,
            .val_number => |v| {
                if (@enumToInt(self) != @enumToInt(other)) {
                    return Error.value_error; // TODO distinguish between rhs and lhs
                }
                return LoxValue{ .val_number = v * other.val_number };
            },
        };
    }

    pub fn div(self: Self, other: LoxValue) Error!LoxValue {
        return switch (self) {
            .val_nil, .val_bool, .val_string => Error.value_error,
            .val_number => |v| {
                if (@enumToInt(self) != @enumToInt(other)) {
                    return Error.value_error; // TODO distinguish between rhs and lhs
                }
                // FIXME: check for div by zero
                return LoxValue{ .val_number = v / other.val_number };
            },
        };
    }

    pub fn neg(self: Self) Error!LoxValue {
        return switch (self) {
            .val_nil, .val_bool, .val_string => Error.value_error,
            .val_number => |v| {
                return LoxValue{ .val_number = -v };
            },
        };
    }

    pub fn gt(self: Self, other: LoxValue) Error!LoxValue {
        return switch (self) {
            .val_nil, .val_bool, .val_string => Error.value_error,
            .val_number => |v| {
                try self.checkIso(other);
                return LoxValue{ .val_bool = v > other.val_number };
            },
        };
    }

    pub fn ge(self: Self, other: LoxValue) Error!LoxValue {
        return switch (self) {
            .val_nil, .val_bool, .val_string => Error.value_error,
            .val_number => |v| {
                try self.checkIso(other);
                return LoxValue{ .val_bool = v >= other.val_number};
            },
        };
    }

    pub fn lt(self: Self, other: LoxValue) Error!LoxValue {
        return switch (self) {
            .val_nil, .val_bool, .val_string => Error.value_error,
            .val_number => |v| {
                try self.checkIso(other);
                return LoxValue{ .val_bool = v < other.val_number};
            },
        };
    }

    pub fn le(self: Self, other: LoxValue) Error!LoxValue {
        return switch (self) {
            .val_nil, .val_bool, .val_string => Error.value_error,
            .val_number => |v| {
                try self.checkIso(other);
                return LoxValue{ .val_bool = v <= other.val_number};
            },
        };
    }


    fn checkIso(self: Self, other: LoxValue) Error!void {
        if (@enumToInt(self) != @enumToInt(other)) {
            return Error.value_error; // TODO distinguish between rhs and lhs
        }
    }

    pub fn eql(self: Self, other: LoxValue) bool {
        if (@enumToInt(self) != @enumToInt(other)) {
            return false;
        }
        // We established that tags are equal. Now we just need to check values if any.
        return switch (self) {
            .val_nil => true,
            .val_bool => |v| v == other.val_bool,
            // TODO: do we need to add floating point approximation?
            .val_number => |v| v == other.val_number,
            .val_string => |v| std.mem.eql(u8, v, other.val_string),
        };
    }
};

pub const Interpreter = struct {
    const Self = @This();

    pub fn init() Self {
        return Interpreter{};
    }

    fn eval_expr(self: *const Self, e: *const expr.Expr) Error!LoxValue {
        return switch (e.*) {
            .literal_number => |v| LoxValue{ .val_number = v },
            .literal_true => LoxValue{ .val_bool = true },
            .literal_false => LoxValue{ .val_bool = false },

            .grouping => |v| try self.eval_expr(v),
            .minus => |v| try (try self.eval_expr(v)).neg(),

            .add => |v| try (try self.eval_expr(v.l)).add(try self.eval_expr(v.r)),
            .sub => |v| try (try self.eval_expr(v.l)).sub(try self.eval_expr(v.r)),
            .mul => |v| try (try self.eval_expr(v.l)).mul(try self.eval_expr(v.r)),
            .div => |v| try (try self.eval_expr(v.l)).div(try self.eval_expr(v.r)),

            .greater => |v| try (try self.eval_expr(v.l)).gt(try self.eval_expr(v.r)),
            .greater_equal => |v| try (try self.eval_expr(v.l)).ge(try self.eval_expr(v.r)),
            .less => |v| try (try self.eval_expr(v.l)).lt(try self.eval_expr(v.r)),
            .less_equal => |v| try (try self.eval_expr(v.l)).le(try self.eval_expr(v.r)),

            else => Error.expr_eval,
        };
    }
};

const expect = std.testing.expect;
test "interpreter" {
    const a = std.testing.allocator;
    const Expr = expr.Expr;
    const BinExpr = expr.BinExpr;

    const in = Interpreter.init();
    {
        const e = try makeArithmeticExpr(a);
        defer a.destroy(e);
        defer e.deinit(a);

        const v = try in.eval_expr(e);

        try expect(v.eql(LoxValue{ .val_number = -9 }));
    }

    const num1 = Expr{ .literal_number = 3 };
    const num2 = Expr{ .literal_number = 4 };

    const l_num1 = try a.create(Expr);
    defer a.destroy(l_num1);
    l_num1.* = num1;

    const l_num2 = try a.create(Expr);
    defer a.destroy(l_num2);
    l_num2.* = num2;

    const l_num3 = try a.create(Expr);
    defer a.destroy(l_num3);
    l_num3.* = num1;

    { // 3 > 4
        const e = Expr{ .greater = BinExpr{ .l = l_num1, .r = l_num2 } };
        const v = try in.eval_expr(&e);
        try expect(v.eql(LoxValue{ .val_bool = false }));
    }
    { // 3 >= 3
        const e = Expr{ .greater_equal = BinExpr{ .l = l_num1, .r = l_num3 } };
        const v = try in.eval_expr(&e);
        try expect(v.eql(LoxValue{ .val_bool = true }));
    }
    { // 3 < 4
        const e = Expr{ .less = BinExpr{ .l = l_num1, .r = l_num2 } };
        const v = try in.eval_expr(&e);
        try expect(v.eql(LoxValue{ .val_bool = true }));
    }
    { // 3 <= 3
        const e = Expr{ .less_equal = BinExpr{ .l = l_num1, .r = l_num3 } };
        const v = try in.eval_expr(&e);
        try expect(v.eql(LoxValue{ .val_bool = true }));
    }
}

fn makeArithmeticExpr(a: Allocator) !*const expr.Expr {
    // -(3 + 3) - (3 / 3) * 3 = -9
    const Expr = expr.Expr;
    const l_num1 = try a.create(Expr);
    const l_num2 = try a.create(Expr);
    const l_num3 = try a.create(Expr);
    const l_num4 = try a.create(Expr);
    const l_num5 = try a.create(Expr);
    const l_num = Expr{ .literal_number = 3 };
    l_num1.* = l_num;
    l_num2.* = l_num;
    l_num3.* = l_num;
    l_num4.* = l_num;
    l_num5.* = l_num;

    const add = try a.create(Expr);
    add.* = Expr{ .add = .{ .l = l_num1, .r = l_num2 } };
    const grouping = try a.create(Expr);
    grouping.* = Expr{ .grouping = add };
    const minus = try a.create(Expr);
    minus.* = Expr{ .minus = grouping };

    const div = try a.create(Expr);
    div.* = Expr{ .div = .{ .l = l_num3, .r = l_num4 } };
    const mul = try a.create(Expr);
    mul.* = Expr{ .mul = .{ .l = div, .r = l_num5 } };

    const sub = try a.create(Expr);
    sub.* = Expr{ .sub = .{ .l = minus, .r = mul } };

    return sub;
}
fn makeLogicExpr(a: Allocator) !*const expr.Expr {
    // !(3 > 3)  (3 < 3) * 3 = -9
    const Expr = expr.Expr;
    const l_num1 = try a.create(Expr);
    _ = l_num1;
}

test "LoxValue" {
    const v_nil1 = @as(LoxValue, LoxValue.val_nil);
    const v_nil2 = @as(LoxValue, LoxValue.val_nil);
    const v_bool_t1 = LoxValue{ .val_bool = true };
    const v_bool_t2 = LoxValue{ .val_bool = true };
    const v_bool_f1 = LoxValue{ .val_bool = false };
    const v_float1 = LoxValue{ .val_number = 1.1 };
    const v_float2 = LoxValue{ .val_number = 1.1 };
    const v_int1 = LoxValue{ .val_number = 1 };

    { // eql
        try expect(v_nil1.eql(v_nil2));

        try expect(v_bool_t1.eql(v_bool_t2));
        try expect(!v_bool_t1.eql(v_bool_f1));

        try expect(v_float1.eql(v_float2));
        try expect(!v_float1.eql(LoxValue{ .val_number = 2 }));

        const v_str1 = LoxValue{ .val_string = "foo bar" };
        const v_str2 = LoxValue{ .val_string = "foo bar" };
        try expect(v_str1.eql(v_str2));
        try expect(!v_str1.eql(LoxValue{ .val_string = "bar" }));
    }
    { // add
        const r1 = try v_float1.add(v_float2);
        const r2 = try v_float1.add(v_int1);
        try expect(r1.eql(LoxValue{ .val_number = 2.2 }));
        try expect(r2.eql(LoxValue{ .val_number = 2.1 }));
    }
    { // sub
        const r1 = try v_float1.sub(v_float2);
        const r2 = try v_float1.sub(v_int1);
        try expect(r1.eql(LoxValue{ .val_number = 0 }));
        try expect(floatApproxEql(r2.val_number, 0.1));
    }
    { // mul
        const r1 = try v_float1.mul(v_float2);
        const r2 = try v_float1.mul(v_int1);
        try expect(floatApproxEql(r1.val_number, 1.21));
        try expect(r2.eql(LoxValue{ .val_number = 1.1 }));
    }
    { // div
        const r1 = try v_float1.div(v_float2);
        const r2 = try v_float1.div(v_int1);
        try expect(r1.eql(LoxValue{ .val_number = 1.0 }));
        try expect(r2.eql(LoxValue{ .val_number = 1.1 }));
    }
}

fn floatApproxEql(a: f64, b: f64) bool {
    const eps_value = comptime std.math.floatEps(f64); // For floating point arithmetic
    const sqrt_eps_value = comptime std.math.sqrt(eps_value);
    return std.math.approxEqRel(f64, a, b, sqrt_eps_value);
}
