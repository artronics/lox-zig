const std = @import("std");
const Allocator = std.mem.Allocator;
const warn = std.log.warn;
const token = @import("token.zig");
const Token = token.Token;
const string_builder = @import("string_builder.zig");
const StringBuilder = string_builder.StringBuilder;
const StringBuilderError = string_builder.StringBuilderError;

pub const Tag = enum {
    // binary
    equal,
    not_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    add,
    sub,
    mul,
    div,
    // unary
    not,
    minus,
    // literal
    literal_nil,
    literal_true,
    literal_false,
    literal_string,
    literal_number,
    // Grouping
    grouping,
};

pub const Expr = struct {
    const Self = @This();

    tag: Tag,
    token: ?u32,
    nodes: ?[]Expr,

    pub fn deinit(e: Self, allocator: Allocator) void {
        if (e.nodes) |ns| {
            for (ns) |n| {
                deinit(n, allocator);
            }
            allocator.free(e.nodes.?);
        }
    }

    fn printZ(self: *const Self, sb: *StringBuilder, tokens: *const token.Tokens) StringBuilderError!void {
        switch (self.tag) {
            .literal_number => {
                if (tokens.literal(self.token)) |lit| {
                    try sb.append2("{d}", .{lit.l_number});
                }
            },
            .literal_string => {
                if (tokens.literal(self.token)) |lit| {
                    try sb.append2("\"{s}\"", .{lit.l_string});
                }
            },
            .literal_false => try sb.append2("false", .{}),
            .literal_true => try sb.append2("true", .{}),
            .literal_nil => try sb.append2("nil", .{}),

            .grouping => try sb.append2("(", .{}),
            .not => try sb.append2("(!", .{}),
            .minus => try sb.append2("(-", .{}),

            .equal => try sb.append2("(==", .{}),
            .not_equal => try sb.append2("(!=", .{}),
            .greater => try sb.append2("(>", .{}),
            .greater_equal => try sb.append2("(>=", .{}),
            .less => try sb.append2("(<", .{}),
            .less_equal => try sb.append2("(<=", .{}),
            .add => try sb.append2("(+", .{}),
            .sub => try sb.append2("(-", .{}),
            .mul => try sb.append2("(*", .{}),
            .div => try sb.append2("(/", .{}),
        }

        if (self.nodes) |nodes| {
            for (nodes) |n| {
                try sb.append2(" ", .{});
                try n.printZ(sb, tokens);
            }
            try sb.append2(")", .{});
        }
    }

    fn print(self: Self, allocator: Allocator, tokens: token.Tokens) StringBuilderError![]const u8 {
        var sb = try StringBuilder.init(2000, allocator);
        defer sb.deinit();

        try self.printZ(&sb, &tokens);
        const s = try sb.toOwnedSlice();

        return s;
    }
};

pub fn eql(a: Expr, b: Expr, tokens: token.Tokens) bool {
    _ = a;
    _ = b;
    _ = tokens;
    return false;
}

const expect = std.testing.expect;
const scanner = @import("scanner2.zig");

test "Expr equality" {}

test "expr" {
    const a = std.testing.allocator;

    var tokens: token.Tokens = undefined;
    var expr: Expr = undefined;
    try makeTestExpr(a, &expr, &tokens);
    defer tokens.deinit();
    defer expr.deinit(a);

    const str = try tokens.printAll(a);
    defer a.free(str);
    warn("{s}", .{str});
    const e_str = try expr.print(a, tokens);
    defer a.free(e_str);
    warn("{s}", .{e_str});
}

fn makeTestExpr(a: Allocator, e: *Expr, t: *token.Tokens) !void {
    // (+ 5.2 (* !true "foo"))
    const source = "!true * \"foo\" + 5.2";
    var sc = try scanner.Scanner.init(a, source[0..]);
    defer sc.deinit();

    var tokens = try sc.scanTokens();
    t.* = tokens;

    const ts = [6]Token{
        Token{ .tag = token.Tag.t_bang, .lexeme = source[0..1] },
        Token{ .tag = token.Tag.t_true, .lexeme = source[1..5] },
        Token{ .tag = token.Tag.t_star, .lexeme = source[6..7] },
        Token{ .tag = token.Tag.t_lit_string, .lexeme = source[9..12] },
        Token{ .tag = token.Tag.t_plus, .lexeme = source[14..15] },
        Token{ .tag = token.Tag.t_lit_number, .lexeme = source[16..19] },
    };
    _ = ts;
    const l_num = Expr{ .tag = Tag.literal_number, .token = 5, .nodes = null };
    const l_true = Expr{ .tag = Tag.literal_true, .token = 1, .nodes = null };
    const l_str = Expr{ .tag = Tag.literal_string, .token = 3, .nodes = null };

    const not_n = try a.alloc(Expr, 1);
    not_n[0] = l_true;
    const not = Expr{ .tag = Tag.not, .token = null, .nodes = not_n };

    const mul_n = try a.alloc(Expr, 2);
    mul_n[0] = not;
    mul_n[1] = l_str;
    const mul = Expr{ .tag = Tag.mul, .token = null, .nodes = mul_n };

    const add_n = try a.alloc(Expr, 2);
    add_n[0] = l_num;
    add_n[1] = mul;
    const add = Expr{ .tag = Tag.add, .token = null, .nodes = add_n };

    e.* = add;
}
