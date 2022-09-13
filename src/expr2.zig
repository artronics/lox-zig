const std = @import("std");
const Allocator = std.mem.Allocator;
const warn = std.log.warn;
const token = @import("token.zig");
const Token = token.Token;

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
    tag: Tag,
    token: ?u32,
    nodes: ?[]Expr,
};

pub fn deinit(e: Expr, allocator: Allocator) void {
    if (e.nodes) |ns| {
        for (ns) |n| {
            deinit(n, allocator);
        }
        allocator.free(e.nodes.?);
    }
}

const expect = std.testing.expect;
const scanner = @import("scanner2.zig");
test "expr" {
    const a = std.testing.allocator;
    var t: token.Tokens = undefined;
    const e = try makeTestExpr(a, &t);
    defer t.deinit();
    defer deinit(e, a);
    _ = e;
}

const SB = @import("string_builder.zig").StringBuilder;
fn makeTestExpr(a: Allocator, t: *token.Tokens) !Expr { //struct{e:Expr, t:token.Tokens} {
    // (+ 5.2 (* !true "foo"))
    const source = "!true * \"foo\" + 5.2";
    var sc = try scanner.Scanner.init(a, source[0..]);
    defer sc.deinit();

    var tokens = try sc.scanTokens();
    t.* = tokens;

    const s = try tokens.printAll(a);
    defer a.free(s);
    warn("{s}", .{s});

    // const source = "!true * \"foo\" + 5.2"
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
    const not = Expr{ .tag = Tag.add, .token = null, .nodes = not_n };

    _ = l_num;
    _ = l_str;
    // return .{.e = not, .t = tokens};
    return not;
}
