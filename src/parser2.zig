const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const warn = std.log.warn;
const token = @import("token.zig");
const Token = token.Token;
const expr = @import("expr2.zig");
const Expr = expr.Expr;

pub const ParserError = error{
    parse_error,
    unknown_state,
} || Allocator.Error;

pub const TokenError = struct {
    token: u32,
    message: []const u8,

    fn init(token_index: u32, message: []const u8) TokenError {
        return TokenError{ .token = token_index, .message = message };
    }
};

pub const Parser = struct {
    const Self = @This();
    allocator: Allocator,
    tokens: token.Tokens,
    current: usize = 0,
    parse_errors: ArrayList(TokenError),

    pub fn init(allocator: Allocator, tokens: token.Tokens) Self {
        return Self{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .parse_errors = ArrayList(TokenError).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.parse_errors.deinit();
    }

    // expression -> equality ;
    // equality -> comparison ( ("==" | "!=") comparison )* ;
    // comparison -> term ( (">" | ">=" | "<" | "<=") term)* ;
    // term -> factor ( ("+" | "-") factor)* ;
    // factor -> unary ( ("*" | "/") unary)* ;
    // unary -> ("!" | "-") unary
    //          | primary ;
    // primary -> NUMBER | STRING | "true" | "false" | "nil" ;
    //          | "(" expression ")" ;

    pub fn parse(self: *Self) ParserError!Expr {
        return try self.expression();
    }

    fn expression(self: *Self) ParserError!Expr {
        return self.equality();
    }

    // equality -> comparison ( ("==" | "!=") comparison )* ;
    fn equality(self: *Self) ParserError!Expr {
        _ = self;
        return Expr{ .tag = token.Tag.literal_number, .token = 5, .nodes = null };
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn match(self: *Self, tags: []const token.Tag) bool {
        for (tags) |tag| {
            if (self.check(tag)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Self, tag: token.Tag) bool {
        if (self.isAtEnd()) return false;
        return self.peek().tag == tag;
    }

    fn isAtEnd(self: *Self) bool {
        return self.peek().tag == token.Tag.t_eof;
    }

    fn peek(self: *Self) Token {
        return self.tokens.tokens.items[self.current];
    }

    fn previous(self: *Self) Token {
        return self.tokens.tokens.items[self.current - 1];
    }

    fn consume(self: *Self, tag: token.Tag, msg: []const u8) ParserError!Token {
        if (self.check(tag)) return self.advance();
        _ = msg;
        return ParserError.parse_error;
        // return self.addError(msg); // FIXME: uncomment and add errors
    }
};

const testing = std.testing;
const expect = std.testing.expect;
const expectError = std.testing.expectError;
const scanner = @import("scanner2.zig");

test "parser" {
    const a = std.testing.allocator;

    var tokens: token.Tokens = undefined;
    var exp_expr: Expr = undefined;
    try makeTestExpr(a, &exp_expr, &tokens);
    defer tokens.deinit();
    defer exp_expr.deinit(a);

    const str = try tokens.printAll(a);
    defer a.free(str);
    warn("{s}", .{str});
    const e_str = try exp_expr.print(a, tokens);
    defer a.free(e_str);
    warn("{s}", .{e_str});
}

fn makeTestExpr(a: Allocator, e: *Expr, t: *token.Tokens) !void {
    const Tag = expr.Tag;
    //(== true (> false ((+ 12.3 (! "foo")))))
    const source = "(!\"foo\" + 12.3) > false == true";

    var sc = try scanner.Scanner.init(a, source[0..]);
    defer sc.deinit();

    var tokens = try sc.scanTokens();
    t.* = tokens;

    const l_num = Expr{ .tag = Tag.literal_number, .token = 4, .nodes = null };
    const l_true = Expr{ .tag = Tag.literal_true, .token = 9, .nodes = null };
    const l_false = Expr{ .tag = Tag.literal_false, .token = 7, .nodes = null };
    const l_str = Expr{ .tag = Tag.literal_string, .token = 2, .nodes = null };

    const not_n = try a.alloc(Expr, 1);
    not_n[0] = l_str;
    const not = Expr{ .tag = Tag.not, .token = null, .nodes = not_n };

    const add_n = try a.alloc(Expr, 2);
    add_n[0] = l_num;
    add_n[1] = not;
    const add = Expr{ .tag = Tag.add, .token = null, .nodes = add_n };

    const grp_n = try a.alloc(Expr, 1);
    grp_n[0] = add;
    const grp = Expr{ .tag = Tag.grouping, .token = null, .nodes = grp_n };

    const grt_n = try a.alloc(Expr, 2);
    grt_n[0] = l_false;
    grt_n[1] = grp;
    const grt = Expr{ .tag = Tag.greater, .token = null, .nodes = grt_n };

    const eql_n = try a.alloc(Expr, 2);
    eql_n[0] = l_true;
    eql_n[1] = grt;
    const eql = Expr{ .tag = Tag.equal, .token = null, .nodes = eql_n };

    e.* = eql;
}
