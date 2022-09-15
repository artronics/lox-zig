const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const warn = std.log.warn;
const token = @import("token.zig");
const Token = token.Token;
const expr = @import("expr2.zig");
const Expr = expr.Expr;
const string_builder = @import("string_builder.zig");

pub const ParserError = error{
    parse_error,
    unknown_state,
    // FIXME: remove string buffer error it's for testing
} || Allocator.Error || string_builder.StringBuilderError;

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
    current: u32 = 0,
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
        const lhs =  try self.equality();
        warn("expr {s}", .{@tagName(lhs.tag)});
        warn("expr n1 tag {s}", .{@tagName(lhs.nodes.?[0].tag)});
        return lhs;
    }

    // equality -> comparison ( ("==" | "!=") comparison )* ;
    fn equality(self: *Self) ParserError!Expr {
        var lhs = try self.comparison();
        warn("equality {s}", .{@tagName(lhs.tag)});
        warn("equality n1 tag {s}", .{@tagName(lhs.nodes.?[0].tag)});

        while (self.match(&.{
            token.Tag.t_equal_equal,
            token.Tag.t_bang_equal,
        })) {
            const op = self.previous().tag;
            warn("equality inside", .{});
            const rhs = try self.comparison();

            const nodes = try self.allocator.alloc(Expr, 2);
            errdefer self.allocator.free(nodes);
            nodes[0] = lhs;
            nodes[1] = rhs;

            lhs = if (op == token.Tag.t_equal_equal)
                Expr{ .tag = expr.Tag.equal, .token = null, .nodes = nodes }
            else
                Expr{ .tag = expr.Tag.not_equal, .token = null, .nodes = nodes };
        }

        return lhs;
    }

    // comparison -> term ( (">" | ">=" | "<" | "<=") term)* ;
    fn comparison(self: *Self) ParserError!Expr {
        var lhs = try self.term();
        // warn("comparison {s}", .{@tagName(lhs.tag)});
        // warn("comparison n1 tag {s}", .{@tagName(lhs.nodes.?[0].tag)});

        while (self.match(&.{
            token.Tag.t_greater,
            token.Tag.t_greater_equal,
            token.Tag.t_less,
            token.Tag.t_less_equal,
        })) {
            warn("comparison inside", .{});
            const op = self.previous().tag;
            const rhs = try self.term();

            const nodes = try self.allocator.alloc(Expr, 2);
            errdefer self.allocator.free(nodes);
            nodes[0] = lhs;
            nodes[1] = rhs;

            lhs = switch (op) {
                token.Tag.t_greater => Expr{ .tag = expr.Tag.greater, .token = null, .nodes = nodes },
                token.Tag.t_greater_equal => Expr{ .tag = expr.Tag.greater_equal, .token = null, .nodes = nodes },
                token.Tag.t_less => Expr{ .tag = expr.Tag.less, .token = null, .nodes = nodes },
                token.Tag.t_less_equal => Expr{ .tag = expr.Tag.less_equal, .token = null, .nodes = nodes },
                else => unreachable,
            };
        }

        return lhs;
    }

    // term -> factor ( ("+" | "-") factor)* ;
    fn term(self: *Self) ParserError!Expr {
        var lhs = try self.factor();
        warn("term {s}", .{@tagName(lhs.tag)});

        while (self.match(&.{
            token.Tag.t_plus,
            token.Tag.t_minus,
        })) {
            const op = self.previous().tag;
            const rhs = try self.factor();

            const nodes = try self.allocator.alloc(Expr, 2);
            errdefer self.allocator.free(nodes);
            nodes[0] = lhs;
            nodes[1] = rhs;
            // warn("plus lhs: {s}", .{try lhs.print(self.allocator, self.tokens)});
            // warn("plus rhs: {s}", .{try rhs.print(self.allocator, self.tokens)});

            lhs = if (op == token.Tag.t_plus)
                Expr{ .tag = expr.Tag.add, .token = null, .nodes = nodes }
            else
                Expr{ .tag = expr.Tag.sub, .token = null, .nodes = nodes };
        }

        return lhs;
    }

    // factor -> unary ( ("*" | "/") unary)* ;
    fn factor(self: *Self) ParserError!Expr {
        var lhs = try self.unary();

        while (self.match(&.{
            token.Tag.t_star,
            token.Tag.t_slash,
        })) {
            warn("factor inside", .{});
            const op = self.previous().tag;
            const rhs = try self.unary();

            const nodes = try self.allocator.alloc(Expr, 2);
            errdefer self.allocator.free(nodes);
            nodes[0] = lhs;
            nodes[1] = rhs;

            lhs = if (op == token.Tag.t_star)
                Expr{ .tag = expr.Tag.mul, .token = null, .nodes = nodes }
            else
                Expr{ .tag = expr.Tag.div, .token = null, .nodes = nodes };
        }

        return lhs;
    }

    // unary -> ("!" | "-") unary
    //          | primary ;
    fn unary(self: *Self) ParserError!Expr {
        if (self.match(&.{ token.Tag.t_bang, token.Tag.t_minus })) {
            warn("here at all", .{});
            const op = self.previous().tag;

            const nodes = try self.allocator.alloc(Expr, 1);
            errdefer self.allocator.free(nodes);
            nodes[0] = try self.unary();

            return if (op == token.Tag.t_bang)
                Expr{ .tag = expr.Tag.not, .token = null, .nodes = nodes }
            else
                Expr{ .tag = expr.Tag.minus, .token = null, .nodes = nodes };
        } else {
            return try self.primary();
        }
    }

    // primary -> NUMBER | STRING | "true" | "false" | "nil" ;
    //          | "(" expression ")" ;
    fn primary(self: *Self) ParserError!Expr {
        if (self.match(&.{
            token.Tag.t_lit_number,
            token.Tag.t_lit_string,
            token.Tag.t_nil,
            token.Tag.t_true,
            token.Tag.t_false,
        })) {
            const token_index = self.current - 1;
            warn("token index: {d}", .{token_index});
            // const lit = self.tokens.literals.get(token_index);
            return switch (self.previous().tag) {
                token.Tag.t_lit_number => Expr{ .tag = expr.Tag.literal_number, .token = token_index, .nodes = null },
                token.Tag.t_lit_string => Expr{ .tag = expr.Tag.literal_string, .token = token_index, .nodes = null },
                token.Tag.t_nil => Expr{ .tag = expr.Tag.literal_nil, .token = token_index, .nodes = null },
                token.Tag.t_true => Expr{ .tag = expr.Tag.literal_true, .token = token_index, .nodes = null },
                token.Tag.t_false => Expr{ .tag = expr.Tag.literal_false, .token = token_index, .nodes = null },
                else => unreachable,
            };
        }
        if (self.match(&.{token.Tag.t_left_paren})) {
            const nodes = try self.allocator.alloc(Expr, 1);
            errdefer self.allocator.free(nodes);
            nodes[0] = try self.expression();

            _ = try self.consume(token.Tag.t_right_paren, "Expect ')' after expression."); // TODO: error handling

            return Expr{ .tag = expr.Tag.grouping, .token = null, .nodes = nodes };
        }

        // TODO: add error handling
        // return self.addError("Expected an expression");
        return ParserError.unknown_state;
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

    // TODO: this should be Token not Tag
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

    var p = Parser.init(a, tokens);
    defer p.deinit();

    var act_expr = try p.parse();
    defer act_expr.deinit(a);

    const str = try tokens.printAll(a);
    defer a.free(str);
    warn("{s}", .{str});
    const e_str = try exp_expr.print(a, tokens);
    defer a.free(e_str);
    warn("{s}", .{e_str});

    const a_str = try act_expr.print(a, tokens);
    defer a.free(a_str);
    warn("{s}", .{a_str});
}

// TODO: unused, delete it.
fn makeTestTokens(a: Allocator) !token.Tokens {
    const source = "2 + 3";

    var sc = try scanner.Scanner.init(a, source[0..]);
    defer sc.deinit();

    return try sc.scanTokens();
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
    add_n[0] = not;
    add_n[1] = l_num;
    const add = Expr{ .tag = Tag.add, .token = null, .nodes = add_n };

    const grp_n = try a.alloc(Expr, 1);
    grp_n[0] = add;
    const grp = Expr{ .tag = Tag.grouping, .token = null, .nodes = grp_n };

    const grt_n = try a.alloc(Expr, 2);
    grt_n[0] = grp;
    grt_n[1] = l_false;
    const grt = Expr{ .tag = Tag.greater, .token = null, .nodes = grt_n };

    const eql_n = try a.alloc(Expr, 2);
    eql_n[0] = grt;
    eql_n[1] = l_true;
    const eql = Expr{ .tag = Tag.equal, .token = null, .nodes = eql_n };

    e.* = eql;
}
