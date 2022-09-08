const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const warn = std.log.warn;

const scanner = @import("scanner.zig");
const Token = scanner.Token;
const TokenType = scanner.TokenType;
const LiteralToken = scanner.LiteralToken;

const expr = @import("expr.zig");
const Expr = expr.Expr;
const BinExpr = expr.BinExpr;

pub const ParserError = error{
    parse_error,
    unknown_state,
} || Allocator.Error;

pub const TokenError = struct {
    token: Token,
    message: []const u8,

    fn init(token: Token, message: []const u8) TokenError {
        return TokenError{ .token = token, .message = message };
    }
};

pub const Parser = struct {
    const Self = @This();
    allocator: Allocator,
    tokens: []const Token,
    current: usize = 0,
    parse_errors: ArrayList(TokenError),

    pub fn init(allocator: Allocator, tokens: []const Token) Self {
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
    fn equality(self: *Self) !Expr {
        var exp = try self.comparison();

        while (self.match(&.{
            TokenType.token_equal_equal,
            TokenType.token_bang_equal,
        })) {
            const l = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(l);
            l.* = exp;

            const op = self.previous().tokenType;

            const r = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(r);
            r.* = try self.comparison();

            const be = expr.BinExpr{ .l = l, .r = r };

            warn("equality cur: {s}", .{@tagName(self.peek().tokenType)});
            warn("equality pre: {s}", .{@tagName(self.previous().tokenType)});
            exp = if (op == TokenType.token_equal_equal)
                Expr{ .equal = be }
            else
                Expr{ .not_equal = be };
        }

        return exp;
    }

    // comparison -> term ( (">" | ">=" | "<" | "<=") term)* ;
    fn comparison(self: *Self) ParserError!Expr {
        var exp = try self.term();
        while (self.match(&.{
            TokenType.token_greater,
            TokenType.token_greater_equal,
            TokenType.token_less,
            TokenType.token_less_equal,
        })) {
            const l = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(l);
            l.* = exp;

            const op = self.previous().tokenType;

            const r = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(r);
            r.* = try self.term();

            const be = expr.BinExpr{ .l = l, .r = r };

            exp = switch (op) {
                TokenType.token_greater => Expr{ .greater = be },
                TokenType.token_greater_equal => Expr{ .greater_equal = be },
                TokenType.token_less => Expr{ .less = be },
                TokenType.token_less_equal => Expr{ .less_equal = be },
                else => unreachable,
            };
        }

        return exp;
    }

    // term -> factor ( ("+" | "-") factor)* ;
    fn term(self: *Self) ParserError!Expr {
        var exp = try self.factor();

        while (self.match(&.{
            TokenType.token_plus,
            TokenType.token_minus,
        })) {
            const l = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(l);
            l.* = exp;

            const op = self.previous().tokenType;

            const r = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(r);
            r.* = try self.factor();

            warn("term l: {s}", .{@tagName(self.previous().tokenType)});
            warn("term op: {s}", .{@tagName(op)});

            const be = expr.BinExpr{ .l = l, .r = r };

            exp = if (op == TokenType.token_plus)
                Expr{ .add = be }
            else
                Expr{ .sub = be };
        }

        return exp;
    }

    // factor -> unary ( ("*" | "/") unary)* ;
    fn factor(self: *Self) ParserError!Expr {
        var exp = try self.unary();

        while (self.match(&.{
            TokenType.token_star,
            TokenType.token_slash,
        })) {
            const l = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(l);
            l.* = exp;

            const op = self.previous().tokenType;

            const r = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(r);
            r.* = try self.unary();

            const be = expr.BinExpr{ .l = l, .r = r };

            exp = if (op == TokenType.token_star)
                Expr{ .mul = be }
            else
                Expr{ .div = be };
        }

        return exp;
    }

    // unary -> ("!" | "-") unary
    //          | primary ;
    fn unary(self: *Self) ParserError!Expr {
        if (self.match(&.{ TokenType.token_bang, TokenType.token_minus })) {
            const op = self.previous().tokenType;

            const ue = try self.allocator.create(expr.Expr);
            errdefer self.allocator.destroy(ue);
            ue.* = try self.unary();

            return if (op == TokenType.token_bang)
                Expr{ .not = ue }
            else
                Expr{ .minus = ue };
        } else {
            return try self.primary();
        }
    }

    // primary -> NUMBER | STRING | "true" | "false" | "nil" ;
    //          | "(" expression ")" ;
    fn primary(self: *Self) ParserError!Expr {
        if (self.match(&.{
            TokenType.token_number,
            TokenType.token_string,
            TokenType.token_true,
            TokenType.token_false,
            TokenType.token_nil,
        })) {
            // warn("primary cur: {s}", .{@tagName(self.peek().tokenType)});
            warn("primary pre: {s}", .{@tagName(self.previous().tokenType)});
            return switch (self.previous().tokenType) {
                TokenType.token_number => Expr{ .literal_number = self.previous().literalNumber() }, // TODO: copy number
                TokenType.token_string => Expr{ .literal_string = self.previous().literalString() }, // TODO: copy string
                TokenType.token_true => Expr.literal_true,
                TokenType.token_false => Expr.literal_false,
                TokenType.token_nil => Expr.literal_nil,
                else => unreachable,
            };
        }
        if (self.match(&.{TokenType.token_left_paren})) {
            const exp = try self.allocator.create(Expr);
            errdefer self.allocator.destroy(exp);
            warn("lef paren matched", .{});
            exp.* = try self.expression();
            _ = try self.consume(TokenType.token_right_paren, "Expect ')' after expression."); // TODO: error handling

            return Expr{ .grouping = exp };
        }

        return ParserError.unknown_state;
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn match(self: *Self, tokenTypes: []const TokenType) bool {
        for (tokenTypes) |tt| {
            if (self.check(tt)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Self, tokenType: TokenType) bool {
        if (self.isAtEnd()) return false;

        return self.peek().tokenType == tokenType;
    }

    fn isAtEnd(self: *Self) bool {
        return self.peek().tokenType == TokenType.token_eof;
    }

    fn peek(self: *Self) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Self) Token {
        return self.tokens[self.current - 1];
    }

    fn consume(self: *Self, tokenType: TokenType, msg: []const u8) ParserError!Token {
        if (self.check(tokenType)) return self.advance();
        // error
        try self.parse_errors.append(TokenError.init(self.peek(), msg));
        return ParserError.parse_error;
    }
};

const testing = std.testing;
const expect = std.testing.expect;
const expectError = std.testing.expectError;

const test_alloc = testing.allocator;
test "parser test" {
    const tokens = makeTestTokens();
    var p = Parser.init(test_alloc, &tokens);
    defer p.deinit();
    const act_expr = try p.parse();
    defer act_expr.deinit(test_alloc);

    const exp_expr = try makeExpectedExpr(test_alloc);
    defer exp_expr.deinit(test_alloc);

    // Print act and exp in case of error
    const exp_s = try exp_expr.allocPrint(test_alloc);
    defer test_alloc.free(exp_s);
    const act_s = try act_expr.allocPrint(test_alloc);
    defer test_alloc.free(act_s);
    errdefer {
        warn("expected| {s}", .{exp_s});
        warn("actual: | {s}", .{act_s});
    }

    try expect(exp_expr.eql(&act_expr));
}

test "parser error" {
    const tokens = &[_]Token{
        Token.init(TokenType.token_true, "true", LiteralToken{ .Identifier = "true" }, 1),
        Token.init(TokenType.token_equal_equal, "==", LiteralToken.None, 1),
        Token.init(TokenType.token_eof, "", LiteralToken.None, 1),
    };

    var p = Parser.init(test_alloc, tokens);
    defer p.deinit();
    const act_result = p.parse();
    // _= act_result;

    // TODO: in case of error all allocated expr will leak. 
    // use errdefer in the error() method to free expr so far has been created
    // or in parse() method catch error and free expr
    // or leave expr as it is but keep a pointer to in if parse_errors is not empty and then destroy it in the deinit() method

    // try expectError(ParserError.parse_error, act_result);
    try expectError(ParserError.unknown_state, act_result);
    // try expect(p.parse_errors.items.len == 1);
}

fn makeTestTokens() [9]Token {
    return [_]Token{
        Token.init(TokenType.token_true, "true", LiteralToken{ .Identifier = "true" }, 1),
        Token.init(TokenType.token_equal_equal, "==", LiteralToken.None, 1),
        Token.init(TokenType.token_false, "false", LiteralToken{ .Identifier = "false" }, 1),
        Token.init(TokenType.token_greater, ">", LiteralToken.None, 1),
        Token.init(TokenType.token_number, "12.3", LiteralToken{ .Number = 12.3 }, 1),
        Token.init(TokenType.token_plus, "+", LiteralToken.None, 1),
        Token.init(TokenType.token_bang, "!", LiteralToken.None, 1),
        Token.init(TokenType.token_string, "foo", LiteralToken{ .String = "foo" }, 1),
        Token.init(TokenType.token_eof, "", LiteralToken.None, 1),
    };
}

fn makeExpectedExpr(a: Allocator) !Expr {
    //(== true (> false (+ 12.3 !"foo")))
    const lit_true = try a.create(Expr);
    lit_true.* = Expr.literal_true;
    const lit_false = try a.create(Expr);
    lit_false.* = Expr.literal_false;
    const lit_num1 = try a.create(Expr);
    lit_num1.* = Expr{ .literal_number = 12.3 };
    const lit_str1 = try a.create(Expr);
    lit_str1.* = Expr{ .literal_string = "foo" };

    const e0 = try a.create(Expr);
    e0.* = Expr{ .not = lit_str1 };
    const e1 = try a.create(Expr);
    e1.* = Expr{ .add = BinExpr{ .l = lit_num1, .r = e0 } };
    const e2 = try a.create(Expr);
    e2.* = Expr{ .greater = BinExpr{ .l = lit_false, .r = e1 } };

    return Expr{ .equal = BinExpr{ .l = lit_true, .r = e2 } };
}

fn makeSyntaxError() [3]Token {
    return [_]Token{
        Token.init(TokenType.token_string, "foo", LiteralToken{ .String = "foo" }, 1),
        Token.init(TokenType.token_string, "bar", LiteralToken{ .String = "bar" }, 1),
        Token.init(TokenType.token_eof, "", LiteralToken.None, 1),
    };
}
