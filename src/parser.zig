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

pub const Parser = struct {
    const Self = @This();
    allocator: Allocator,
    tokens: []const Token,
    current: usize = 0,

    pub fn init(allocator: Allocator, tokens: []const Token) Self {
        return Self{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
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

    pub fn parse(self: *Self) !Expr {
        return try self.expression();
    }

    fn expression(self: *Self) !Expr {
        return try self.equality();
    }

    // equality -> comparison ( ("==" | "!=") comparison )* ;
    fn equality(self: *Self) !Expr {
        var exp = try self.comparison();

        while (self.match(&[_]TokenType{
            TokenType.token_equal_equal,
            TokenType.token_bang_equal,
        })) {
            const be = try self.allocator.create(expr.BinaryExpr);
            be.* = expr.BinaryExpr{
                .left = exp,
                .right = try self.comparison(),
            };

            exp = if (self.previous().tokenType == TokenType.token_equal_equal)
                Expr{ .eql = be }
            else
                Expr{ .not_eql = be };
        }

        return exp;
    }

    // comparison -> term ( (">" | ">=" | "<" | "<=") term)* ;
    fn comparison(self: *Self) !Expr {
        _ = self;
        return Expr{ .value = 42.0 };
    }

    fn term(self: *Self) !Expr {
        _ = self;
        return Expr{ .value = 42.0 };
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
};

const testing = std.testing;
const expect = std.testing.expect;

test "parser test" {
    var allocator = testing.allocator;

    const tokens = [_]Token{
        Token.init(TokenType.token_equal_equal, "==", LiteralToken.None, 1),
        Token.init(TokenType.token_bang_equal, "!=", LiteralToken.None, 1),
        Token.init(TokenType.token_left_paren, "(", LiteralToken.None, 1),
        Token.init(TokenType.token_right_paren, ")", LiteralToken.None, 1),
    };

    var p = Parser.init(allocator, &tokens);
    defer p.deinit();
    const exp = try p.parse();
    defer exp.deinit(allocator);
    // defer allocator.destroy(exp.eql[0]);
    // defer allocator.destroy(exp.eql[1]);
    _ = exp;

    // try expect(exp.eql.*.left.not_eql.*.left.value == 42);
}
