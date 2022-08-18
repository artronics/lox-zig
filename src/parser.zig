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

        while (self.match(&.{
            TokenType.token_equal_equal,
            TokenType.token_bang_equal,
        })) {
            const be = try self.allocator.create(expr.BinaryExpr);
            be.* = expr.BinaryExpr{
                .left = exp,
                .right = try self.comparison(),
            };

            exp = if (self.previous().tokenType == TokenType.token_equal_equal)
                Expr{ .equal = be }
            else
                Expr{ .not_equal = be };
        }

        return exp;
    }

    // comparison -> term ( (">" | ">=" | "<" | "<=") term)* ;
    fn comparison(self: *Self) !Expr {
        var exp = try self.term();
        while (self.match(&.{
            TokenType.token_greater,
            TokenType.token_greater_equal,
            TokenType.token_less,
            TokenType.token_less_equal,
        })) {
            const be = try self.allocator.create(expr.BinaryExpr);
            be.* = expr.BinaryExpr{
                .left = exp,
                .right = try self.term(),
            };

            exp = switch (self.previous().tokenType) {
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
    fn term(self: *Self) !Expr {
        var exp = try self.factor();

        while (self.match(&.{
            TokenType.token_plus,
            TokenType.token_minus,
        })) {
            const be = try self.allocator.create(expr.BinaryExpr);
            be.* = expr.BinaryExpr{
                .left = exp,
                .right = try self.factor(),
            };

            exp = if (self.previous().tokenType == TokenType.token_plus)
                Expr{ .add = be }
            else
                Expr{ .sub = be };
        }

        return exp;
    }

    // factor -> unary ( ("*" | "/") unary)* ;
    fn factor(self: *Self) !Expr {
        var exp = try self.unary();

        while (self.match(&.{
            TokenType.token_star,
            TokenType.token_slash,
        })) {
            const be = try self.allocator.create(expr.BinaryExpr);
            be.* = expr.BinaryExpr{
                .left = exp,
                .right = try self.unary(),
            };

            exp = if (self.previous().tokenType == TokenType.token_star)
                Expr{ .mul = be }
            else
                Expr{ .div = be };
        }

        return exp;
    }

    // unary -> ("!" | "-") unary
    //          | primary ;
    fn unary(self: *Self) !Expr {
        if (self.match(&.{ TokenType.token_bang, TokenType.token_minus })) {
            const ue = try self.allocator.create(expr.Expr);
            // ue.* = try self.unary();

            return if (self.previous().tokenType == TokenType.token_bang)
                Expr{ .not = ue }
            else
                Expr{ .minus = ue };
        } else {
            return try self.primary();
        }
    }

    // primary -> NUMBER | STRING | "true" | "false" | "nil" ;
    //          | "(" expression ")" ;
    fn primary(self: *Self) !Expr {
        if (self.match(&.{
            TokenType.token_number,
            TokenType.token_string,
            TokenType.token_true,
            TokenType.token_false,
            TokenType.token_nil,
        })) {
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
            // exp.* = try self.expression(); // FIXME: error issue (recursive)
            exp.* = Expr{.value = 56};
            _ = self.consume(TokenType.token_right_paren); // TODO: error handling
            return Expr{ .grouping = exp };
        }

        return Expr{ .value = 111 };
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
    fn consume(self: *Self, tokenType: TokenType) ?Token {
        if (self.check(tokenType)) return self.advance();
        return null;
    }
};

const testing = std.testing;
const expect = std.testing.expect;

test "parser test" {
    var allocator = testing.allocator;

    const tokens = [_]Token{
        Token.init(TokenType.token_equal_equal, "==", LiteralToken.None, 1),
        Token.init(TokenType.token_bang_equal, "!=", LiteralToken.None, 1),
        // Token.init(TokenType.token_left_paren, "(", LiteralToken.None, 1),
        // Token.init(TokenType.token_right_paren, ")", LiteralToken.None, 1),
        Token.init(TokenType.token_eof, "", LiteralToken.None, 1),
    };

    var p = Parser.init(allocator, &tokens);
    defer p.deinit();
    const exp = try p.parse();
    defer exp.deinit(allocator);
    _ = exp;

    // try expect(exp.equal.*.left.value == 42);
    // try expect(exp.equal.*.left.not_equal.*.left.value == 42);
}
//
