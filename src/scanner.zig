const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const warn = std.log.warn;

const TokenType = enum {
    // Single-character tokens.
    token_left_paren,
    token_right_paren,
    token_left_brace,
    token_right_brace,
    token_comma,
    token_dot,
    token_minus,
    token_plus,
    token_semicolon,
    token_slash,
    token_star,
    // One or two character tokens.
    token_bang,
    token_bang_equal,
    token_equal,
    token_equal_equal,
    token_greater,
    token_greater_equal,
    token_less,
    token_less_equal,
    // Literals.
    token_identifier,
    token_string,
    token_number,
    // Keywords.
    token_and,
    token_class,
    token_else,
    token_false,
    token_for,
    token_fun,
    token_if,
    token_nil,
    token_or,
    token_print,
    token_return,
    token_super,
    token_this,
    token_true,
    token_var,
    token_while,

    token_error,
    token_eof,
};

const Token = struct {
    tokenType: TokenType,
    lexeme: []const u8,
    literal: ?*anyopaque = null,
    line: usize,

    const Self = @This();

    pub fn init(tokenType: TokenType, lexeme: []const u8, literal: anytype, line: usize) Self {
        return Self{
            .tokenType = tokenType,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }

    pub fn toString(self: Self, allocator: Allocator) ![]u8 {
        const s = try std.fmt.allocPrint(allocator, "{s} {s}", .{ @tagName(self.tokenType), self.lexeme });
        return s;
    }
};

const Scanner = struct {
    allocator: Allocator,
    source: []const u8,
    tokens: ArrayList(Token),

    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    const Self = @This();

    pub fn init(allocator: Allocator, source: []const u8) Self {
        return Self{
            .allocator = allocator,
            .source = source,
            .tokens = ArrayList(Token).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
    }

    pub fn scanTokens(self: *Self) !ArrayList(Token) {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }

        const srcLen = self.source.len;
        try self.tokens.append(Token.init(TokenType.token_eof, self.source[srcLen..srcLen], null, self.line));

        return self.tokens;
    }

    fn scanToken(self: *Self) !void {
        const ch = self.advance();
        switch (ch) {
            // One character
            '(' => try self.addToken(TokenType.token_left_paren),

            // Two characters
            '!' => try self.addToken(if (self.match('=')) TokenType.token_bang_equal else TokenType.token_bang),

            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                } else {
                    try self.addToken(TokenType.token_slash);
                }
            },

            // Whitespace
            ' ', '\t', '\r' => {},
            '\n' => {
                self.line += 1;
            },

            else => unreachable,
        }
    }

    fn advance(self: *Self) u8 {
        const ch = self.source[self.current];
        self.current += 1;

        return ch;
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;

        self.current += 1;
        return true;
    }

    fn peek(self: *Self) u8 {
        if (self.isAtEnd()) return 0;

        return self.source[self.current];
    }

    fn addToken(self: *Self, tokenType: TokenType) !void {
        const t = Token.init(tokenType, self.source[self.start..self.current], null, self.line);
        try self.tokens.append(t);
    }
    fn addTokenLiteral(self: *Self, tokenType: TokenType, literal: anytype) void {
        const t = Token.init(tokenType, self.source[self.start..self.current], literal, self.line);
        self.tokens.append(t);
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }
};

const testing = std.testing;
const expect = testing.expect;

test "init" {
    const source = "(((!!=//yoo comment\n(";
    {
        var scanner = Scanner.init(testing.allocator, source[0..]);
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();
        for (tokens.items) |token| {
            warn("token {s}", .{token.lexeme});
        }
    }
    {
        const content = "prog ()";
        const t = Token.init(TokenType.token_left_brace, content[0..2], null, 2);
        const s = try t.toString(testing.allocator);
        defer testing.allocator.free(s);
        try expect(std.mem.eql(u8, s, "token_left_brace pr"));
    }
}
