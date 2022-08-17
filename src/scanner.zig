const std = @import("std");
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;
const warn = std.log.warn;

pub const TokenType = enum {
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

pub const LiteralToken = union(enum) {
    None,
    String: []const u8,
    Number: f64,
    Identifier: []const u8,
};

pub const Token = struct {
    tokenType: TokenType,
    lexeme: []const u8,
    literal: LiteralToken,
    line: usize,

    const Self = @This();

    pub fn init(tokenType: TokenType, lexeme: []const u8, literal: LiteralToken, line: usize) Self {
        return Self{
            .tokenType = tokenType,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }

    pub fn eql(self: Self, other: Token) bool {
        // FIXME: extracting literal value for f64 .Number case doesn't work
        _ = switch (self.literal) {
            LiteralToken.String => |s| std.mem.eql(u8, s, other.literal.String),
            LiteralToken.Identifier => |s| std.mem.eql(u8, s, other.literal.Identifier),
            LiteralToken.Number => |n| n == other.literal.Number,
            else => true,
        };

        return @enumToInt(self.tokenType) == @enumToInt(other.tokenType) and
            std.mem.eql(u8, self.lexeme, other.lexeme) and
            self.line == other.line and
            std.mem.eql(u8, @tagName(self.literal), @tagName(other.literal));
    }

    pub fn toString(self: Self, allocator: Allocator) ![]u8 {
        const s = try std.fmt.allocPrint(allocator, "{s} {s}", .{ @tagName(self.tokenType), self.lexeme });
        return s;
    }
};

pub const Scanner = struct {
    const Self = @This();

    allocator: Allocator,
    source: []const u8,
    tokens: ArrayList(Token),
    keywords: StringHashMap(TokenType),

    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(allocator: Allocator, source: []const u8) !Self {
        var keywords = StringHashMap(TokenType).init(allocator);
        try Scanner.populateKeywords(&keywords);

        return Self{
            .allocator = allocator,
            .source = source,
            .tokens = ArrayList(Token).init(allocator),
            .keywords = keywords,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.keywords.deinit();
    }

    pub fn scanTokens(self: *Self) ![]const Token {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }

        const srcLen = self.source.len;
        try self.tokens.append(Token.init(TokenType.token_eof, self.source[srcLen..srcLen], LiteralToken.None, self.line));

        return self.tokens.toOwnedSlice();
    }

    fn scanToken(self: *Self) !void {
        const ch = self.advance();
        switch (ch) {
            // One character (){},.-+;*
            '(' => try self.addToken(TokenType.token_left_paren),
            ')' => try self.addToken(TokenType.token_right_paren),
            '{' => try self.addToken(TokenType.token_left_brace),
            '}' => try self.addToken(TokenType.token_right_brace),
            ',' => try self.addToken(TokenType.token_comma),
            '.' => try self.addToken(TokenType.token_dot),
            '-' => try self.addToken(TokenType.token_minus),
            '+' => try self.addToken(TokenType.token_plus),
            ';' => try self.addToken(TokenType.token_semicolon),
            '*' => try self.addToken(TokenType.token_star),

            // Two characters
            '!' => try self.addToken(if (self.match('=')) TokenType.token_bang_equal else TokenType.token_bang),
            '=' => try self.addToken(if (self.match('=')) TokenType.token_equal_equal else TokenType.token_equal),
            '<' => try self.addToken(if (self.match('=')) TokenType.token_less_equal else TokenType.token_less),
            '>' => try self.addToken(if (self.match('=')) TokenType.token_greater_equal else TokenType.token_greater),

            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                } else {
                    try self.addToken(TokenType.token_slash);
                }
            },

            // Literals
            '"' => try self.string(),
            '0'...'9' => try self.number(),

            // Identifier & keywords
            'a'...'z', 'A'...'Z', '_' => try self.identifier(),

            // Whitespace
            ' ', '\t', '\r' => {},
            '\n' => {
                self.line += 1;
            },

            else => unreachable,
        }
    }

    fn string(self: *Self) !void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            // TODO: produce error: unterminated string
        }

        _ = self.advance(); // closing "
        const value: LiteralToken = LiteralToken{ .String = self.source[self.start + 1 .. self.current - 1] };

        try self.addTokenLiteral(TokenType.token_string, value);
    }

    fn number(self: *Self) !void {
        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            _ = self.advance(); // consume .
            while (std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        const value = try std.fmt.parseFloat(f64, self.source[self.start..self.current]);
        try self.addTokenLiteral(TokenType.token_number, LiteralToken{ .Number = value });
    }

    fn identifier(self: *Self) !void {
        while (Scanner.isAlphaNumUnderscore(self.peek())) {
            _ = self.advance();
        }

        const value = self.source[self.start..self.current];
        const tokenType = self.keywords.get(value) orelse TokenType.token_identifier;

        try self.addTokenLiteral(tokenType, LiteralToken{ .Identifier = value });
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

    fn peekNext(self: *Self) u8 {
        if (self.current + 1 >= self.source.len) return 0;

        return self.source[self.current + 1];
    }

    fn addToken(self: *Self, tokenType: TokenType) !void {
        const t = Token.init(tokenType, self.source[self.start..self.current], LiteralToken.None, self.line);
        try self.tokens.append(t);
    }

    fn addTokenLiteral(self: *Self, tokenType: TokenType, literal: LiteralToken) !void {
        const t = Token.init(tokenType, self.source[self.start..self.current], literal, self.line);
        try self.tokens.append(t);
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    fn isAlphaNumUnderscore(ch: u8) bool {
        return std.ascii.isAlNum(ch) or ch == '_';
    }

    fn populateKeywords(map: *std.StringHashMap(TokenType)) !void {
        try map.put("and", TokenType.token_and);
        try map.put("class", TokenType.token_class);
        try map.put("else", TokenType.token_else);
        try map.put("false", TokenType.token_false);
        try map.put("for", TokenType.token_for);
        try map.put("fun", TokenType.token_fun);
        try map.put("if", TokenType.token_if);
        try map.put("nil", TokenType.token_nil);
        try map.put("or", TokenType.token_or);
        try map.put("print", TokenType.token_print);
        try map.put("return", TokenType.token_return);
        try map.put("super", TokenType.token_super);
        try map.put("this", TokenType.token_this);
        try map.put("true", TokenType.token_true);
        try map.put("var", TokenType.token_var);
        try map.put("while", TokenType.token_while);
    }
};

const testing = std.testing;
const expect = testing.expect;

test "init" {
    const source = "(){},.-+;/*\n! !=\n= ==\t> >=\r< <=\n\"a string\"\n \"multiline\nstring\n\" 12.24 42 \nfoo _foo123 _123 \n and";
    const tokens = [_]Token{
        Token.init(TokenType.token_left_paren, "(", LiteralToken.None, 1),
        Token.init(TokenType.token_right_paren, ")", LiteralToken.None, 1),
        Token.init(TokenType.token_left_brace, "{", LiteralToken.None, 1),
        Token.init(TokenType.token_right_brace, "}", LiteralToken.None, 1),
        Token.init(TokenType.token_comma, ",", LiteralToken.None, 1),
        Token.init(TokenType.token_dot, ".", LiteralToken.None, 1),
        Token.init(TokenType.token_minus, "-", LiteralToken.None, 1),
        Token.init(TokenType.token_plus, "+", LiteralToken.None, 1),
        Token.init(TokenType.token_semicolon, ";", LiteralToken.None, 1),
        Token.init(TokenType.token_slash, "/", LiteralToken.None, 1),
        Token.init(TokenType.token_star, "*", LiteralToken.None, 1),

        Token.init(TokenType.token_bang, "!", LiteralToken.None, 2),
        Token.init(TokenType.token_bang_equal, "!=", LiteralToken.None, 2),
        Token.init(TokenType.token_equal, "=", LiteralToken.None, 3),
        Token.init(TokenType.token_equal_equal, "==", LiteralToken.None, 3),
        Token.init(TokenType.token_greater, ">", LiteralToken.None, 3),
        Token.init(TokenType.token_greater_equal, ">=", LiteralToken.None, 3),
        Token.init(TokenType.token_less, "<", LiteralToken.None, 3),
        Token.init(TokenType.token_less_equal, "<=", LiteralToken.None, 3),

        // literals
        Token.init(TokenType.token_string, "\"a string\"", LiteralToken{ .String = "a string" }, 4),
        Token.init(TokenType.token_string, "\"multiline\nstring\n\"", LiteralToken{ .String = "a string" }, 7),
        Token.init(TokenType.token_number, "12.24", LiteralToken{ .Number = 12.24 }, 7),
        Token.init(TokenType.token_number, "42", LiteralToken{ .Number = 42 }, 7),

        // identifier
        Token.init(TokenType.token_identifier, "foo", LiteralToken{ .Identifier = "foo" }, 8),
        Token.init(TokenType.token_identifier, "_foo123", LiteralToken{ .Identifier = "_foo123" }, 8),
        Token.init(TokenType.token_identifier, "_123", LiteralToken{ .Identifier = "_123" }, 8),

        // keywords
        Token.init(TokenType.token_and, "and", LiteralToken{ .Identifier = "and" }, 9),
        // TODO: add the rest of keywords tests!

        // eof
        Token.init(TokenType.token_eof, "", LiteralToken.None, 9),
    };
    {
        var scanner = try Scanner.init(testing.allocator, source[0..]);
        defer scanner.deinit();

        const actTokens = try scanner.scanTokens();
        defer testing.allocator.free(actTokens);
        for (actTokens) |token, index| {
            try expect(tokenEql(token, tokens[index]));
        }
    }
}

fn tokenEql(this: Token, that: Token) bool {
    // warn("\nthis lexeme: {s}, line: {d} \nthat lexeme: {s}, line: {d}", .{ this.lexeme, this.line, that.lexeme, that.line });
    return this.eql(that);
}
