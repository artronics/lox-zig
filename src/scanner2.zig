const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const HashMap = std.AutoHashMap;
const StringHashMap = std.StringHashMap;
const warn = std.log.warn;
const expect = std.testing.expect;
const expectError = std.testing.expectError;

pub const Token = struct {
    lexeme: []const u8,
    tag: TokenTag,

    pub fn eql(self: Token, other: Token) bool {
        if (self.tag != other.tag) return false;
        if (self.lexeme.ptr != other.lexeme.ptr) return false;
        return std.mem.eql(u8, self.lexeme, other.lexeme);
    }

    pub fn print(self: Token, buf: []u8) ![]const u8 {
        return std.fmt.bufPrint(buf, "{s}:{s}%", .{ @tagName(self.tag), self.lexeme });
    }
};

const TokenLiteral = union(enum) {
    l_identifier: []const u8,
    l_string: []const u8,
    l_number: f64,
    l_bool: bool,

    // TODO: it doesn't check the slice original (ptr). The behaviour is inconsistent with the Token.eql
    fn eql(self: TokenLiteral, other: TokenLiteral) bool {
        if (@enumToInt(self) != @enumToInt(other)) return false;
        return switch (self) {
            .l_identifier => |v| std.mem.eql(u8, v, other.l_identifier),
            .l_string => |v| std.mem.eql(u8, v, other.l_string),
            .l_number => |v| v == other.l_number,
            .l_bool => |v| v == other.l_bool,
        };
    }
};

const TokenTag = enum {
    // Single-character tokens.
    t_left_paren,
    t_right_paren,
    t_left_brace,
    t_right_brace,
    t_comma,
    t_dot,
    t_minus,
    t_plus,
    t_semicolon,
    t_slash,
    t_star,
    // One or two character tokens.
    t_bang,
    t_bang_equal,
    t_equal,
    t_equal_equal,
    t_greater,
    t_greater_equal,
    t_less,
    t_less_equal,
    // Literals.
    t_lit_identifier,
    t_lit_string,
    t_lit_number,
    // Keywords.
    t_and,
    t_class,
    t_else,
    t_false,
    t_for,
    t_fun,
    t_if,
    t_nil,
    t_or,
    t_print,
    t_return,
    t_super,
    t_this,
    t_true,
    t_var,
    t_while,

    t_error,
    t_eof,
};

pub const Tokens = struct {
    const Self = @This();

    pub const Pos = struct {
        line: u32,
        column: u32,
    };
    // allocator: Allocator,
    source: []const u8,
    tokens: ArrayList(Token),
    literals: HashMap(usize, TokenLiteral),

    // FIXME: Because of HashMap deinit, we can't create a const of Tokens
    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.literals.deinit();
    }

    pub fn pos(self: Self, lexeme: []const u8) Tokens.Pos {
        const src_offset = @ptrToInt(self.source.ptr);
        const lex_offset = @ptrToInt(lexeme.ptr);
        const token_start_pos: usize = lex_offset - src_offset;

        return self.calcPos(token_start_pos);
    }

    fn calcPos(self: Self, to: usize) Tokens.Pos {
        var line: u32 = 1;
        for (self.source[0..to]) |ch| {
            if (ch == '\n') {
                line += 1;
            }
        }
        var col: u32 = 1; // take one because we need to account until \n
        var i: usize = to;
        while (self.source[i] != '\n' and i > 0) : (i -= 1) {
            col += 1;
        }
        if (self.source[i] == '\n') { // when col matches with \n it counts one extra
            col -= 1;
        }

        return Tokens.Pos{ .line = line, .column = col };
    }
};

test "Tokens" {
    const a = std.testing.allocator;
    {
        var ts = ArrayList(Token).init(a);
        var ls = HashMap(usize, TokenLiteral).init(a);
        const source = "\n\n  \n    \"foo\""; // line = 4 , column = 6
        const lexeme = source[10 .. 10 + 3];

        try ts.append(Token{ .lexeme = lexeme, .tag = TokenTag.t_lit_string });
        var tokens = Tokens{ .source = source, .tokens = ts, .literals = ls };
        defer tokens.deinit();

        const pos = tokens.pos(lexeme);
        try expect(pos.line == 4);
        try expect(pos.column == 6);
    }
    {
        var ts = ArrayList(Token).init(a);
        var ls = HashMap(usize, TokenLiteral).init(a);
        const source = "    \"foo\""; // line = 1 , column = 6
        const lexeme = source[5 .. 5 + 3];

        try ts.append(Token{ .lexeme = lexeme, .tag = TokenTag.t_lit_string });
        var tokens = Tokens{ .source = source, .tokens = ts, .literals = ls };
        defer tokens.deinit();

        const pos = tokens.pos(lexeme);
        try expect(pos.line == 1);
        try expect(pos.column == 6);
    }
}

pub const Error = error{
    unrecognised_token,
    unterminated_string,
} || Allocator.Error || std.fmt.ParseFloatError;

pub const Scanner = struct {
    const Self = @This();
    allocator: Allocator,
    source: []const u8,
    tokens: ArrayList(Token),
    literal_tokens: HashMap(usize, TokenLiteral),
    keywords: StringHashMap(TokenTag),

    start: usize = 0,
    current: usize = 0,

    pub fn init(allocator: Allocator, source: []const u8) Error!Self {
        var keywords = StringHashMap(TokenTag).init(allocator);
        try Scanner.populateKeywords(&keywords);

        return Self{
            .allocator = allocator,
            .source = source,
            .tokens = ArrayList(Token).init(allocator),
            .literal_tokens = HashMap(usize, TokenLiteral).init(allocator),
            .keywords = keywords,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.literal_tokens.deinit();
        self.keywords.deinit();
    }

    pub fn scanTokens(self: *Self) Error!Tokens {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }

        const src_len = self.source.len;
        try self.addTokenLexeme(self.source[src_len..src_len], TokenTag.t_eof);

        return Tokens{
            .source = self.source,
            .tokens = try self.tokens.clone(),
            .literals = try self.literal_tokens.clone(),
        };
    }

    fn scanToken(self: *Self) Error!void {
        const ch = self.advance();
        try switch (ch) {
            // One character
            '(' => try self.addToken(TokenTag.t_left_paren),
            ')' => try self.addToken(TokenTag.t_right_paren),
            '{' => try self.addToken(TokenTag.t_left_brace),
            '}' => try self.addToken(TokenTag.t_right_brace),
            ',' => try self.addToken(TokenTag.t_comma),
            '.' => try self.addToken(TokenTag.t_dot),
            '-' => try self.addToken(TokenTag.t_minus),
            '+' => try self.addToken(TokenTag.t_plus),
            ';' => try self.addToken(TokenTag.t_semicolon),
            '*' => try self.addToken(TokenTag.t_star),
            // Two characters
            '!' => try self.addToken(if (self.match('=')) TokenTag.t_bang_equal else TokenTag.t_bang),
            '=' => try self.addToken(if (self.match('=')) TokenTag.t_equal_equal else TokenTag.t_equal),
            '<' => try self.addToken(if (self.match('=')) TokenTag.t_less_equal else TokenTag.t_less),
            '>' => try self.addToken(if (self.match('=')) TokenTag.t_greater_equal else TokenTag.t_greater),
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                } else {
                    try self.addToken(TokenTag.t_slash);
                }
            },
            // Literals
            '"' => try self.string(),
            '0'...'9' => try self.number(),
            'a'...'z', 'A'...'Z', '_' => try self.identifier(),
            // Whitespace
            ' ', '\t', '\r', '\n' => {},

            else => Error.unrecognised_token,
        };
    }

    fn addToken(self: *Self, tag: TokenTag) Error!void {
        return self.addTokenLexeme(self.source[self.start..self.current], tag);
    }

    fn addTokenLexeme(self: *Self, lexeme: []const u8, tag: TokenTag) Error!void {
        const token = Token{ .lexeme = lexeme, .tag = tag };
        try self.tokens.append(token);
    }

    fn addTokenLiteral(self: *Self, lexeme: []const u8, tag: TokenTag) Error!void {
        const lit = switch (tag) {
            .t_lit_string => blk: {
                try self.addTokenLexeme(lexeme, TokenTag.t_lit_string);
                break :blk TokenLiteral{ .l_string = lexeme };
            },
            .t_lit_number => blk: {
                const value = try std.fmt.parseFloat(f64, lexeme);
                try self.addTokenLexeme(lexeme, TokenTag.t_lit_number);
                break :blk TokenLiteral{ .l_number = value };
            },
            .t_lit_identifier => blk: {
                try self.addTokenLexeme(lexeme, TokenTag.t_lit_identifier);
                break :blk TokenLiteral{ .l_identifier = lexeme };
            },
            else => unreachable,
        };
        const id = self.tokens.items.len - 1;
        try self.literal_tokens.put(id, lit);
    }

    fn string(self: *Self) Error!void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            return Error.unterminated_string;
        }
        _ = self.advance(); // closing "

        const lexeme = self.source[self.start + 1 .. self.current - 1];
        try self.addTokenLiteral(lexeme, TokenTag.t_lit_string);
    }

    fn number(self: *Self) Error!void {
        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            _ = self.advance(); // consume .
            while (std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        const lexeme = self.source[self.start..self.current];
        try self.addTokenLiteral(lexeme, TokenTag.t_lit_number);
    }

    fn identifier(self: *Self) Error!void {
        while (Scanner.isAlphaNumUnderscore(self.peek())) {
            _ = self.advance();
        }

        const lexeme = self.source[self.start..self.current];
        if (self.keywords.get(lexeme)) |tag| {
            try self.addToken(tag);
        } else {
            try self.addTokenLiteral(lexeme, TokenTag.t_lit_identifier);
        }
    }

    fn advance(self: *Self) u8 {
        defer self.current += 1;
        return self.source[self.current];
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

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    fn isAlphaNumUnderscore(ch: u8) bool {
        return std.ascii.isAlNum(ch) or ch == '_';
    }

    fn populateKeywords(map: *std.StringHashMap(TokenTag)) !void {
        try map.put("and", TokenTag.t_and);
        try map.put("class", TokenTag.t_class);
        try map.put("else", TokenTag.t_else);
        try map.put("false", TokenTag.t_false);
        try map.put("for", TokenTag.t_for);
        try map.put("fun", TokenTag.t_fun);
        try map.put("if", TokenTag.t_if);
        try map.put("nil", TokenTag.t_nil);
        try map.put("or", TokenTag.t_or);
        try map.put("print", TokenTag.t_print);
        try map.put("return", TokenTag.t_return);
        try map.put("super", TokenTag.t_super);
        try map.put("this", TokenTag.t_this);
        try map.put("true", TokenTag.t_true);
        try map.put("var", TokenTag.t_var);
        try map.put("while", TokenTag.t_while);
    }
};

test "scanner" {
    const a = std.testing.allocator;
    const source = "(){},.-+;/*! != = == > >= < <=\"foo bar\"\n\"foo\nbar\n\" 12.24 42 foo _foo123 _123 and";
    var scanner = try Scanner.init(a, source[0..]);
    defer scanner.deinit();

    var tokens = try scanner.scanTokens();
    defer tokens.deinit();
    const ts = tokens.tokens;
    const ls = tokens.literals;

    try expect(ts.items.len == 28);
    try expect(ls.count() == 7);

    try expect(ts.items[0].eql(Token{ .lexeme = source[0..1], .tag = TokenTag.t_left_paren }));
    try expect(ts.items[1].eql(Token{ .lexeme = source[1..2], .tag = TokenTag.t_right_paren }));
    try expect(ts.items[2].eql(Token{ .lexeme = source[2..3], .tag = TokenTag.t_left_brace }));
    try expect(ts.items[3].eql(Token{ .lexeme = source[3..4], .tag = TokenTag.t_right_brace }));
    try expect(ts.items[4].eql(Token{ .lexeme = source[4..5], .tag = TokenTag.t_comma }));
    try expect(ts.items[5].eql(Token{ .lexeme = source[5..6], .tag = TokenTag.t_dot }));
    try expect(ts.items[6].eql(Token{ .lexeme = source[6..7], .tag = TokenTag.t_minus }));
    try expect(ts.items[7].eql(Token{ .lexeme = source[7..8], .tag = TokenTag.t_plus }));
    try expect(ts.items[8].eql(Token{ .lexeme = source[8..9], .tag = TokenTag.t_semicolon }));
    try expect(ts.items[9].eql(Token{ .lexeme = source[9..10], .tag = TokenTag.t_slash }));
    try expect(ts.items[10].eql(Token{ .lexeme = source[10..11], .tag = TokenTag.t_star }));
    try expect(ts.items[11].eql(Token{ .lexeme = source[11..12], .tag = TokenTag.t_bang }));
    try expect(ts.items[12].eql(Token{ .lexeme = source[13..15], .tag = TokenTag.t_bang_equal }));
    try expect(ts.items[13].eql(Token{ .lexeme = source[16..17], .tag = TokenTag.t_equal }));
    try expect(ts.items[14].eql(Token{ .lexeme = source[18..20], .tag = TokenTag.t_equal_equal }));
    try expect(ts.items[15].eql(Token{ .lexeme = source[21..22], .tag = TokenTag.t_greater }));
    try expect(ts.items[16].eql(Token{ .lexeme = source[23..25], .tag = TokenTag.t_greater_equal }));
    try expect(ts.items[17].eql(Token{ .lexeme = source[26..27], .tag = TokenTag.t_less }));
    try expect(ts.items[18].eql(Token{ .lexeme = source[28..30], .tag = TokenTag.t_less_equal }));
    try expect(ts.items[19].eql(Token{ .lexeme = source[31..38], .tag = TokenTag.t_lit_string }));
    try expect(ls.get(19).?.eql(TokenLiteral{ .l_string = source[31..38] }));
    try expect(ts.items[20].eql(Token{ .lexeme = source[41..49], .tag = TokenTag.t_lit_string }));
    try expect(ls.get(20).?.eql(TokenLiteral{ .l_string = source[41..49] }));
    try expect(ts.items[21].eql(Token{ .lexeme = source[51..56], .tag = TokenTag.t_lit_number }));
    try expect(ls.get(21).?.eql(TokenLiteral{ .l_number = 12.24 }));
    try expect(ts.items[22].eql(Token{ .lexeme = source[57..59], .tag = TokenTag.t_lit_number }));
    try expect(ls.get(22).?.eql(TokenLiteral{ .l_number = 42 }));
    try expect(ts.items[23].eql(Token{ .lexeme = source[60..63], .tag = TokenTag.t_lit_identifier }));
    try expect(ls.get(23).?.eql(TokenLiteral{ .l_identifier = source[60..63] }));
    try expect(ts.items[24].eql(Token{ .lexeme = source[64..71], .tag = TokenTag.t_lit_identifier }));
    try expect(ls.get(24).?.eql(TokenLiteral{ .l_identifier = source[64..71] }));
    try expect(ts.items[25].eql(Token{ .lexeme = source[72..76], .tag = TokenTag.t_lit_identifier }));
    try expect(ls.get(25).?.eql(TokenLiteral{ .l_identifier = source[72..76] }));
    try expect(ts.items[26].eql(Token{ .lexeme = source[77..80], .tag = TokenTag.t_and }));
    try expect(ts.items[27].eql(Token{ .lexeme = source[source.len..source.len], .tag = TokenTag.t_eof }));
}

test "scanner errors" {
    const a = std.testing.allocator;
    {
        const source = "\"unterminated string";
        var scanner = try Scanner.init(a, source[0..]);
        defer scanner.deinit();

        var result = scanner.scanTokens();

        try expectError(Error.unterminated_string, result);
    }
    {
        const source = "🚀 unrecognised token";
        var scanner = try Scanner.init(a, source[0..]);
        defer scanner.deinit();

        var result = scanner.scanTokens();

        try expectError(Error.unrecognised_token, result);
    }
}
