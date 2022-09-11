const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const HashMap = std.AutoHashMap;
const StringHashMap = std.StringHashMap;
const warn = std.log.warn;
const expect = std.testing.expect;
const expectError = std.testing.expectError;
const token = @import("token.zig");
const Token = token.Token;
const Tag = token.Tag;
const Tokens = token.Tokens;
const Literal = token.Literal;

pub const Error = error{
    unrecognised_token,
    unterminated_string,
} || Allocator.Error || std.fmt.ParseFloatError;

pub const Scanner = struct {
    const Self = @This();
    allocator: Allocator,
    source: []const u8,
    tokens: ArrayList(Token),
    literal_tokens: HashMap(usize, Literal),
    keywords: StringHashMap(Tag),

    start: usize = 0,
    current: usize = 0,

    pub fn init(allocator: Allocator, source: []const u8) Error!Self {
        var keywords = StringHashMap(Tag).init(allocator);
        try Scanner.populateKeywords(&keywords);

        return Self{
            .allocator = allocator,
            .source = source,
            .tokens = ArrayList(Token).init(allocator),
            .literal_tokens = HashMap(usize, Literal).init(allocator),
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
        try self.addTokenLexeme(self.source[src_len..src_len], Tag.t_eof);

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
            '(' => try self.addToken(Tag.t_left_paren),
            ')' => try self.addToken(Tag.t_right_paren),
            '{' => try self.addToken(Tag.t_left_brace),
            '}' => try self.addToken(Tag.t_right_brace),
            ',' => try self.addToken(Tag.t_comma),
            '.' => try self.addToken(Tag.t_dot),
            '-' => try self.addToken(Tag.t_minus),
            '+' => try self.addToken(Tag.t_plus),
            ';' => try self.addToken(Tag.t_semicolon),
            '*' => try self.addToken(Tag.t_star),
            // Two characters
            '!' => try self.addToken(if (self.match('=')) Tag.t_bang_equal else Tag.t_bang),
            '=' => try self.addToken(if (self.match('=')) Tag.t_equal_equal else Tag.t_equal),
            '<' => try self.addToken(if (self.match('=')) Tag.t_less_equal else Tag.t_less),
            '>' => try self.addToken(if (self.match('=')) Tag.t_greater_equal else Tag.t_greater),
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                } else {
                    try self.addToken(Tag.t_slash);
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

    fn addToken(self: *Self, tag: Tag) Error!void {
        return self.addTokenLexeme(self.source[self.start..self.current], tag);
    }

    fn addTokenLexeme(self: *Self, lexeme: []const u8, tag: Tag) Error!void {
        const t = Token{ .lexeme = lexeme, .tag = tag };
        try self.tokens.append(t);
    }

    fn addTokenLiteral(self: *Self, lexeme: []const u8, tag: Tag) Error!void {
        const lit = switch (tag) {
            .t_lit_string => blk: {
                try self.addTokenLexeme(lexeme, Tag.t_lit_string);
                break :blk Literal{ .l_string = lexeme };
            },
            .t_lit_number => blk: {
                const value = try std.fmt.parseFloat(f64, lexeme);
                try self.addTokenLexeme(lexeme, Tag.t_lit_number);
                break :blk Literal{ .l_number = value };
            },
            .t_lit_identifier => blk: {
                try self.addTokenLexeme(lexeme, Tag.t_lit_identifier);
                break :blk Literal{ .l_identifier = lexeme };
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
        try self.addTokenLiteral(lexeme, Tag.t_lit_string);
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
        try self.addTokenLiteral(lexeme, Tag.t_lit_number);
    }

    fn identifier(self: *Self) Error!void {
        while (Scanner.isAlphaNumUnderscore(self.peek())) {
            _ = self.advance();
        }

        const lexeme = self.source[self.start..self.current];
        if (self.keywords.get(lexeme)) |tag| {
            try self.addToken(tag);
        } else {
            try self.addTokenLiteral(lexeme, Tag.t_lit_identifier);
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

    fn populateKeywords(map: *std.StringHashMap(Tag)) !void {
        try map.put("and", Tag.t_and);
        try map.put("class", Tag.t_class);
        try map.put("else", Tag.t_else);
        try map.put("false", Tag.t_false);
        try map.put("for", Tag.t_for);
        try map.put("fun", Tag.t_fun);
        try map.put("if", Tag.t_if);
        try map.put("nil", Tag.t_nil);
        try map.put("or", Tag.t_or);
        try map.put("print", Tag.t_print);
        try map.put("return", Tag.t_return);
        try map.put("super", Tag.t_super);
        try map.put("this", Tag.t_this);
        try map.put("true", Tag.t_true);
        try map.put("var", Tag.t_var);
        try map.put("while", Tag.t_while);
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

    try expect(ts.items[0].eql(Token{ .lexeme = source[0..1], .tag = Tag.t_left_paren }));
    try expect(ts.items[1].eql(Token{ .lexeme = source[1..2], .tag = Tag.t_right_paren }));
    try expect(ts.items[2].eql(Token{ .lexeme = source[2..3], .tag = Tag.t_left_brace }));
    try expect(ts.items[3].eql(Token{ .lexeme = source[3..4], .tag = Tag.t_right_brace }));
    try expect(ts.items[4].eql(Token{ .lexeme = source[4..5], .tag = Tag.t_comma }));
    try expect(ts.items[5].eql(Token{ .lexeme = source[5..6], .tag = Tag.t_dot }));
    try expect(ts.items[6].eql(Token{ .lexeme = source[6..7], .tag = Tag.t_minus }));
    try expect(ts.items[7].eql(Token{ .lexeme = source[7..8], .tag = Tag.t_plus }));
    try expect(ts.items[8].eql(Token{ .lexeme = source[8..9], .tag = Tag.t_semicolon }));
    try expect(ts.items[9].eql(Token{ .lexeme = source[9..10], .tag = Tag.t_slash }));
    try expect(ts.items[10].eql(Token{ .lexeme = source[10..11], .tag = Tag.t_star }));
    try expect(ts.items[11].eql(Token{ .lexeme = source[11..12], .tag = Tag.t_bang }));
    try expect(ts.items[12].eql(Token{ .lexeme = source[13..15], .tag = Tag.t_bang_equal }));
    try expect(ts.items[13].eql(Token{ .lexeme = source[16..17], .tag = Tag.t_equal }));
    try expect(ts.items[14].eql(Token{ .lexeme = source[18..20], .tag = Tag.t_equal_equal }));
    try expect(ts.items[15].eql(Token{ .lexeme = source[21..22], .tag = Tag.t_greater }));
    try expect(ts.items[16].eql(Token{ .lexeme = source[23..25], .tag = Tag.t_greater_equal }));
    try expect(ts.items[17].eql(Token{ .lexeme = source[26..27], .tag = Tag.t_less }));
    try expect(ts.items[18].eql(Token{ .lexeme = source[28..30], .tag = Tag.t_less_equal }));
    try expect(ts.items[19].eql(Token{ .lexeme = source[31..38], .tag = Tag.t_lit_string }));
    try expect(ls.get(19).?.eql(Literal{ .l_string = source[31..38] }));
    try expect(ts.items[20].eql(Token{ .lexeme = source[41..49], .tag = Tag.t_lit_string }));
    try expect(ls.get(20).?.eql(Literal{ .l_string = source[41..49] }));
    try expect(ts.items[21].eql(Token{ .lexeme = source[51..56], .tag = Tag.t_lit_number }));
    try expect(ls.get(21).?.eql(Literal{ .l_number = 12.24 }));
    try expect(ts.items[22].eql(Token{ .lexeme = source[57..59], .tag = Tag.t_lit_number }));
    try expect(ls.get(22).?.eql(Literal{ .l_number = 42 }));
    try expect(ts.items[23].eql(Token{ .lexeme = source[60..63], .tag = Tag.t_lit_identifier }));
    try expect(ls.get(23).?.eql(Literal{ .l_identifier = source[60..63] }));
    try expect(ts.items[24].eql(Token{ .lexeme = source[64..71], .tag = Tag.t_lit_identifier }));
    try expect(ls.get(24).?.eql(Literal{ .l_identifier = source[64..71] }));
    try expect(ts.items[25].eql(Token{ .lexeme = source[72..76], .tag = Tag.t_lit_identifier }));
    try expect(ls.get(25).?.eql(Literal{ .l_identifier = source[72..76] }));
    try expect(ts.items[26].eql(Token{ .lexeme = source[77..80], .tag = Tag.t_and }));
    try expect(ts.items[27].eql(Token{ .lexeme = source[source.len..source.len], .tag = Tag.t_eof }));
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
