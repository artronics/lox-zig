const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const HashMap = std.AutoHashMap;
const StringHashMap = std.StringHashMap;
const warn = std.log.warn;
const expect = std.testing.expect;
const expectError = std.testing.expectError;
const StringBuilder = @import("string_builder.zig").StringBuilder;

pub const Token = struct {
    lexeme: []const u8,
    tag: Tag,

    pub fn eql(self: Token, other: Token) bool {
        if (self.tag != other.tag) return false;
        if (self.lexeme.ptr != other.lexeme.ptr) return false;
        return std.mem.eql(u8, self.lexeme, other.lexeme);
    }

    pub fn print(self: Token, buf: []u8) ![]const u8 {
        return std.fmt.bufPrint(buf, "{s}:{s}%", .{ @tagName(self.tag), self.lexeme });
    }

    pub fn allocPrint(self: Token, allocator: Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{s}:{s}%", .{ @tagName(self.tag), self.lexeme });
    }
};

pub const Literal = union(enum) {
    l_identifier: []const u8,
    l_string: []const u8,
    l_number: f64,
    // TODO: bool is not used as literal since token covers the value (like keywords). Should we add it for consistency?
    l_bool: bool,

    // TODO: it doesn't check the slice original (ptr). The behaviour is inconsistent with the Token.eql
    // This means two literals can be equal even coming from different sources.
    pub fn eql(self: Literal, other: Literal) bool {
        if (@enumToInt(self) != @enumToInt(other)) return false;
        return switch (self) {
            .l_identifier => |v| std.mem.eql(u8, v, other.l_identifier),
            .l_string => |v| std.mem.eql(u8, v, other.l_string),
            .l_number => |v| v == other.l_number,
            .l_bool => |v| v == other.l_bool,
        };
    }

    pub fn allocPrint(self: Literal, allocator: Allocator) ![]const u8 {
        return switch (self) {
            .l_identifier => |v| try std.fmt.allocPrint(allocator, "identifier:{s}%", .{v}),
            .l_string => |v| try std.fmt.allocPrint(allocator, "string:{s}%", .{v}),
            .l_number => |v| try std.fmt.allocPrint(allocator, "number:{d}%", .{v}),
            .l_bool => |v| try std.fmt.allocPrint(allocator, "bool:{any}%", .{v}),
        };
    }
};

pub const Tag = enum {
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
    // TODO: remove _lit
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
    // TODO: probably better to use slice here.
    tokens: ArrayList(Token),
    literals: HashMap(usize, Literal),

    // FIXME: Because of HashMap deinit, we can't create a const of Tokens
    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.literals.deinit();
    }

    pub fn literal(self: *const Self, token: ?u32) ?Literal {
        if (token) |t| {
            return self.literals.get(t);
        }
        return null;
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

    pub fn printAll(self: Self, allocator: Allocator) ![]const u8 {
        const SB = @import("string_builder.zig").StringBuilder;
        var sb = try SB.init(2000, allocator);
        defer sb.deinit();

        try sb.append2("tokens and literals:\n", .{});
        for (self.tokens.items) |t, i| {
            const ts = try t.allocPrint(allocator);
            defer allocator.free(ts);
            try sb.append2("{d}: {s}\n", .{ i, ts });

            if (self.literals.get(i)) |lit| {
                const ls = try lit.allocPrint(allocator);
                defer allocator.free(ls);
                try sb.append2("|--literal {s}\n", .{ls});
            }
        }

        const str = sb.string();
        const dst = try allocator.alloc(u8, str.len);
        std.mem.copy(u8, dst, str);

        return dst;
    }
};

test "Tokens" {
    const a = std.testing.allocator;
    {
        var ts = ArrayList(Token).init(a);
        var ls = HashMap(usize, Literal).init(a);
        const source = "\n\n  \n    \"foo\""; // line = 4 , column = 6
        const lexeme = source[10 .. 10 + 3];

        try ts.append(Token{ .lexeme = lexeme, .tag = Tag.t_lit_string });
        var tokens = Tokens{ .source = source, .tokens = ts, .literals = ls };
        defer tokens.deinit();

        const pos = tokens.pos(lexeme);
        try expect(pos.line == 4);
        try expect(pos.column == 6);
    }
    {
        var ts = ArrayList(Token).init(a);
        var ls = HashMap(usize, Literal).init(a);
        const source = "    \"foo\""; // line = 1 , column = 6
        const lexeme = source[5 .. 5 + 3];

        try ts.append(Token{ .lexeme = lexeme, .tag = Tag.t_lit_string });
        var tokens = Tokens{ .source = source, .tokens = ts, .literals = ls };
        defer tokens.deinit();

        const pos = tokens.pos(lexeme);
        try expect(pos.line == 1);
        try expect(pos.column == 6);
    }
    { // literal
        var ts = ArrayList(Token).init(a);
        var ls = HashMap(usize, Literal).init(a);
        const source = "3.4 bar \"foo\"";
        const num = source[0 .. 3];
        const id = source[4..7];
        const str = source[9..12];
        const l_num = Literal{.l_number = 3.4};
        const l_id = Literal{.l_identifier = "bar"};
        const l_str = Literal{.l_string = "foo"};

        try ts.append(Token{ .lexeme = num, .tag = Tag.t_lit_number });
        try ts.append(Token{ .lexeme = id, .tag = Tag.t_lit_identifier });
        try ts.append(Token{ .lexeme = str, .tag = Tag.t_lit_string });
        try ls.put(0, l_num);
        try ls.put(1, l_id);
        try ls.put(2, l_str);

        var tokens = Tokens{ .source = source, .tokens = ts, .literals = ls };
        defer tokens.deinit();

        try expect(tokens.literal(0).?.eql(l_num));
        try expect(tokens.literal(1).?.eql(l_id));
        try expect(tokens.literal(2).?.eql(l_str));
    }
}
