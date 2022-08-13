const std = @import("std");
const SinglyLinkedList = std.SinglyLinkedList;
const testing = std.testing;
const warn = std.log.warn;

const Token = struct {
    value: []const u8,
};

const Scanner = struct {
    source: []const u8,
    tokens: SinglyLinkedList(Token),

    const Self = @This();

    pub fn init(source: []const u8) Self {
        return Self{
            .source = source,
            .tokens = SinglyLinkedList(Token){},
        };
    }

    pub fn scanTokens(self: Self) !SinglyLinkedList(Token){
        return self.tokens;
    }
};

test "init" {
    const source = "source";
    {
        const scanner = Scanner.init(source[0..]);
        const tokens = try scanner.scanTokens();
        var it = tokens.first;
        while(it) |token|: (it = token.next) {
            warn("token {s}", .{token.data.value});
        }
    }
}
