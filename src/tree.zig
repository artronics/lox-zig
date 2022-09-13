const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const warn = std.log.warn;

pub const Expr = enum {
    add,
    sub,

    pub fn print(self: Expr) []const u8 {
        _ = self;
        return "yo";
    }
};

fn printFn(e: Expr) void {
    const s = e.print();
    warn("print: {s}", .{s});
}

pub fn Node(comptime T: type) type {
    return union(enum) {
        const Self = @This();

        leaf: T,
        node: struct { l: *const Node(T), r: *const Node(T) },

        fn traverse(self: Self, f: fn (T) void) void {
            switch (self) {
                .leaf => |v| f(v),
                .node => |v| {
                    v.l.traverse(f);
                    v.r.traverse(f);
                },
            }
        }
    };
}

pub fn Tree(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        root: Node(T),

        fn traverse(self: *Self, f: fn (T) void) void {
            self.root.traverse(f);
        }
    };
}

test "Tree" {
    const a = std.testing.allocator;
    const MyNode = Node(Expr);
    const nl = MyNode{ .leaf = Expr.add };
    const nr = MyNode{ .leaf = Expr.sub };
    const n = MyNode{ .node = .{ .l = &nl, .r = &nr } };
    var t = Tree(Expr){ .allocator = a, .root = n };
    var b = struct {
        fn function(e: Expr) void {
            const s = e.print();
            _ = a;
            warn("print: {s}", .{s});
        }
    }.function;
    t.traverse(b);
    _ = t;
}
