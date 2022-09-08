const std = @import("std");

pub const Lox = struct {
    hadError: bool,

    const Self = @This();
    pub fn init(args: [][:0]u8) Self {
        _ = args;
    }
};