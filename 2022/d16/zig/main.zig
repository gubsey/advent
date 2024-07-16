const std = @import("std");

pub fn main() !void {
    const map = try parse_valves(std.heap.page_allocator);
    defer map.deinit();

    var it = map.map.iterator();
    while (it.next()) |entry| {
        std.debug.print("{s}", .{entry});
    }
}

fn parse_valves(alloc: std.mem.Allocator) !std.StringHashMap(Valve) {
    const buf = try std.fs.cwd()
        .readFileAlloc(alloc, "../example.txt", 99999999);
    defer alloc.free(buf);

    var map = std.StringHashMap(Valve).init(alloc);

    var lines = std.mem.tokenizeAny(u8, buf, "\n");
    while (lines.next()) |line| {
        var it = std.mem.tokenizeAny(u8, line, " =;");

        var wordslist = std.ArrayList([]const u8).init(alloc);
        defer wordslist.deinit();

        while (it.next()) |word| {
            try wordslist.append(word);
        }

        const rate = try std.fmt.parseInt(i32, alloc.dupe(u8, wordslist.items[5]), 10);
        const leads = std.ArrayList([]const u8).fromOwnedSlice(alloc, wordslist.items[10..]);

        try map.put(alloc.dupe(u8, wordslist.items[1]), .{ .flow_rate = rate, .tunnels = leads.toOwnedSlice() });
    }

    return map;
}

const MyMap = struct {
    map: std.StringHashMap(Valve),
    strings: [][]const u8,

    fn deinit(self: *@This()) void {
        self.map.deinit();
        //self.strings.deinit();
    }
};

fn hash_string(str: std.ArrayList(u8)) void {
    std.hash.Fnv1a_32.hash(str.items);
}

test "parse values" {
    var map = try parse_valves(std.testing.allocator);
    std.debug.print("{s}", .{map.strings.items[0]});
    map.deinit();
}

const Valve = struct {
    flow_rate: i32,
    tunnels: std.ArrayList([]const u8),
};
