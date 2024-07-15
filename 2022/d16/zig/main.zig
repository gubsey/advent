const std = @import("std");

pub fn main() !void {
    const map = try load(std.heap.page_allocator);

    var it = map.iterator();
    while (it.next()) |entry| {
        std.debug.print("{s}", .{entry});
    }
}

fn load(alloc: std.mem.Allocator) !std.StringHashMap(Valve) {
    const buf = try std.fs.cwd()
        .readFileAlloc(alloc, "../example.txt", 99999999);

    var map = std.StringHashMap(Valve).init(alloc);

    var lines = std.mem.tokenizeAny(u8, buf, "\n");
    while (lines.next()) |line| {
        var it = std.mem.tokenizeAny(u8, line, " =;");
        var wordslist = std.ArrayList([]const u8).init(alloc);
        while (it.next()) |word| {
            try wordslist.append(word);
        }
        var words = wordslist.items;

        const name = words[1];
        const rate = try std.fmt.parseInt(i32, words[5], 10);
        var leads = std.ArrayList([]const u8).init(alloc);
        try leads.appendSlice(words[10..]);

        try map.put(name, .{ .flow_rate = rate, .tunnels = leads });
    }

    return map;
}

const Valve = struct {
    flow_rate: i32,
    tunnels: std.ArrayList([]const u8),
};
