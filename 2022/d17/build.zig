const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{ .name = "day17", .root_source_file = b.path("main.zig") });

    b.installArtifact(exe);

    
}
