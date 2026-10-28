const std = @import("std");
const ini = @import("ini");

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const file = try std.Io.Dir.cwd().openFile(io, "example.ini", .{});
    defer file.close(io);

    var read_buffer: [1024]u8 = undefined;
    var file_reader = file.reader(io, &read_buffer);
    var parser = ini.parse(init.gpa, &file_reader.interface, ";#");
    defer parser.deinit();

    var write_buffer: [1024]u8 = undefined;
    var file_writer = std.Io.File.stdout().writer(io, &write_buffer);
    const writer = &file_writer.interface;
    defer writer.flush() catch @panic("Could not flush to stdout");

    while (try parser.next()) |record| {
        switch (record) {
            .section => |heading| try writer.print("[{s}]\n", .{heading}),
            .property => |kv| try writer.print("{s} = {s}\n", .{ kv.key, kv.value }),
            .enumeration => |value| try writer.print("{s}\n", .{value}),
        }
    }
}
