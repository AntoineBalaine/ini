const std = @import("std");

/// An entry in a ini file. Each line that contains non-whitespace text can
/// be categorized into a record type.
pub const Record = union(enum) {
    /// A section heading enclosed in `[` and `]`. The brackets are not included.
    section: [:0]const u8,

    /// A line that contains a key-value pair separated by `=`.
    /// Both key and value have the excess whitespace trimmed.
    /// Both key and value allow escaping with C string syntax.
    property: KeyValue,

    /// A line that is either escaped as a C string or contains no `=`
    enumeration: [:0]const u8,
};

pub const KeyValue = struct {
    key: [:0]const u8,
    value: [:0]const u8,
};

const whitespace = " \r\t\x00";

/// WARNING:
/// This function is not a general purpose function but
/// requires to be executed on slices of the line_buffer *after*
/// the NUL terminator appendix.
/// This function will override the character after the slice end,
/// so make sure there is one available!
fn insertNulTerminator(slice: []const u8) [:0]const u8 {
    const mut_ptr = @as([*]u8, @ptrFromInt(@intFromPtr(slice.ptr)));
    mut_ptr[slice.len] = 0;
    return mut_ptr[0..slice.len :0];
}

pub fn Parser(comptime Reader: type) type {
    return struct {
        const Self = @This();

        line_buffer: std.ArrayList(u8),
        reader: Reader,

        pub fn deinit(self: *Self) void {
            self.line_buffer.deinit();
            self.* = undefined;
        }

        pub fn next(self: *Self) !?Record {
            while (true) {
                self.reader.readUntilDelimiterArrayList(&self.line_buffer, '\n', 4096) catch |err| switch (err) {
                    error.EndOfStream => {
                        if (self.line_buffer.items.len == 0)
                            return null;
                    },
                    else => |e| return e,
                };
                try self.line_buffer.append(0); // append guaranteed space for sentinel

                const line = if (std.mem.indexOfAny(u8, self.line_buffer.items, ";#")) |index|
                    std.mem.trim(u8, self.line_buffer.items[0..index], whitespace)
                else
                    std.mem.trim(u8, self.line_buffer.items, whitespace);
                if (line.len == 0)
                    continue;

                if (std.mem.startsWith(u8, line, "[") and std.mem.endsWith(u8, line, "]")) {
                    return Record{ .section = insertNulTerminator(line[1 .. line.len - 1]) };
                }

                if (std.mem.indexOfScalar(u8, line, '=')) |index| {
                    return Record{
                        .property = KeyValue{
                            // note: the key *might* replace the '=' in the slice with 0!
                            .key = insertNulTerminator(std.mem.trim(u8, line[0..index], whitespace)),
                            .value = insertNulTerminator(std.mem.trim(u8, line[index + 1 ..], whitespace)),
                        },
                    };
                }

                return Record{ .enumeration = insertNulTerminator(line) };
            }
        }
    };
}

pub const State = enum { normal, section, key, value, comment };

/// Returns a new parser that can read the ini structure
pub fn parse(allocator: std.mem.Allocator, reader: anytype) Parser(@TypeOf(reader)) {
    return Parser(@TypeOf(reader)){
        .line_buffer = std.ArrayList(u8).init(allocator),
        .reader = reader,
    };
}

const StructError = error{
    NotAStruct,
};

const truthyAndFalsy = std.StaticStringMap(bool).initComptime(.{ .{ "true", true }, .{ "false", false }, .{ "1", true }, .{ "0", false } });

pub fn convert(comptime T: type, val: []const u8) !T {
    return switch (@typeInfo(T)) {
        .Int, .ComptimeInt => try std.fmt.parseInt(T, val, 0),
        .Float, .ComptimeFloat => try std.fmt.parseFloat(T, val),
        .Bool => truthyAndFalsy.get(val).?,
        else => @as(T, val),
    };
}

pub fn readToStruct(comptime T: type, parser: anytype) !T {
    std.debug.assert(@typeInfo(T) == .Struct);
    var cur_section: []const u8 = "";

    const ret_struct = std.mem.zeroes(T);

    while (try parser.next()) |record| {
        switch (record) {
            .section => |heading| {
                cur_section = heading;
            },
            .property => |kv| {
                const key = kv.key;
                const value = kv.value;
                inline for (std.meta.fields(T)) |ns_info| {
                    // if we find the current section name
                    if (std.mem.eql(u8, ns_info.name, cur_section)) {
                        // @field(ret, ns_info.name) contains the inner struct now
                        // loop over the fields of the inner struct, and check for key matches
                        const innerStruct = @field(ret_struct, ns_info.name);
                        inline for (std.meta.fields(@TypeOf(innerStruct))) |key_info| {
                            const field_name = key_info.name;
                            // if we find the current key
                            if (std.mem.eql(u8, field_name, key)) {
                                // now we have a key match, give it the value
                                const my_type = @TypeOf(@field(innerStruct, field_name));
                                @field(innerStruct, field_name) = try convert(my_type, value);
                            }
                        }
                    }
                }
            },
            .enumeration => |value| {
                _ = value;
            },
        }
    }
    return ret_struct;
}

test readToStruct {
    const expect = std.testing.expect;
    const NewConfig = struct {
        //
        core: struct {
            //
            repositoryformatversion: isize,
            filemode: bool,
            bare: bool,
            logallrefupdates: bool,
        },
    };
    const example =
        \\ [core]
        \\ 	repositoryformatversion = 0
        \\ 	filemode = true
        \\ 	bare = false
        \\ 	logallrefupdates = true
    ;
    var fbs = std.io.fixedBufferStream(example);
    var parser = parse(std.testing.allocator, fbs.reader());
    defer parser.deinit();
    const result = try readToStruct(NewConfig, parser);
    try expect(result.core.repositoryformatversion == 0);
    try expect(result.core.filemode == true);
    try expect(result.core.bare == false);
    try expect(result.core.logallrefupdates == true);
}
