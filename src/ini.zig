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
const ConvertError = error{
    NotConvertible,
};

const truthyAndFalsy = std.StaticStringMap(bool).initComptime(.{ .{ "true", true }, .{ "false", false }, .{ "1", true }, .{ "0", false } });
// const truthyAndFalsy = std.ComptimeStringMap(bool, .{ .{ "true", true }, .{ "false", false }, .{ "1", true }, .{ "0", false } });

pub fn convert(comptime T: type, val: []const u8) !?T {
    if (T == []const u8 or T == []u8) return @as([]const u8, val);

    return switch (@typeInfo(T)) {
        .Int, .ComptimeInt => try std.fmt.parseInt(T, val, 10),
        .Float, .ComptimeFloat => try std.fmt.parseFloat(T, val),
        .Bool => truthyAndFalsy.get(val).?,
        else => null,
    };
}

pub fn readToEnumArray(enum_arr: anytype, Or_enum: type, parser: anytype, allocator: std.mem.Allocator, modulesList: ?*std.StringHashMap(Or_enum)) !void {
    const T = @TypeOf(enum_arr.*);
    std.debug.assert(@typeInfo(T) == .Struct);
    std.debug.assert(@typeInfo(Or_enum) == .Enum);

    // replace with parent enum
    var cur_section: ?Or_enum = null;

    while (try parser.*.next()) |record| {
        switch (record) {
            .section => |heading| {
                // fit the enum
                const head_val = std.meta.stringToEnum(Or_enum, heading);
                if (head_val != null) {
                    cur_section = head_val;
                }
            },
            .property => {},
            .enumeration => |value| {
                var it = enum_arr.*.iterator();
                var i: usize = 0;
                while (it.next() != null) : (i += 1) {
                    if (cur_section == null) {
                        continue;
                    }
                    const idx = T.Indexer.indexOf(cur_section.?);
                    if (i != idx) {
                        continue;
                    }
                    std.debug.print("in loop\n", .{});

                    const innerArray = enum_arr.get(cur_section.?);
                    const X = @TypeOf(innerArray);
                    if (X == std.ArrayList([]const u8) or X == std.ArrayList([]u8)) {
                        const value_copy = try allocator.dupe(u8, value);
                        var inn = enum_arr.getPtr(cur_section.?);
                        try inn.append(value_copy);
                    } else if (X == std.StringHashMap(void)) {
                        const value_copy = try allocator.dupe(u8, value);
                        var inn = enum_arr.getPtr(cur_section.?);
                        try inn.put(value_copy, {});
                        // FIXME: maybe this should have a dedicated match arm
                        if (modulesList != null) {
                            try modulesList.?.put(value_copy, cur_section.?);
                        }
                    } else if (X == []const u8) {
                        std.debug.print("val: {s}\n", .{value});
                        const value_copy = try allocator.dupe(u8, value);
                        enum_arr.set(cur_section.?, value_copy);
                    } else {
                        std.debug.print("\nfailed\n", .{});
                        return error.NotConvertible;
                    }
                }
            },
        }
    }
}

pub fn readToStruct(ret_struct: anytype, parser: anytype, allocator: std.mem.Allocator) !@TypeOf(ret_struct) {
    const T = @TypeOf(ret_struct.*);
    std.debug.assert(@typeInfo(T) == .Struct);
    var cur_section = std.ArrayList(u8).init(allocator);
    defer cur_section.deinit();

    while (try parser.*.next()) |record| {
        switch (record) {
            .section => |heading| {
                cur_section.clearRetainingCapacity();
                try cur_section.appendSlice(heading);
            },
            .property => |kv| {
                const key = kv.key;
                const value = kv.value;
                inline for (std.meta.fields(T)) |ns_info| {
                    // if we find the current section name
                    if (std.mem.eql(u8, ns_info.name, cur_section.items)) {
                        // @field(ret, ns_info.name) contains the inner struct now
                        // loop over the fields of the inner struct, and check for key matches
                        var innerStruct = &@field(ret_struct, ns_info.name); // err local var is never mutated
                        inline for (std.meta.fields(@TypeOf(innerStruct.*))) |key_info| {
                            const field_name = key_info.name;
                            // if we find the current key
                            if (std.mem.eql(u8, field_name, key)) {
                                // now we have a key match, give it the value
                                const my_type = @TypeOf(@field(innerStruct, field_name));
                                const conversion = try convert(my_type, value);
                                if (conversion) |converted| {
                                    @field(innerStruct, field_name) = converted;
                                }
                            }
                        }
                    }
                }
            },
            .enumeration => |value| {
                inline for (std.meta.fields(T)) |ns_info| {
                    // if we find the current section name
                    if (std.mem.eql(u8, ns_info.name, cur_section.items)) {
                        // @field(ret, ns_info.name) contains the inner struct now
                        var innerArray = &@field(ret_struct, ns_info.name);
                        const X = @TypeOf(innerArray.*);
                        if (X == std.ArrayList([]const u8) or X == std.ArrayList([]u8)) {
                            const value_copy = try allocator.dupe(u8, value);
                            try innerArray.append(value_copy);
                        } else if (X == std.StringHashMap(void)) {
                            const value_copy = try allocator.dupe(u8, value);
                            try innerArray.put(value_copy, {});
                        } else {
                            std.debug.print("\nfailed\n", .{});
                            return error.NotConvertible;
                        }
                    }
                }
            },
        }
    }
    return ret_struct;
}

test truthyAndFalsy {
    const expect = std.testing.expect;
    try expect(truthyAndFalsy.get("true") == true);
    try expect(truthyAndFalsy.get("false") == false);
    try expect(truthyAndFalsy.get("1") == true);
    try expect(truthyAndFalsy.get("0") == false);
}
test convert {
    const expect = std.testing.expect;
    const result = try convert(bool, "true");
    if (result) |val| {
        try expect(val == true);
    }
    const result2 = try convert([]const u8, "123");
    if (result2) |val2| {
        try expect(std.mem.eql(u8, val2, "123"));
    }
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
    const allocator = std.testing.allocator;
    var fbs = std.io.fixedBufferStream(example);
    var parser = parse(std.testing.allocator, fbs.reader());
    defer parser.deinit();

    var ret_struct = std.mem.zeroes(NewConfig);
    const result = try readToStruct(&ret_struct, &parser, allocator);
    try expect(result.core.repositoryformatversion == 0);
    try expect(result.core.filemode == true);
    try expect(result.core.bare == false);
    try expect(result.core.logallrefupdates == true);
}

test "nested structs" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    const Config2 = struct {
        first: struct {
            //
            repositoryformatversion: isize,
            filemode: bool,
            bare: bool,
            logallrefupdates: bool,
        },
        second: struct {
            second_thing1: isize,
            second_thing2: bool,
            second_thing3: []const u8,
        },
    };
    const example2 =
        \\ [first]
        \\ 	repositoryformatversion = 0
        \\ 	filemode = true
        \\ 	bare = false
        \\ 	logallrefupdates = true
        \\ [second]
        \\ 	second_thing1 = 1
        \\ 	second_thing2 = false
        \\ 	second_thing3 = hello
    ;
    var fbs2 = std.io.fixedBufferStream(example2);
    var parser2 = parse(std.testing.allocator, fbs2.reader());
    defer parser2.deinit();

    var ret_struct = std.mem.zeroes(Config2);
    const result2 = try readToStruct(&ret_struct, &parser2, allocator);
    try expect(result2.first.repositoryformatversion == 0);
    try expect(result2.first.filemode == true);
    try expect(result2.first.bare == false);
    try expect(result2.first.logallrefupdates == true);
    try expect(result2.second.second_thing1 == 1);
    try expect(result2.second.second_thing2 == false);
    try expect(std.mem.eql(u8, result2.second.second_thing3, "hello"));
}

test "nested array" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    const Config3 = struct {
        second: std.ArrayList([]const u8),
    };
    const example3 =
        \\ [second]
        \\ 	hello
        \\ 	world
    ;
    var fbs3 = std.io.fixedBufferStream(example3);
    var parser3 = parse(std.testing.allocator, fbs3.reader());
    defer parser3.deinit();
    var ret_struct = Config3{
        .second = std.ArrayList([]const u8).init(allocator),
    };
    const result3 = try readToStruct(&ret_struct, &parser3, allocator);
    defer {
        for (ret_struct.second.items) |item| {
            allocator.free(item);
        }
        ret_struct.second.deinit();
    }
    try expect(std.mem.eql(u8, result3.second.items[0], "hello"));
    try expect(std.mem.eql(u8, result3.second.items[1], "world"));
}

test "nested set" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;
    const config = struct {
        first: std.StringHashMap(void),
    };
    const example =
        \\ [first]
        \\ 	hello
        \\ 	world
    ;
    var ret_struct: config = .{
        .first = std.StringHashMap(void).init(allocator),
    };
    var fbs = std.io.fixedBufferStream(example);
    var parser = parse(std.testing.allocator, fbs.reader());
    defer parser.deinit();

    _ = try readToStruct(&ret_struct, &parser, allocator);
    defer {
        var iterator = ret_struct.first.iterator();
        while (iterator.next()) |item| {
            allocator.free(item.key_ptr.*);
        }
        ret_struct.first.deinit();
    }

    try expect(ret_struct.first.get("hello") != null);
    try expect(ret_struct.first.get("world") != null);
}

test "nested struct with array" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    const Config4 = struct {
        first: struct {
            //
            repositoryformatversion: isize,
            filemode: bool,
            bare: bool,
            logallrefupdates: bool,
        },
        second: std.ArrayList([]const u8),
    };
    const example4 =
        \\ [first]
        \\ 	repositoryformatversion = 0
        \\ 	filemode = true
        \\ 	bare = false
        \\ 	logallrefupdates = true
        \\ [second]
        \\ 	hello
        \\ 	world
    ;
    var fbs4 = std.io.fixedBufferStream(example4);
    var parser4 = parse(std.testing.allocator, fbs4.reader());
    defer parser4.deinit();

    var ret_struct = Config4{
        .first = .{
            .repositoryformatversion = 0,
            .filemode = true,
            .bare = false,
            .logallrefupdates = true,
        },
        .second = std.ArrayList([]const u8).init(allocator),
    };
    const result4 = try readToStruct(&ret_struct, &parser4, allocator);
    defer {
        for (ret_struct.second.items) |item| {
            allocator.free(item);
        }
        ret_struct.second.deinit();
    }
    try expect(result4.first.repositoryformatversion == 0);
    try expect(result4.first.filemode == true);
    try expect(result4.first.bare == false);
    try expect(result4.first.logallrefupdates == true);
    try expect(std.mem.eql(u8, result4.second.items[0], "hello"));
    try expect(std.mem.eql(u8, result4.second.items[1], "world"));
}

test "nested struct with enum array" {
    const myenum = enum {
        one,
        two,
        three,
    };
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;
    const example =
        \\[one]
        \\smth1
        \\[two]
        \\smth2
        \\[three]
        \\smth3
    ;

    var fbs = std.io.fixedBufferStream(example);
    var parser = parse(std.testing.allocator, fbs.reader());
    defer parser.deinit();

    var enum_arr = std.EnumArray(myenum, []const u8).initUndefined();
    _ = try readToEnumArray(&enum_arr, myenum, &parser, allocator);
    defer {
        inline for (std.meta.fields(myenum)) |f| {
            const val = enum_arr.get(std.meta.stringToEnum(myenum, f.name).?);
            allocator.free(val);
        }
    }
    expect(std.mem.eql(u8, enum_arr.get(.one), "smth1")) catch |err| {
        std.debug.print(
            "\n\n\n{s}\n\n\n",
            .{enum_arr.get(.one)},
        );
        return err;
    };
    try expect(std.mem.eql(u8, enum_arr.get(.two), "smth2"));
    try expect(std.mem.eql(u8, enum_arr.get(.three), "smth3"));
}
