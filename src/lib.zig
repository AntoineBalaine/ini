const std = @import("std");
const ini = @import("ini.zig");
const c = @import("c");

const Record = extern struct {
    type: Type,
    value: Data,

    const Type = enum(c.ini_RecordType) {
        nul = 0,
        section = 1,
        property = 2,
        enumeration = 3,
    };

    const Data = extern union {
        section: [*:0]const u8,
        property: KeyValuePair,
        enumeration: [*:0]const u8,
    };

    const KeyValuePair = extern struct {
        key: [*:0]const u8,
        value: [*:0]const u8,
    };
};

const BufferParser = struct {
    stream: std.Io.Reader,
    parser: ini.Parser,
};

const FileParser = struct {
    reader: CReader,
    parser: ini.Parser,
};

const IniParser = union(enum) {
    buffer: BufferParser,
    file: FileParser,
};

const IniError = enum(c.ini_Error) {
    success = 0,
    out_of_memory = 1,
    io = 2,
    invalid_data = 3,
};

comptime {
    if (@sizeOf(c.ini_Parser) < @sizeOf(IniParser))
        @compileError(std.fmt.comptimePrint("ini_Parser struct in header is too small. Please set the char array to at least {d} chars!", .{@sizeOf(IniParser)}));
    if (@alignOf(c.ini_Parser) < @alignOf(IniParser))
        @compileError("align mismatch: ini_Parser struct does not match IniParser");

    if (@sizeOf(c.ini_Record) != @sizeOf(Record))
        @compileError("size mismatch: ini_Record struct does not match Record!");
    if (@alignOf(c.ini_Record) != @alignOf(Record))
        @compileError("align mismatch: ini_Record struct does not match Record!");

    if (@sizeOf(c.ini_KeyValuePair) != @sizeOf(Record.KeyValuePair))
        @compileError("size mismatch: ini_KeyValuePair struct does not match Record.KeyValuePair!");
    if (@alignOf(c.ini_KeyValuePair) != @alignOf(Record.KeyValuePair))
        @compileError("align mismatch: ini_KeyValuePair struct does not match Record.KeyValuePair!");
}

export fn ini_create_buffer(parser: *IniParser, data: [*]const u8, data_length: usize, comment_characters: [*]const u8, comment_characters_length: usize) void {
    parser.* = IniParser{
        .buffer = .{
            .stream = std.Io.Reader.fixed(data[0..data_length]),
            .parser = undefined,
        },
    };
    // this is required to have the parser store a pointer to the stream.
    parser.buffer.parser = ini.parse(std.heap.c_allocator, &parser.buffer.stream, comment_characters[0..comment_characters_length]);
}

export fn ini_create_file(parser: *IniParser, read_buffer: [*]u8, read_buffer_length: usize, file: *std.c.FILE, comment_characters: [*]const u8, comment_characters_length: usize) void {
    parser.* = IniParser{
        .file = .{
            .reader = CReader.init(file, read_buffer[0..read_buffer_length]),
            .parser = undefined,
        },
    };

    parser.file.parser = ini.parse(std.heap.c_allocator, &parser.file.reader.interface, comment_characters[0..comment_characters_length]);
}

export fn ini_destroy(parser: *IniParser) void {
    switch (parser.*) {
        .buffer => |*p| p.parser.deinit(),
        .file => |*p| p.parser.deinit(),
    }
    parser.* = undefined;
}

const ParseError = error{ OutOfMemory, StreamTooLong } || std.Io.Reader.Error || std.Io.Writer.Error;

fn mapError(err: ParseError) IniError {
    return switch (err) {
        error.OutOfMemory => IniError.out_of_memory,
        error.StreamTooLong => IniError.invalid_data,
        else => IniError.io,
    };
}

export fn ini_next(parser: *IniParser, record: *Record) IniError {
    const src_record_or_null: ?ini.Record = switch (parser.*) {
        .buffer => |*p| p.parser.next() catch |e| return mapError(e),
        .file => |*p| p.parser.next() catch |e| return mapError(e),
    };

    if (src_record_or_null) |src_record| {
        record.* = switch (src_record) {
            .section => |heading| Record{
                .type = .section,
                .value = .{ .section = heading.ptr },
            },
            .enumeration => |enumeration| Record{
                .type = .enumeration,
                .value = .{ .enumeration = enumeration.ptr },
            },
            .property => |property| Record{
                .type = .property,
                .value = .{ .property = .{
                    .key = property.key.ptr,
                    .value = property.value.ptr,
                } },
            },
        };
    } else {
        record.* = Record{
            .type = .nul,
            .value = undefined,
        };
    }

    return .success;
}

extern "c" fn feof(stream: *std.c.FILE) c_int;

const CReader = struct {
    file: *std.c.FILE,
    interface: std.Io.Reader,

    fn init(file: *std.c.FILE, buffer: []u8) CReader {
        return .{
            .file = file,
            .interface = .{
                .vtable = &.{ .stream = stream },
                .buffer = buffer,
                .seek = 0,
                .end = 0,
            },
        };
    }

    fn stream(r: *std.Io.Reader, w: *std.Io.Writer, limit: std.Io.Limit) std.Io.Reader.StreamError!usize {
        const creader: *CReader = @alignCast(@fieldParentPtr("interface", r));

        if (limit == .nothing) return 0;
        const dest = limit.slice(try w.writableSliceGreedy(1));

        const n = std.c.fread(dest.ptr, 1, dest.len, creader.file);
        if (n > 0) {
            w.advance(n);
            return n;
        }

        if (feof(creader.file) != 0) return error.EndOfStream;
        return error.ReadFailed;
    }
};
