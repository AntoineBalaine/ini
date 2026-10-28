const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    _ = b.addModule("ini", .{
        .root_source_file = b.path("src/ini.zig"),
        .optimize = optimize,
        .target = target,
    });

    const ini_c_header = b.addTranslateC(.{
        .root_source_file = b.path("src/ini.h"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .name = "ini",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/lib.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .imports = &.{
                .{
                    .name = "c",
                    .module = ini_c_header.createModule(),
                },
            },
        }),
    });
    lib.bundle_compiler_rt = true;
    lib.root_module.addIncludePath(b.path("src"));
    lib.installHeader(b.path("src/ini.h"), "ini.h");
    b.installArtifact(lib);

    const example_step = b.step("example", "Build examples");
    const example_c = b.addExecutable(.{
        .name = "example-c",
        .root_module = b.createModule(.{
            .optimize = optimize,
            .target = target,
            .link_libc = true,
        }),
    });
    example_c.root_module.addCSourceFile(.{
        .file = b.path("example/example.c"),
        .flags = &.{
            "-Wall",
            "-Wextra",
            "-pedantic",
        },
    });
    example_c.root_module.addIncludePath(b.path("src"));
    example_c.root_module.linkLibrary(lib);
    example_step.dependOn(&b.addInstallArtifact(example_c, .{}).step);

    const example_zig = b.addExecutable(.{
        .name = "example-zig",
        .root_module = b.createModule(.{
            .root_source_file = b.path("example/example.zig"),
            .optimize = optimize,
            .target = target,
            .imports = &.{
                .{ .name = "ini", .module = b.modules.get("ini").? },
            },
        }),
    });
    example_step.dependOn(&b.addInstallArtifact(example_zig, .{}).step);

    const test_step = b.step("test", "Run library tests");
    const main_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test.zig"),
            .optimize = optimize,
            .target = target,
        }),
    });
    test_step.dependOn(&b.addRunArtifact(main_tests).step);

    const binding_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/lib-test.zig"),
            .optimize = optimize,
            .target = target,
            .link_libc = true,
            .imports = &.{
                .{
                    .name = "c",
                    .module = ini_c_header.createModule(),
                },
            },
        }),
    });
    binding_tests.root_module.addIncludePath(b.path("src"));
    binding_tests.root_module.linkLibrary(lib);
    test_step.dependOn(&b.addRunArtifact(binding_tests).step);
}
