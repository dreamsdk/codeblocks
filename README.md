# Code::Blocks

[Code::Blocks](http://www.codeblocks.org/) is a free, open-source cross-platform IDE that supports multiple compilers.

This repository contains a special version of the `17.12` stable release in order to provide full support for the **DreamSDK** package.

## Changes

Notables changes of this release includes:

* New compiler/options file (`dc-gcc`) which specify the `GNU GCC Compiler for Sega Dreamcast` compiler.
* The `compiler` and `debugger` plugins have been patched to run the loader (i.e. `dc-tool`) before running the target.
* The `Sega Dreamcast Project` (`dc`) wizard template has been added (**Note:** a new `sdk` (`codeblocks.dll`) module is needed to expose the required `CallHooks` function in the *Squirrel* script, that's why almost all compiled binaries are provided in the generated patch).

## Installing Boost

The last version of the **Boost** libraries supporting **Windows XP** is the `1.64.0` version. Starting from that version, Windows XP is unsupported.

1. Unzip [boost 1.64.0](https://dl.bintray.com/boostorg/release/1.64.0/source/boost_1_64_0.7z).
2. Open the [TDM-GCC-32](http://tdm-gcc.tdragon.net) prompt.
3. Enter the following commands:

		cd /D <path>\boost_1_64_0
		bootstrap gcc
		b2 --toolset=gcc "--prefix=C:\Program Files\CodeBlocks" install

## Building Code::Blocks

In **Code::Blocks**, specify the `boost` **Global Variable**:

1. In the `base` directory, enter `C:\Program Files\CodeBlocks`.
2. In `include`, enter `C:\Program Files\CodeBlocks\include\boost-1_64`.
3. In `lib`, enter `C:\Program Files\CodeBlocks\lib`.
4. In `cb_release_type`, enter `-O2` in the `base` field.
5. In `wx`, enter `./wxMSW` (e.g. `C:\codeblocks\wxMSW`) in the `base` field.

Then follow [the standard steps](http://wiki.codeblocks.org/index.php/Installing_Code::Blocks_from_source_on_Windows), basically you just need to build the whole tree.

## Making the package

After building this **Code::Blocks** release, you need to build the package that will be used in the **DreamSDK** Setup script.

This is detailed in the `./packager` directory.

