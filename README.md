# Code::Blocks

[Code::Blocks](http://www.codeblocks.org/) is a free, open-source cross-platform IDE that supports multiple compilers.

This repository contains a special version of the `17.12` stable release in order to provide full support for the **DreamSDK** package.

## Changes

Notables changes of this release includes:

* New compiler/options file (`dc-gcc`) which specify the `GNU GCC Compiler for Sega Dreamcast` compiler.
* The `compiler` and `debugger` plugins have been patched to run the loader (i.e. `dc-tool`) before running the target.
* The `Sega Dreamcast Project` (`dc`) wizard template has been added (**Note:** a new `sdk` (`codeblocks.dll`) module is needed to expose the required `CallHooks` function in the *Squirrel* script, that's why almost all compiled binaries are provided in the generated patch).

## Introduction

Basically this repository goal is to build the **Code::Blocks Patcher for DreamSDK** (`codeblocks-patcher.exe`).
This patcher will be embedded in the **DreamSDK** Setup file.

Please read this document in order to generate the **Code::Blocks Patcher**.

## Prerequisites

* [7-Zip](http://www.7-zip.org)
* [Boost](https://dl.bintray.com/boostorg/release/1.64.0/source/boost_1_64_0.7z)
* [Code::Blocks](http://www.codeblocks.org)
* [TDM-GCC-32](http://tdm-gcc.tdragon.net)
* [Lazarus](http://www.lazarus-ide.org)
* [Zip from Info-ZIP](ftp://ftp.info-zip.org/pub/infozip/win32/zip300xn.zip)

## Installing Boost

The last version of the **Boost** libraries supporting **Windows XP** is the `1.64.0` version. Starting from that version, Windows XP is unsupported. To keep the XP support (as the original Code::Blocks 17.12 release) you must use that specific version. Please don't try to update it!

1. Unzip [boost 1.64.0](https://dl.bintray.com/boostorg/release/1.64.0/source/boost_1_64_0.7z).
2. Open the [TDM-GCC-32](http://tdm-gcc.tdragon.net) prompt.
3. Enter the following commands:

		cd /D <path>\boost_1_64_0
		bootstrap gcc
		b2 --toolset=gcc "--prefix=C:\Program Files\CodeBlocks" install

## Configuring Code::Blocks IDE

To build **Code::Blocks** you will need... **Code::Blocks**. Install the IDE and [TDM-GCC-32](http://tdm-gcc.tdragon.net) if not already done.

1. Start **Code::Blocks** then open the `.\codeblocks\codeblocks\src\CodeBlocks.workspace` file. This will open the `Code::Blocks wx2.8.x` workspace.
2. Select the **Settings** > **Global Variable** menu item then select (or create) the `boost` variable. In the `base` directory field, enter `C:\Program Files\CodeBlocks`. In the `include` field, enter `C:\Program Files\CodeBlocks\include\boost-1_64` then in the `lib` field, enter `C:\Program Files\CodeBlocks\lib`.
4. Select (or create) the `cb_release_type` variable and enter `-O2` in the `base` field.
5. Select (or create) the `wx` variable, enter `./wxMSW` (e.g. `C:\codeblocks\wxMSW`) in the `base` field. Please use the provided `wxMSW` directory provided in that repository.

## Building a Code::Blocks release

1. Make your changes in the **Code:Blocks** source (basically in `sdk`, `Compiler` and `Debugger` targets).
2. Rebuild the [the whole workspace](http://wiki.codeblocks.org/index.php/Installing_Code::Blocks_from_source_on_Windows).
3. Run the `.\codeblocks\src\update.bat` file.

## Debugging the Code::Blocks build

If you want to debug the **Code::Blocks** build, select the `src` target and install a **DreamSDK** working package in `E:\DreamSDK\`.
If you don't have an `E:` drive, you have to do some modifications:

1. Change the `DREAMSDK_HOME_DEBUG_DRIVE` variable in `.\packager\mkpkg.cmd`.
2. Change the `E:` drive reference in the two files below:

	- `.\codeblocks\src\plugins\compilergcc\resources\compilers\compiler_dc-gcc.xml`
	- `.\codeblocks\src\plugins\compilergcc\resources\compilers\options_dc-gcc.xml`

3. In the debugged **Code::Blocks**, go to the **Settings** > **Compiler** menu, 
select the **GNU GCC Compiler for Sega Dreamcast** profile and click on **Reset defaults**.
**Code::Blocks** should detect the **DreamSDK** package environment used for debug your **Code::Blocks** build.

## Making the package

After building the **Code::Blocks** release, you need to build the package that will be embedded in the **Code::Blocks Patcher for DreamSDK**.

1. Go to the `.\packager` directory.
2. From there, run the `mkpkg.cmd` file.
3. Build the `.\cbpatcher\src\splash\codeblocks-splash.lpi` in **Release** mode from **Lazarus**.
4. Build the `.\cbpatcher\src\codeblocks-patcher.lpi` in **Release** mode from **Lazarus**. Don't forget to pack it with **UPX**.
5. Done!

You have now the `codeblocks-patcher.exe` file which can be embedded in the **DreamSDK Setup** file.
