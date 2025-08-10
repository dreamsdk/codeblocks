# Code::Blocks 25.03 for DreamSDK

![Code::Blocks for DreamSDK](./codeblocks/src/src/resources/start_here/title_1712.png)

[Code::Blocks](http://www.codeblocks.org/) is a free, open-source cross-platform IDE that supports various compilers.

[DreamSDK](https://dreamsdk.org "DreamSDK") is a modern, ready-to-use environment for the [Sega Dreamcast](https://en.wikipedia.org/wiki/Dreamcast) development, designed for the **Microsoft Windows** platform. It's a package composed by a lot of pre-compiled tools; and Code::Blocks is a nice IDE which unleash the power of **DreamSDK**.

**This repository hold a special version of the official Code::Blocks 25.03 stable release modified for adding full support of DreamSDK.**

Notables changes of this special release of **Code::Blocks** includes:

* New compiler/options file (`dc-gcc`) which specify the `GNU GCC Compiler for Sega Dreamcast` compiler.
* The `compiler` and `debugger` plugins have been patched to run the loader (i.e. `dc-tool`) before running the target.
* The `Sega Dreamcast Project` (`dc`) wizard template has been added.

If you are interested about Code::Blocks but not in Sega Dreamcast development using DreamSDK, then we can use this version if you want to, or you can simply use the regular Code::Blocks program.

**Note:** a new `sdk` (`codeblocks.dll`) module is needed to expose the required `CallHooks()` function in the *Squirrel* script, that's why almost all compiled binaries are provided in the generated patch.

## Introduction

This repository will allow you to build [Code::Blocks 25.03](https://www.codeblocks.org "Code::Blocks") for embedding it in the [Code::Blocks Patcher for DreamSDK](https://github.com/dreamsdk/codeblocks-patcher).

Code::Blocks 25.03 is available in both 32-bits and 64-bits. You will need to build both versions from the same source.
Unlike Code::Blocks 17.12 or 20.03, Code::Blocks 25.03 does not support Windows XP: it only works with Windows 10 and later.

In summary, the goal of this repository is to generate the following packages: 
- `.\packager\dist\codeblocks-25.03-dreamsdk-addon-bin-x86.7z` for 32-bit;
- `.\packager\dist\codeblocks-25.03-dreamsdk-addon-bin-x64.7z` for 64-bit.

This package will be embedded in the **Code::Blocks Patcher for DreamSDK** (`codeblocks-patcher.exe`).
This patcher is available in the [Code::Blocks Patcher for DreamSDK](https://github.com/dreamsdk/codeblocks-patcher) repository.

## Prerequisites

This section contains instructions to follow after the initial cloning is complete. 
Install all the prerequisites below before trying to work with this repository. Some are provided for convenience while others must be downloaded manually.

These **are** provided directly in this repository, under the `tools` directory:

- [GCC 14.2.0 (with POSIX threads) + LLVM/Clang/LLD/LLDB 19.1.7 + MinGW-w64 12.0.0 UCRT from WinLibs](https://winlibs.com/ "WinLibs")
- [Zip 3.00 from Info-ZIP](http://infozip.sourceforge.net/ "Info-ZIP").
- [Ultimate Packer for eXecutables](https://upx.github.io/ "UPX") (UPX).

These **are not** provided in this repository but could be easily downloaded:

- [7-Zip](http://www.7-zip.org).
- [Boost 1.87.0](http://www.boost.org/users/history/version_1_87_0.html).
- [Code::Blocks](https://www.codeblocks.org) (**yes, for building Code::Blocks you will need Code::Blocks**).

### Install WinLibs toolchains

Just unzip the 2 WinLibs toolchains in the drive root, usually `C:\`:
- 32-bit will be unzipped in `C:\mingw32`
- 64-bit will be unzipped in `C:\mingw64`

### Install other prerequisites

1. Make sure `zip` and `upx` are available in your `PATH` variable.
2. Install [7-Zip](http://www.7-zip.org) using the default settings and make sure `7z` is available in your `PATH` variable.
3. Install [Code::Blocks](https://www.codeblocks.org) with the default settings. It's easiest to install Code::Blocks in 64-bit if you plan to compile the 64-bit version; do the same for the 32-bit version, although it's not required. This document will assume you'll follow this rule.

### Building wxMSW

After installing all the prerequisites, you need to build **wxWidgets for Windows**, i.e. **wxMSW**. You only need to do that once; fortunately because this process is really very long (even if it would indeed be theoretically possible to use the `-jx` parameter where `x` is the number of jobs that could be launched in parallel, it is preferred not to do so to be sure of successful builds).

1. Open the `.\wxMSW\build.ini` file and adapt it as needed.
2. Double-click on the `.\wxMSW\build.cmd` file.

The `.\wxMSW\bin` directory will be created, that will contains both `debug` and `release` binaries, both on 32-bit and 64-bit.

### Installing Boost

Boost is used for some plugins in Code::Blocks, for example for the [Nassi–Shneiderman](https://wiki.codeblocks.org/index.php/NassiShneiderman_plugin) plugin.

This section explains how to install Boost for Code::Blocks; and it assumes that the 64-bit version is the one that will be built.

1. Unzip [boost 1.87.0](http://www.boost.org/users/history/version_1_87_0.html). The location where Boost is unzipped is called officially `$BOOST_ROOT`. Usually, `$BOOST_ROOT` will be set as `C:\Program Files\boost\boost_1_87_0`, but it could be `C:\boost_1_87_0` or whatever you want.
2. Open a Windows Command prompt.
3. Enter the following commands:

		set WINLIBS_ROOT=C:\mingw64
		set CODEBLOCKS_ROOT=C:\Program Files\CodeBlocks
		set BOOST_ROOT=C:\Program Files\boost\boost_1_87_0
		set PATH=%WINLIBS_ROOT%\bin\;%PATH%
		cd /D %BOOST_ROOT%
		bootstrap gcc
		b2 --toolset=gcc "--prefix=%CODEBLOCKS_ROOT%" boost.stacktrace.from_exception=off install

After running those commands, Boost is indeed installed, but we need to configure Code::Blocks IDE, as explained below.

### Configuring Code::Blocks IDE

To build **Code::Blocks** you will need **Code::Blocks**. Install the IDE and both toolchains if not already done (see above).

1. Start **Code::Blocks** then open the `.\codeblocks\codeblocks\src\CodeBlocks_wx32_64.workspace` file for 64-bit build or the `CodeBlocks_wx32.workspace` file for 32-bit build. This will open the `CodeBlocks Workspace wx3.2.x (64 bit)` workspace.
2. Select the **Settings** > **Global Variable** menu item in Code::Blocks. Select (or create) the following variables:
    - the `wx32_64` variable, enter `.\wxMSW` (e.g. `C:\codeblocks\wxMSW\bin\x64\release`) in the `base` field.
    - the `cb_release_type` variable and enter `-g -O0` in the `base` field.    
	- the `boost` variable. In the `base` directory field, enter `C:\Program Files\CodeBlocks`. In the `include` field, enter `C:\Program Files\CodeBlocks\include\boost-1_87` then in the `lib` field, enter `C:\Program Files\CodeBlocks\lib`.
3. Select the **Settings** > **Compiler** menu item in Code::Blocks, then select the `C:\mingw64` base directory for the `GNU GCC Compiler`.

## Debug build

### Making a Code::Blocks debug build

1. Make your changes in the **Code:Blocks** source (basically in `sdk`, `Compiler` and `Debugger` targets).
2. Select the **Settings** > **Global Variable** menu item then select the `cb_release_type` variable and enter `-g -O0` in the `base` field.
3. autorevision.h??
4. Rebuild the [the whole workspace](http://wiki.codeblocks.org/index.php/Installing_Code::Blocks_from_source_on_Windows).
5. Run the `.\codeblocks\src\update.bat` file.

### Debugging your Code::Blocks build

If you want to debug the **Code::Blocks** build, select the `src` target and install a **DreamSDK** working package in `E:\DreamSDK\`.
If you don't have an `E:` drive, you have to do some modifications:

1. Change the `DREAMSDK_HOME_DEBUG_DRIVE` variable in `.\packager\mkpkg.cmd`.
2. Change the `E:` drive reference in the two files below:

	- `.\codeblocks\src\plugins\compilergcc\resources\compilers\compiler_dc-gcc.xml`
	- `.\codeblocks\src\plugins\compilergcc\resources\compilers\options_dc-gcc.xml`

3. In the debugged **Code::Blocks**, go to the **Settings** > **Compiler** menu, 
select the **GNU GCC Compiler for Sega Dreamcast** profile and click on **Reset defaults**.
**Code::Blocks** should detect the **DreamSDK** package environment used for debug your **Code::Blocks** build.
4. The GNU Debugger (GDB) included in the latest release of **TDM-GCC** is buggy: some breakpoints are never reached. You should use a newer GNU Debugger (GDB) binary, for example the one included in **DreamSDK** (i.e. `E:\DreamSDK\bin\gdb.exe`). To change that, you may update the Debugger profile inside Code::Blocks (in the `Settings` menu).

## Release build

### Making a Code::Blocks release build

2. Select the **Settings** > **Global Variable** menu item then select the `cb_release_type` variable and enter `-O2` in the `base` field.
3. Change the content of the `.\codeblocks\src\include\autorevision.h` file. In normal conditions, this file is created automatically when using **SVN** and the `autorevision` tool. Or you may just create this `autorevision.h` file manually. The SVN revision `11983` is the official revision for the `25.03` release.

		/*11983*/
		//don't include this header, only configmanager-revision.cpp should do this.
		#ifndef AUTOREVISION_H
		#define AUTOREVISION_H
	
	
		#include <wx/string.h>
	
		namespace autorevision
		{
			const unsigned int svn_revision = 11983;
			const wxString svnRevision(_T("11983"));
			const wxString svnDate(_T("YYYY-MM-DD hh:mm:ss")); // update manually the date/time using this format
		}
	
	
	
		#endif


4. Rebuild the [the whole workspace](http://wiki.codeblocks.org/index.php/Installing_Code::Blocks_from_source_on_Windows).
5. Run the `.\codeblocks\src\update.bat` file.

### Making the package

After building the **Code::Blocks** release, you need to build the package that will be embedded in the **Code::Blocks Patcher for DreamSDK**.

1. Build Code::Blocks in Release mode (i.e. `cb_release_type` variable should be `-O2`) and run the `.\codeblocks\src\update.bat` file.
2. Go to the `.\packager` directory.
3. From there, run the `mkpkg.cmd` file.
4. Build the `.\cbpatcher\src\splash\codeblocks-splash.lpi` in **Release** mode from **Lazarus**, then pack the `.\cbpatcher\src\engine\embedded\codeblocks-splash.exe` file with **UPX**.
5. Build the `.\cbpatcher\src\codeblocks-patcher.lpi` in **Release** mode from **Lazarus**. No need to pack this with **UPX**.
6. Done! You should have now the `codeblocks-patcher.exe` file which can be embedded in the **DreamSDK Setup** file. Don't be surprised, it's a big file around `10MB`.

## FAQ

### Unable to debug the compiled Code::Blocks? ###

Sometimes, the breakpoints are never reached while compiling with **TDM-GCC-32** including **GCC 5.1.0**.

Please verify the following:

* The `cb_release_type` global variable should be set to `-g -O0` in order to activate debug symbols.
* Sometimes the **GDB** version included in **TDM-GCC-32** is buggy. Try to use another **GDB** build (like the one included in **DreamSDK** itself).

### Couldn't add an image to the image list. ###

This message is sometimes shown when starting the debug build of **Code::Blocks**. It caused by missing image files in the `.\codeblocks\src\devel\share\CodeBlocks\images\` directory.

To solve this issue, you just have to run the `.\codeblocks\src\update.bat` file, this will copy the missing files to the `devel` and `output` directories.
