# Code::Blocks 17.12 for DreamSDK

[Code::Blocks](http://www.codeblocks.org/) is a free, open-source cross-platform IDE that supports multiple compilers. This repository contains **Code::Blocks 17.12 for DreamSDK**.

This special version of **Code::Blocks** provides full support for the **DreamSDK** package.

Notables changes/features of this special release of **Code::Blocks** includes:

* New compiler/options file (`dc-gcc`) which specify the `GNU GCC Compiler for Sega Dreamcast` compiler.
* The `compiler` and `debugger` plugins have been patched to run the loader (i.e. `dc-tool`) before running the target.
* The `Sega Dreamcast Project` (`dc`) wizard template has been added.
* The `Static Library` project template is also supported.

## Introduction

The goal of all this repository is to generate the following package: `.\packager\dist\codeblocks-17.12-dreamsdk-addon-bin-x86.7z`.

This package will be embedded in the **Code::Blocks Patcher for DreamSDK** (`codeblocks-patcher.exe`).
This patcher is available in the `codeblocks-patcher` repository.

The recipe to follow is:

1. Install prerequisites
2. Build wxMSW
3. Build a debug build of **Code::Blocks for DreamSDK** (and debug it)
4. Build a release build of **Code::Blocks for DreamSDK**
5. Make the final package (`codeblocks-17.12-dreamsdk-addon-bin-x86.7z`) that will be embedded in **Code::Blocks Patcher for DreamSDK** (`codeblocks-patcher.exe`)
6. Make the **Code::Blocks Patcher for DreamSDK** (`codeblocks-patcher.exe`) itself (see `codeblocks-patcher` repository)

## Prerequisites

Install all the following prerequisites before trying to work with this repository:

* [7-Zip](http://www.7-zip.org)
* [Boost](https://dl.bintray.com/boostorg/release/1.64.0/source/boost_1_64_0.7z)
* [DreamSDK](https://dreamsdk.org)
* [Code::Blocks](http://www.codeblocks.org)
* [TDM-GCC-32](http://tdm-gcc.tdragon.net)
* [Zip from Info-ZIP](http://infozip.sourceforge.net/Zip.html)

All of these prerequisites are available directly in the repository in the `.\prerequisites` directory.

### Installing Boost

The last version of the **Boost** libraries supporting **Windows XP** is the `1.64.0` version. After that version, Windows XP is unsupported. To keep the XP support (as the original Code::Blocks 17.12 release) you must use that specific version. Please don't try to update it!

1. Unzip [boost 1.64.0](https://dl.bintray.com/boostorg/release/1.64.0/source/boost_1_64_0.7z).
2. Open the [TDM-GCC-32](http://tdm-gcc.tdragon.net) prompt.
3. Enter the following commands:

		cd /D <path>\boost_1_64_0
		bootstrap gcc
		b2 --toolset=gcc "--prefix=C:\Program Files\CodeBlocks" install

### Installing DreamSDK for debugging this Code::Blocks edition

If you want to debug the **Code::Blocks for DreamSDK** package, you need to install **DreamSDK** in `E:\DreamSDK\`.
If you don't have an `E:` drive or doesn't want to use that `E:` drive, you will have to do some modifications:

1. Change the `DREAMSDK_HOME_DEBUG_DRIVE` variable in `.\packager\mkpkg.ini`.
2. Change the `E:` drive reference in the two files below:

	- `.\codeblocks\src\plugins\compilergcc\resources\compilers\compiler_dc-gcc.xml`
	- `.\codeblocks\src\plugins\compilergcc\resources\compilers\options_dc-gcc.xml`

Then everything will be ready for debugging.

### Installing the others prerequisites
 
For all the others prerequisites, you'll need to follow the standard instructions, i.e. basically you can install them in their default location.

## Building wxMSW

After installing all the prerequisites, you need to build **wxWidgets for Windows**, i.e. **wxMSW**. You only need to do that once.

1. Open the `.\wxMSW\build.ini` file and adapt it as needed.
2. Double-click on the `.\wxMSW\build.cmd` file.

The `.\wxMSW\bin` directory will be created.

## Configuring Code::Blocks IDE

To build a new **Code::Blocks** release, you will need **Code::Blocks**.
The following instructions assumes you have installed **Code::Blocks** in `C:\Program Files\CodeBlocks`.

1. Start **Code::Blocks** then open the `.\codeblocks\codeblocks\src\CodeBlocks.workspace` file. This will open the `Code::Blocks wx2.8.x` workspace.
2. Select the **Settings** > **Global Variable** menu item then select (or create) the `boost` variable. In the `base` directory field, enter `C:\Program Files\CodeBlocks`. In the `include` field, enter `C:\Program Files\CodeBlocks\include\boost-1_64` then in the `lib` field, enter `C:\Program Files\CodeBlocks\lib`.
4. Select (or create) the `cb_release_type` variable and enter `-g -O0` in the `base` field.
5. Select (or create) the `wx` variable, enter `.\wxMSW\bin\x86\release` (e.g. `C:\codeblocks\wxMSW\bin\x86\release`) in the `base` field.

## Debugging

If you want to debug the **Code::Blocks** build:

1. Build the **Code::Blocks** project (see below).
2. Execute the `Debug` command and select the `src` target when prompted. The **Code::Blocks for DreamSDK** build 
3. In the debugged **Code::Blocks**, go to the **Settings** > **Compiler** menu, 
select the **GNU GCC Compiler for Sega Dreamcast** profile and click on **Reset defaults**.
**Code::Blocks** should detect the **DreamSDK** package environment used for debug your **Code::Blocks** build.

The GNU Debugger (GDB) included in the latest release of **TDM-GCC** is buggy: some breakpoints are never reached. You should use a newer GNU Debugger (GDB) binary, for example the one included in **DreamSDK** (i.e. `E:\DreamSDK\bin\gdb.exe`). To change that, you may update the Debugger profile inside Code::Blocks (in the `Settings` menu).

## Building a Code::Blocks release

1. Make your changes in the **Code:Blocks** source (basically in `sdk`, `Compiler` and `Debugger` targets).
2. Select the **Settings** > **Global Variable** menu item then select the `cb_release_type` variable and enter `-O2` in the `base` field.
3. Change the content of the `.\codeblocks\src\include\autorevision.h` file. In normal conditions, this file is created automatically when using **SVN** and the `autorevision` tool. Or you may just create this `autorevision.h` file manually. The SVN revision `11256` is the official revision for the `17.12` release.

		/*0*/
		//don't include this header, only configmanager-revision.cpp should do this.
		#ifndef AUTOREVISION_H
		#define AUTOREVISION_H
	
	
		#include <wx/string.h>
	
		namespace autorevision
		{
			const unsigned int svn_revision = 11256;
			const wxString svnRevision(_T("11256"));
			const wxString svnDate(_T("YYYY-MM-DD hh:mm:ss")); // update manually the date/time using this format
		}
	
	
	
		#endif


4. Rebuild the [the whole workspace](http://wiki.codeblocks.org/index.php/Installing_Code::Blocks_from_source_on_Windows).
5. Run the `.\codeblocks\src\update.bat` file.

## Making the package

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
