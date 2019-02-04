# Code::Blocks for DreamSDK Packager

This tool will create the package used in the **DreamSDK** Setup package.

How to use this tool:

1. Make your changes in the **Code:Blocks** source (basically in `sdk`, `Compiler` and `Debugger` targets).
2. Rebuild the whole workspace.
3. Run the `.\codeblocks\src\update.bat` file.
4. Run the `mkpkg.cmd` file.
5. Build the `.\cbpatcher\src\codeblocks-patcher.lpi` in **Lazarus**. Don't forget to pack it with **UPX**.
6. Run the `release.cmd` file.
7. Place the content of the `dist` directory in the `{app}\msys\1.0\opt\dreamsdk\packages\ide\codeblocks\` directory.
8. Done!
