@echo off
md   ..\..\..\devel                                           > nul 2>&1
md   ..\..\..\output                                          > nul 2>&1
md   ..\..\..\devel\share                                     > nul 2>&1
md   ..\..\..\output\share                                    > nul 2>&1
md   ..\..\..\devel\share\CodeBlocks                          > nul 2>&1
md   ..\..\..\output\share\CodeBlocks                         > nul 2>&1
md   ..\..\..\devel\share\CodeBlocks\images                   > nul 2>&1
md   ..\..\..\output\share\CodeBlocks\images                  > nul 2>&1
md   ..\..\..\devel\share\CodeBlocks\images\fortranproject    > nul 2>&1
md   ..\..\..\output\share\CodeBlocks\images\fortranproject   > nul 2>&1

copy images\fortranproject\*.*       ..\..\..\devel\share\CodeBlocks\images\fortranproject\     > nul 2>&1
copy images\fortranproject\*.*       ..\..\..\output\share\CodeBlocks\images\fortranproject\    > nul 2>&1

exit 0
