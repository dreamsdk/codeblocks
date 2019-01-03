@echo off
md   ..\..\..\devel31                                           > nul 2>&1
md   ..\..\..\output31                                          > nul 2>&1
md   ..\..\..\devel31\share                                     > nul 2>&1
md   ..\..\..\output31\share                                    > nul 2>&1
md   ..\..\..\devel31\share\CodeBlocks                          > nul 2>&1
md   ..\..\..\output31\share\CodeBlocks                         > nul 2>&1
md   ..\..\..\devel31\share\CodeBlocks\images                   > nul 2>&1
md   ..\..\..\output31\share\CodeBlocks\images                  > nul 2>&1
md   ..\..\..\devel31\share\CodeBlocks\images\fortranproject    > nul 2>&1
md   ..\..\..\output31\share\CodeBlocks\images\fortranproject   > nul 2>&1

copy images\fortranproject\*.*       ..\..\..\devel31\share\CodeBlocks\images\fortranproject\     > nul 2>&1
copy images\fortranproject\*.*       ..\..\..\output31\share\CodeBlocks\images\fortranproject\    > nul 2>&1

exit 0
