@echo off
md   ..\..\..\devel31_64                                           > nul 2>&1
md   ..\..\..\output31_64                                          > nul 2>&1
md   ..\..\..\devel31_64\share                                     > nul 2>&1
md   ..\..\..\output31_64\share                                    > nul 2>&1
md   ..\..\..\devel31_64\share\CodeBlocks                          > nul 2>&1
md   ..\..\..\output31_64\share\CodeBlocks                         > nul 2>&1
md   ..\..\..\devel31_64\share\CodeBlocks\images                   > nul 2>&1
md   ..\..\..\output31_64\share\CodeBlocks\images                  > nul 2>&1
md   ..\..\..\devel31_64\share\CodeBlocks\images\fortranproject    > nul 2>&1
md   ..\..\..\output31_64\share\CodeBlocks\images\fortranproject   > nul 2>&1

copy images\fortranproject\*.*       ..\..\..\devel31_64\share\CodeBlocks\images\fortranproject\     > nul 2>&1
copy images\fortranproject\*.*       ..\..\..\output31_64\share\CodeBlocks\images\fortranproject\    > nul 2>&1

exit 0
