@echo off
md   ..\..\..\devel30_64                                           > nul 2>&1
md   ..\..\..\output30_64                                          > nul 2>&1
md   ..\..\..\devel30_64\share                                     > nul 2>&1
md   ..\..\..\output30_64\share                                    > nul 2>&1
md   ..\..\..\devel30_64\share\CodeBlocks                          > nul 2>&1
md   ..\..\..\output30_64\share\CodeBlocks                         > nul 2>&1
md   ..\..\..\devel30_64\share\CodeBlocks\images                   > nul 2>&1
md   ..\..\..\output30_64\share\CodeBlocks\images                  > nul 2>&1
md   ..\..\..\devel30_64\share\CodeBlocks\images\fortranproject    > nul 2>&1
md   ..\..\..\output30_64\share\CodeBlocks\images\fortranproject   > nul 2>&1

copy images\fortranproject\*.*       ..\..\..\devel30_64\share\CodeBlocks\images\fortranproject\     > nul 2>&1
copy images\fortranproject\*.*       ..\..\..\output30_64\share\CodeBlocks\images\fortranproject\    > nul 2>&1

exit 0
