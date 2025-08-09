@echo off
set CB_DEVEL_DIR=devel%1
set CB_OUTPUT_DIR=output%1
set CB_DEVEL_RESDIR=%CB_DEVEL_DIR%\share\CodeBlocks
set CB_OUTPUT_RESDIR=%CB_OUTPUT_DIR%\share\CodeBlocks

md   ..\..\..\%CB_DEVEL_DIR%                                   > nul 2>&1
md   ..\..\..\%CB_OUTPUT_DIR%                                  > nul 2>&1
md   ..\..\..\%CB_DEVEL_DIR%\share                             > nul 2>&1
md   ..\..\..\%CB_OUTPUT_DIR%\share                            > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%                                > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%                               > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images                         > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images                        > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject          > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject         > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\16x16    > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\20x20    > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\24x24    > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\28x28    > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\32x32    > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\40x40    > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\48x48    > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\56x56    > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\64x64    > nul 2>&1
md   ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\svg      > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\16x16   > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\20x20   > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\24x24   > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\28x28   > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\32x32   > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\40x40   > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\48x48   > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\56x56   > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\64x64   > nul 2>&1
md   ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\svg     > nul 2>&1

copy images\fortranproject\*.*             ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\           > nul 2>&1
copy images\fortranproject\*.*             ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\          > nul 2>&1
copy images\fortranproject\16x16\*.*       ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\16x16\     > nul 2>&1
copy images\fortranproject\20x20\*.*       ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\20x20\     > nul 2>&1
copy images\fortranproject\24x24\*.*       ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\24x24\     > nul 2>&1
copy images\fortranproject\28x28\*.*       ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\28x28\     > nul 2>&1
copy images\fortranproject\32x32\*.*       ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\32x32\     > nul 2>&1
copy images\fortranproject\40x40\*.*       ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\40x40\     > nul 2>&1
copy images\fortranproject\48x48\*.*       ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\48x48\     > nul 2>&1
copy images\fortranproject\56x56\*.*       ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\56x56\     > nul 2>&1
copy images\fortranproject\64x64\*.*       ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\64x64\     > nul 2>&1
copy images\fortranproject\svg\*.*         ..\..\..\%CB_DEVEL_RESDIR%\images\fortranproject\svg\       > nul 2>&1
copy images\fortranproject\16x16\*.*       ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\16x16\    > nul 2>&1
copy images\fortranproject\20x20\*.*       ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\20x20\    > nul 2>&1
copy images\fortranproject\24x24\*.*       ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\24x24\    > nul 2>&1
copy images\fortranproject\28x28\*.*       ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\28x28\    > nul 2>&1
copy images\fortranproject\32x32\*.*       ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\32x32\    > nul 2>&1
copy images\fortranproject\40x40\*.*       ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\40x40\    > nul 2>&1
copy images\fortranproject\48x48\*.*       ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\48x48\    > nul 2>&1
copy images\fortranproject\56x56\*.*       ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\56x56\    > nul 2>&1
copy images\fortranproject\64x64\*.*       ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\64x64\    > nul 2>&1
copy images\fortranproject\svg\*.*         ..\..\..\%CB_OUTPUT_RESDIR%\images\fortranproject\svg\      > nul 2>&1

exit 0
