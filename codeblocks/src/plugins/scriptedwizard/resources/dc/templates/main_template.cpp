/*******************************************************************************
    Sega Dreamcast Project
    
    Project name : [PROJECT_NAME]
    Created on   : [NOW]
*******************************************************************************/

#define PROJECT_NAME "[PROJECT_NAME]"

/* This example simply demonstrates what needs to be done to have a properly
   managed Debug and Release targets in a project created by Code::Blocks.

   For KallistiOS demonstrations, see the examples provided with KallistiOS:
   /opt/toolchains/dc/kos/examples/dreamcast 

   If you're new to Sega Dreamcast development, this is a must-read:
   https://dreamcast.wiki/Getting_Started_with_Dreamcast_development

   You can also browse the online help here:
   https://kos-docs.dreamcast.wiki/
   
   If you need support, feel free to join the official Discord channel:
   https://discord.gg/bpDZHT78PA */

#include <iostream>
#include <kos/init.h>
[KOSLIBS_INC]

#ifdef DEBUG
#include <kos/dbgio.h>
#include <arch/gdb.h>
#endif

/* These macros tell KOS how to initialize itself. All of this initialization
   happens before main() gets called, and the shutdown happens afterwards. So
   you need to set any flags you want here. Here are some possibilities:

   INIT_NONE         -- don't do any auto init
   INIT_IRQ          -- Enable IRQs
   INIT_THD_PREEMPT  -- Enable pre-emptive threading
   INIT_NET          -- Enable networking (doesn't imply lwIP!)
   INIT_MALLOCSTATS  -- Enable a call to malloc_stats() right before shutdown

   You can OR any or all of those together. If you want to start out with
   the current KOS defaults, use INIT_DEFAULT (or leave it out entirely). */
KOS_INIT_FLAGS(INIT_DEFAULT | INIT_MALLOCSTATS);

/* Your program's main entry point */
int main(int argc, char *argv[])
{
#ifdef DEBUG
    /* This is required for the Debug target.
       This instruction initializes the connection with GNU Debugger (GDB).
       Don't forget to configure Dreamcast Tool (dc-tool) by using
       DreamSDK Manager. */
    gdb_init();	
	std::cout << "Connection established with " << PROJECT_NAME << "!" << std::endl;
    
    /* Example: Set the framebuffer as the output device for dbgio. */
    /* dbgio_dev_select("fb"); */
#endif

    /* Your program start here... */
    std::cout << "\nHello world from " << PROJECT_NAME << "!\n\n" << std::endl;    

    /* Bye... */
    return 0;
}
