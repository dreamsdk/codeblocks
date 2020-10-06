
/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */

#ifndef MAKEFILEGEN_H
#define MAKEFILEGEN_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <cbproject.h>
#endif

#include "projectdependencies.h"

class MakefileGen
{
    public:
        static void GenerateMakefile(cbProject* project, ProjectDependencies* projDep, NativeParserF* pNativeParser);

    private:
        static bool SelectMikefileName(wxFileName& mffn);
};

#endif // MAKEFILEGEN_H
