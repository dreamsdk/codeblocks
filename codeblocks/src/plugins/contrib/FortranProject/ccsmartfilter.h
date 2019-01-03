/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */
#ifndef CCSMARTFILTER_H
#define CCSMARTFILTER_H

#include <wx/string.h>
#include <wx/event.h>
#include <wx/file.h>
#include "tokenf.h"
#include "tokenizerf.h"
#include "cbeditor.h"
#include <set>
#include <vector>

typedef std::vector<FortranSourceForm> ArrayOfFortranSourceForm;
typedef std::set<wxString> StringSet;

enum kindOfCCList
{
    kccUseAssocTokens = 1,
    kccAccessList,
    kccOther
};


class CCSmartFilter
{
    public:
        static void GetTokenKind(wxArrayString& words, int& kindFilter, bool& allowVariables, kindOfCCList& kindCC);
    protected:
    private:
};

#endif // CCSMARTFILTER_H
