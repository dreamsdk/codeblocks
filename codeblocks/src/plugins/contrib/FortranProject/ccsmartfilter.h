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

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/string.h>
    #include <wx/event.h>
    #include <wx/file.h>

    #include <cbeditor.h>
#endif
#include <set>
#include <vector>

#include "tokenizerf.h"
#include "tokenf.h"

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
        static bool FitsToContext(const wxString& kw, const wxArrayString& firstWords);

    protected:
    private:
        static bool hasWord(const wxString& word, const wxArrayString& wordArr);

};

#endif // CCSMARTFILTER_H
