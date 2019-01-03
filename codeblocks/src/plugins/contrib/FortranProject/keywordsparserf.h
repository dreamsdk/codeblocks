/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef KEYWORDSPARSERF_H
#define KEYWORDSPARSERF_H

#include <wx/string.h>
#include <wx/event.h>
#include <wx/file.h>
#include "tokenf.h"
#include "tokenizerf.h"
#include "parserf.h"
#include "cbeditor.h"
#include <set>
#include <vector>

typedef std::vector<FortranSourceForm> ArrayOfFortranSourceForm;
typedef std::set<wxString> StringSet;

enum CompilerDirective
{
    cdNone = 0,
    cdOther,
    cdOpenMP,
    cdOpenACC,
};

class KeywordsParserF
{
    public:
        KeywordsParserF();
        ~KeywordsParserF();
        bool HasTokenSuitableKind(const wxString& name, int tokKind);
        void GetCallTips(const wxString& name, wxArrayString& callTips, TokensArrayFlat* result);
        void FindTokens(const wxString& name, TokensArrayFlat& result);
        ParserF* GetParser(){ return &m_Parser; };
        const wxArrayString* GetKeywords(CompilerDirective cdir);

    protected:
    private:
        void MakeOtherKeywordSet();
        bool m_IsDone;

        ParserF m_Parser;

        StringSet m_SubrSet;
        StringSet m_FuncSet;
        StringSet m_OtherKeywordSet;

        wxArrayString m_OpenMPKeywords;
        wxArrayString m_OpenACCKeywords;
};

#endif // KEYWORDSPARSERF_H
