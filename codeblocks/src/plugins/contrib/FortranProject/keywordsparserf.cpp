/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#include "keywordsparserf.h"
#include "parserthreadf.h"

#include <wx/tokenzr.h>
#include <wx/string.h>
#include <wx/arrstr.h>
#include <wx/file.h>
#include <globals.h>
#include "cbstyledtextctrl.h"
#include <configmanager.h>

#include <logmanager.h>


KeywordsParserF::KeywordsParserF():
    m_Parser(false)
{
    m_IsDone = false;
    wxString filename = ConfigManager::GetDataFolder() + _T("/images/fortranproject/fortran_procedures.f90");
    if (!wxFileExists(filename))
    {
        Manager::Get()->GetLogManager()->Log(_T("FortranProject plugin error: file ")+filename+_T(" was not found."));
        return;
    }
    m_Parser.Reparse(filename, fsfFree);

    TokensArrayF* pTokensArr = m_Parser.GetTokens();
    TokensArrayF* pTokens = &pTokensArr->Item(0)->m_Children;

    for (size_t i=0; i<pTokens->GetCount(); i++)
    {
        if (pTokens->Item(i)->m_TokenKind == tkSubroutine)
        {
            m_SubrSet.insert(pTokens->Item(i)->m_Name);
        }
        else if (pTokens->Item(i)->m_TokenKind == tkFunction)
        {
            m_FuncSet.insert(pTokens->Item(i)->m_Name);
        }
        else if (pTokens->Item(i)->m_TokenKind == tkModule && pTokens->Item(i)->m_Name.IsSameAs(_T("openmp")))
        {
            TokensArrayF* pOMPMod = &pTokens->Item(i)->m_Children;
            for (size_t j=0; j<pOMPMod->GetCount(); j++)
            {
                if (pOMPMod->Item(j)->m_TokenKind == tkVariable)
                    m_OpenMPKeywords.Add(pOMPMod->Item(j)->m_DisplayName);
            }
        }
        else if (pTokens->Item(i)->m_TokenKind == tkModule && pTokens->Item(i)->m_Name.IsSameAs(_T("openacc")))
        {
            TokensArrayF* pACCMod = &pTokens->Item(i)->m_Children;
            for (size_t j=0; j<pACCMod->GetCount(); j++)
            {
                if (pACCMod->Item(j)->m_TokenKind == tkVariable)
                    m_OpenACCKeywords.Add(pACCMod->Item(j)->m_DisplayName);
            }
        }
    }
    MakeOtherKeywordSet();
    m_IsDone = true;
}

KeywordsParserF::~KeywordsParserF()
{
    //dtor
}

bool KeywordsParserF::HasTokenSuitableKind(const wxString& name, int tokKind)
{
    if (!m_IsDone)
        return true;

    wxString nameLow = name.Lower();
    bool found = false;
    if ( (m_FuncSet.count(nameLow) && (tokKind & tkFunction)) ||
         (m_SubrSet.count(nameLow) && (tokKind & tkSubroutine)) ||
         (m_OtherKeywordSet.count(nameLow) && (tokKind & tkOther)) )
        found = true;

    return found;
}

void KeywordsParserF::GetCallTips(const wxString& name, wxArrayString& callTips, TokensArrayFlat* result)
{
    int tokKind = tkFunction | tkSubroutine;
    size_t resCountOld = result->GetCount();
    size_t resCount = m_Parser.FindMatchTokensDeclared(name, *result, tokKind, false);
    for (size_t i=resCountOld; i<resCount; ++i)
    {
        callTips.Add(result->Item(i)->m_Args);
    }
}

void KeywordsParserF::FindTokens(const wxString& name, TokensArrayFlat& result)
{
    int tokKind = tkFunction | tkSubroutine;
    m_Parser.FindMatchTokensDeclared(name, result, tokKind, false);
}

void KeywordsParserF::MakeOtherKeywordSet()
{
    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* result = tokensTmp.GetTokens();
    size_t resCount = m_Parser.FindMatchTokensDeclared(_T("list_of_other_fortran_keywords"), *result, tkFunction, false);
    if (resCount != 1)
    {
        Manager::Get()->GetLogManager()->Log(_T("FortranProject plugin error: "));
        Manager::Get()->GetLogManager()->Log(_T("Can't parse 'list_of_other_fortran_keywords' function."));
        return;
    }
    TokenFlat* token = result->Item(0);
    wxString txtRange;
    if (!m_Parser.FindTokenRange(*token, txtRange))
    {
        Manager::Get()->GetLogManager()->Log(_T("FortranProject plugin error: "));
        Manager::Get()->GetLogManager()->Log(_T("Can't parse 'list_of_other_fortran_keywords' function."));
        return;
    }

    //Parse
    TokensArrayClass tokensKeyTmp;
    TokensArrayF* parsResult = tokensKeyTmp.GetTokens();
    ParserThreadF thread = ParserThreadF(txtRange, parsResult, fsfFree, true);
    thread.ParseDeclarations();

    for (size_t i=0; i<parsResult->GetCount(); i++)
    {
        if (parsResult->Item(i)->m_TokenKind == tkVariable)
        {
            m_OtherKeywordSet.insert(parsResult->Item(i)->m_Name);
        }
    }
}

const wxArrayString* KeywordsParserF::GetKeywords(CompilerDirective cdir)
{
    if (cdir == cdOpenMP)
        return &m_OpenMPKeywords;
    else if (cdir == cdOpenACC)
        return &m_OpenACCKeywords;
    return NULL;
}
