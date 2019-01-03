
#include "calledbydict.h"
#include "tokenf.h"
#include <wx/string.h>
#include <wx/arrstr.h>
#include <set>


CalledByDict::CalledByDict()
{
}

CalledByDict::~CalledByDict()
{
    std::map<wxString,std::list<TokenF*>*>::iterator it;

    for (it = m_NamesDict.begin(); it != m_NamesDict.end(); ++it)
        delete(it->second);
}

void CalledByDict::Build(TokensArrayF* allTokens)
{
    std::set<wxString> definedNames;
    int tokenMask = tkSubroutine | tkFunction | tkInterface | tkProcedure;
    FindChildrenNames(allTokens, tokenMask, definedNames);
    FillCalledByDict(allTokens, definedNames);
}

void CalledByDict::FindChildrenNames(TokensArrayF* tokens, int tokenMask, std::set<wxString> &definedNames)
{
    for (size_t i=0; i<tokens->GetCount(); i++)
    {
        if (tokens->Item(i)->m_TokenKind & tokenMask)
        {
            TokenF* pTok = tokens->Item(i);
            if (definedNames.count(pTok->m_Name) == 0)
                definedNames.insert(pTok->m_Name);
        }
        if (tokens->Item(i)->m_Children.GetCount() > 0)
            FindChildrenNames(&tokens->Item(i)->m_Children, tokenMask, definedNames);
    }
}

void CalledByDict::FillCalledByDict(TokensArrayF* tokens, std::set<wxString> &definedNames)
{
    const int tokenCallMask = tkCallFunction | tkCallSubroutine;
    const int subfunMask = tkFunction | tkSubroutine;
    const int procMask = tkProcedure | tkProcedureFinal;
    for (size_t i=0; i<tokens->GetCount(); i++)
    {
        TokenF* pTok = tokens->Item(i);
        if (pTok->m_TokenKind & tokenCallMask ||
            (pTok->m_TokenKind & subfunMask && pTok->m_pParent->m_TokenKind == tkInterfaceExplicit) ||
            (pTok->m_TokenKind == tkOther && pTok->m_pParent->m_TokenKind == tkInterface) ||
            (pTok->m_TokenKind & procMask && pTok->m_pParent->m_TokenKind == tkType))
        {
            wxString name;
            int idx = pTok->m_Name.Find('%', true);
            if (idx != wxNOT_FOUND)
            {
                // called type-bound procedure
                name = pTok->m_Name.Mid(idx+1);
            }
            else
                name = pTok->m_Name;

            if (definedNames.count(name) > 0)
            {
                if (m_NamesDict.count(name) == 0)
                {
                    std::list<TokenF*>* tokList = new std::list<TokenF*>;
                    tokList->push_back(pTok);
                    m_NamesDict[name] = tokList;
                }
                else
                {
                    std::list<TokenF*>* tokList = m_NamesDict[name];
                    tokList->push_back(pTok);
                }
            }
        }
        if (tokens->Item(i)->m_Children.GetCount() > 0)
            FillCalledByDict(&tokens->Item(i)->m_Children, definedNames);
    }
}

std::list<TokenF*>* CalledByDict::GetCallingTokens(const wxString& name)
{
    if (m_NamesDict.count(name) == 0)
        return NULL;

    return m_NamesDict[name];
}


