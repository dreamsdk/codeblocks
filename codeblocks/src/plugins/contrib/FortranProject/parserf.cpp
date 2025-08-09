/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#include "parserf.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/string.h>
    #include <wx/thread.h>
    #include <wx/arrstr.h>
    #include <wx/regex.h>

    #include <configmanager.h>
    #include <editormanager.h>
    #include <globals.h>
    #include <logmanager.h>
#endif
#include <vector>

#include <wx/tokenzr.h>

#include <cbstyledtextctrl.h>

#include "workspaceparserthread.h"
#include "parserthreadf.h"
#include "adddirparserthread.h"
#include "ccsmartfilter.h"

FortranFileExt g_FortranFileExt;

static wxCriticalSection s_CurrentBTokensCritSect;

ParserF::ParserF(bool withIntrinsicModules)
{
    m_pTokens = new TokensArrayF();
    m_pIntrinsicModuleTokens = NULL;
    m_pIncludeDB = new IncludeDB();
    m_pAdditionalDirTokens = NULL;
    m_pIncludeDBADir = NULL;
    m_Done = false;

    m_RecursiveDeep = 0;
    m_UseRenameArrays = false;
    m_RenameDeep = 0;
    m_IncludeDeep = 0;
    m_SubmodDeep = 0;

    m_pTokensNew = NULL;
    m_pIncludeDBNew = NULL;
    m_pTokensADirNew = NULL;
    m_pIncludeDBADirNew = NULL;
    m_pBufferTokens = new TokensArrayF();
    m_pCurrentBufferTokensNew = NULL;
    m_pAIncludeFiles = NULL;
    m_InterpretCPP = true;

    if (withIntrinsicModules)
    {
        m_pIntrinsicModuleTokens = new TokensArrayF();
        ParseIntrinsicModules();
    }
}

ParserF::~ParserF()
{
    //dtor
    Clear();
    delete m_pTokens;
    if (m_pIntrinsicModuleTokens)
        delete m_pIntrinsicModuleTokens;
    delete m_pIncludeDB;
    if (m_pAdditionalDirTokens)
        delete m_pAdditionalDirTokens;
    if (m_pIncludeDBADir)
        delete m_pIncludeDBADir;

    if (m_pTokensNew)
        delete m_pTokensNew;
    if (m_pIncludeDBNew)
        delete m_pIncludeDBNew;
    if (m_pTokensADirNew)
        delete m_pTokensADirNew;
    if (m_pIncludeDBADirNew)
        delete m_pIncludeDBADirNew;
    if (m_pBufferTokens)
        delete m_pBufferTokens;
    if (m_pCurrentBufferTokensNew)
        delete m_pCurrentBufferTokensNew;

    for (auto const& mPair : m_SkippedLinesMap)
    {
        delete mPair.second;
    }

    for (auto const& mPair : m_NewSkippedLinesMap)
    {
        delete mPair.second;
    }
}

bool ParserF::Parse(const wxString& projectFilename, const wxString& filename, FortranSourceForm fsForm,
                    const std::vector<wxString>* pCppMacros)
{
    wxCriticalSectionLocker locker(s_CritSect);
    wxString fn = UnixFilename(filename);
    ParserThreadF* thread = new ParserThreadF(projectFilename, fn, m_pTokens, fsForm, false, m_pIncludeDB,
                                              m_InterpretCPP, m_pAIncludeFiles, pCppMacros);
    bool res = thread->Parse();

    std::vector<wxString> parsedFileNames = thread->GetParsedFileNames();
    for (const auto& fileName: parsedFileNames)
    {
        TokenizerPP::SkippedLinesStruct* skipStruct = thread->GetSkippedLines(fileName);
        if (skipStruct)
            SetSkippedLines(fileName, skipStruct->lineStarts, skipStruct->lineEnds);
    }

    delete thread;
    return res;
}

bool ParserF::Reparse(const wxString& projectFilename, const wxString& filename, FortranSourceForm fsForm,
                      const std::vector<wxString>* pCppMacros)
{
    m_Done = false;
    RemoveFile(filename);
    bool res = Parse(projectFilename, filename, fsForm, pCppMacros);
    m_Done = true;
    return res;
}

bool ParserF::BatchParse(const wxArrayString& projectFilenames, const wxArrayString& filenames, ArrayOfFortranSourceForm& fileForms,
                         const std::vector<wxString>* pCppMacros)
{
    m_Done = false;
    bool res = true;
    if (filenames.size() != fileForms.size())
        return false;
    for (size_t i=0; i<filenames.size(); i++)
    {
        if(!Parse(projectFilenames[i], filenames[i], fileForms[i], pCppMacros))
        {
            res = false;
            //break;
        }
    }
    m_Done = true;
    return res;
}

bool ParserF::RemoveFile(const wxString& filename)
{
	wxString file = UnixFilename(filename);
    m_Done = false;
    wxCriticalSectionLocker locker(s_CritSect);

    RemoveBuffer(filename);

    size_t i = 0;
    while (i < m_pTokens->size())
    {
        if (m_pTokens->Item(i)->m_Filename.IsSameAs(file))
        {
            m_pTokens->Item(i)->Clear();
            delete m_pTokens->Item(i);
            m_pTokens->RemoveAt(i);
        }
        else
            ++i;
    }
    wxFileName fn(filename);
    m_pIncludeDB->RemoveFile(fn.GetFullName());
    m_Done = true;
	return true;
}

void ParserF::RemoveBuffer(const wxString& filename)
{
    wxString file = UnixFilename(filename);

    if (m_pBufferTokens &&
        (m_pBufferTokens->size() > 0))
    {
        size_t i = 0;
        while (i < m_pBufferTokens->size())
        {
            if (m_pBufferTokens->Item(i)->m_Filename.IsSameAs(file))
            {
                m_pBufferTokens->Item(i)->Clear();
                delete m_pBufferTokens->Item(i);
                m_pBufferTokens->RemoveAt(i);
                break;
            }
            else
                ++i;
        }
    }
}

void ParserF::SetAdditionalIncludeFiles(std::map<wxString,wxString>* pAIncludeFiles)
{
    m_pAIncludeFiles = pAIncludeFiles;
}

bool ParserF::FindTypeBoundProcedures(const TokenFlat& interToken, const wxArrayString& searchArr, TokensArrayFlat& resTokenArr)
{
    wxCriticalSectionLocker locker(s_CritSect);
    bool foundType = false;

    TokensArrayF* fileChildren = FindFileTokens(interToken.m_Filename);

    // find module
    TokenF* module;
    for (size_t j=0; j<fileChildren->GetCount(); j++)
    {
        if (fileChildren->Item(j)->m_TokenKind == tkModule)
        {
            module = fileChildren->Item(j);
            for (size_t k=0; k < module->m_Children.GetCount(); k++)
            {
                if (interToken.m_ParentName.IsSameAs(module->m_Children.Item(k)->m_Name) &&
                     interToken.m_ParentTokenKind == module->m_Children.Item(k)->m_TokenKind)
                {
                    // type was found
                    TokenF* typeTok = module->m_Children.Item(k);
                    for (size_t m=0; m < typeTok->m_Children.GetCount(); m++)
                    {
                        for (size_t l=0; l < searchArr.GetCount(); l++)
                        {
                            if (typeTok->m_Children.Item(m)->m_TokenKind == tkProcedure &&
                                typeTok->m_Children.Item(m)->m_Name.IsSameAs(searchArr.Item(l)) )
                            {
                                resTokenArr.Add(new TokenFlat(typeTok->m_Children.Item(m)));
                            }
                        }
                    }
                    foundType = true;
                    break;
                }
            }
            if (foundType)
                break;
        }
    }
    return foundType;
}

bool ParserF::FindMatchTokenInSameModule(const TokenFlat& procedureToken, const wxString& search, TokensArrayFlat& result, int tokenKindMask, int noChildrenOf)
{
    wxCriticalSectionLocker locker(s_CritSect);

    // find module
    TokensArrayF* fileChildren = FindFileTokens(procedureToken.m_Filename);
    TokenF* module;
    bool foundModule = false;
    for (size_t j=0; j<fileChildren->GetCount(); j++)
    {
        if (fileChildren->Item(j)->m_TokenKind == tkModule)
        {
            module = fileChildren->Item(j);
            for (size_t k=0; k < module->m_Children.GetCount(); k++)
            {
                if (procedureToken.m_ParentName.IsSameAs(module->m_Children.Item(k)->m_Name) &&
                     procedureToken.m_ParentTokenKind == module->m_Children.Item(k)->m_TokenKind)
                {
                    foundModule = true;
                    break;
                }
            }
        }
        if (foundModule)
            break;
    }
    if (foundModule)
    {
        // find match token
        size_t rCount = result.GetCount();
        FindMatchChildrenDeclared(module->m_Children, search, result, tokenKindMask, false, noChildrenOf);
        if (rCount < result.GetCount())
        {
            return true;
        }
    }
    return false;
}

size_t ParserF::FindMatchTokensDeclared(const wxString& search, TokensArrayFlat& result, int tokenKindMask, bool partialMatch, int noChildrenOf,
                                        bool onlyPublicNames, bool noIncludeFiles)
{
    wxString searchLw = search.Lower();
    wxChar sep = wxFileName::GetPathSeparator();

    wxCriticalSectionLocker locker(s_CritSect);

    for (size_t i=0; i<m_pTokens->GetCount(); i++)
    {
        if (noIncludeFiles)
        {
            wxString fn = m_pTokens->Item(i)->m_Filename.AfterLast(sep);
            if (m_pIncludeDB->IsIncludeFile(fn))
                continue;
        }
        TokensArrayF* fileChildren = FindFileTokens(m_pTokens->Item(i)->m_Filename);
        if (fileChildren && fileChildren->GetCount() > 0)
        {
            FindMatchChildrenDeclared(*fileChildren, searchLw, result, tokenKindMask, partialMatch, noChildrenOf, onlyPublicNames);
        }
    }

    if (m_pIntrinsicModuleTokens)
    {
        for (size_t i=0; i<m_pIntrinsicModuleTokens->GetCount(); i++)
        {
            if (m_pIntrinsicModuleTokens->Item(i)->m_Children.GetCount() > 0)
            {
                FindMatchChildrenDeclared(m_pIntrinsicModuleTokens->Item(i)->m_Children, searchLw, result, tokenKindMask,
                                          partialMatch, noChildrenOf, onlyPublicNames);
            }
        }
    }

    if (m_pAdditionalDirTokens)
    {
        for (size_t i=0; i<m_pAdditionalDirTokens->GetCount(); i++)
        {
            wxString fn = m_pAdditionalDirTokens->Item(i)->m_Filename.AfterLast(sep);
            if (m_pIncludeDBADir && m_pIncludeDBADir->IsIncludeFile(fn))
                continue;

            if (m_pAdditionalDirTokens->Item(i)->m_Children.GetCount() > 0)
            {
                FindMatchChildrenDeclared(m_pAdditionalDirTokens->Item(i)->m_Children, searchLw, result, tokenKindMask,
                                          partialMatch, noChildrenOf, onlyPublicNames);
            }
        }
    }
    return result.GetCount();
}

void ParserF::FindMatchChildrenDeclared(TokensArrayF& children, const wxString search, TokensArrayFlat& result, int tokenKindMask,
                                        bool partialMatch, int noChildrenOf, bool onlyPublicNames)
{
    size_t nChil = children.GetCount();
    for (size_t i=0; i<nChil; ++i)
    {
        TokenF* tok = children.Item(i);
        if ((partialMatch && (tok->m_TokenKind & tokenKindMask) && tok->m_Name.StartsWith(search)) ||
            (!partialMatch && (tok->m_TokenKind & tokenKindMask) && tok->m_Name.IsSameAs(search)))
        {
            if (!onlyPublicNames || (tok->m_TokenAccess != taPrivate) )
            {
                result.Add(new TokenFlat(tok));
            }
        }
        else if (tok->m_TokenKind == tkInclude && !(tkInclude & noChildrenOf))
        {
            if (m_IncludeDeep > 5)
                continue;
            TokensArrayF includedTokens;
            AddIncludeFileChildren(tok, includedTokens);
            if (includedTokens.GetCount() > 0)
            {
                m_IncludeDeep++;
                FindMatchChildrenDeclared(includedTokens, search, result, tokenKindMask, partialMatch, noChildrenOf, onlyPublicNames);
                m_IncludeDeep--;
            }
        }
        else if ((tok->m_TokenKind == tkSubmodule) && (tokenKindMask & tkSubmodule))
        {
            wxString sname = tok->m_Name.AfterLast(':');
            if ((partialMatch && sname.StartsWith(search)) ||
                (!partialMatch && sname.IsSameAs(search)))
            {
                result.Add(new TokenFlat(tok));
            }
        }

        if (tok->m_Children.GetCount() > 0 && !(tok->m_TokenKind & noChildrenOf))
        {
            FindMatchChildrenDeclared(tok->m_Children, search, result, tokenKindMask, partialMatch, noChildrenOf, onlyPublicNames);
        }
    }
}

void ParserF::FindMatchVariablesInModules(const wxString& search, TokensArrayFlat& result, bool partialMatch)
{
    wxString searchLw = search.Lower();

    wxCriticalSectionLocker locker(s_CritSect);

    for (size_t i=0; i<m_pTokens->GetCount(); i++)
    {
        TokensArrayF* children = FindFileTokens(m_pTokens->Item(i)->m_Filename);
        if (!children)
            continue;
        for (size_t j=0; j<children->GetCount(); j++)
        {
            if ( children->Item(j)->m_TokenKind == tkModule )
            {
                TokensArrayF* modChildren = &children->Item(j)->m_Children;
                for (size_t k=0; k<modChildren->GetCount(); k++)
                {
                    if ( (modChildren->Item(k)->m_TokenKind == tkVariable)
                        && ( ( partialMatch && modChildren->Item(k)->m_Name.StartsWith(searchLw) ) ||
                             ( !partialMatch && modChildren->Item(k)->m_Name.IsSameAs(searchLw) ) ) )
                    {
                        result.Add(new TokenFlat(modChildren->Item(k)));
                    }
                }
            }
        }
    }
}

bool ParserF::FindMatchTypeComponents(TokenFlat& parentTok, const wxString& lineStr, TokensArrayFlat& result)
{
    bool isAfterPercent;
    wxArrayString parts;
    if (!CutLineIntoParts(lineStr, isAfterPercent, parts))
        return false;
    if (parts.Count() == 0)
        return true;

    wxString name = parts.Item(0);

    TokensArrayFlatClass childrenTmp;
    TokensArrayFlat* childTFTmp = childrenTmp.GetTokens();
    GetChildren(&parentTok, tkVariable, *childTFTmp);
    size_t i=0;
    while(true)
    {
        if(i >= childTFTmp->Count())
            break;

        if(!childTFTmp->Item(i)->m_Name.IsSameAs(name))
        {
            delete childTFTmp->Item(i);
            childTFTmp->RemoveAt(i);
        }
        else
            i++;
    }

    wxString myFilename = parentTok.m_Filename;
    unsigned int myScopeLine = 0;
    if (childTFTmp->Count() > 0)
        myScopeLine = childTFTmp->Item(0)->m_LineStart;
    else // (childTFTmp->Count() == 0)
        FindUseAssociatedTokens(true, &parentTok, name, false, *childTFTmp, tkVariable, false);

    if (childTFTmp->Count() == 0)
        return false; //Type was not found.

    bool found = FindMatchTypeComponents2(childTFTmp, myScopeLine, myFilename, parts, result, false, true, true);
    return found;
}

bool ParserF::FindMatchTypeComponents(cbEditor* ed, const wxString& lineCur, TokensArrayFlat& result, bool partialMatch,
                                      bool onlyPublicNames, bool& isAfterPercent, bool getAsProcedure)
{
    wxArrayString parts;
    if (!CutLineIntoParts(lineCur, isAfterPercent, parts))
        return false;
    if (parts.Count() == 0)
        return true;

    wxString name = parts.Item(0);

    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* resultTmp = tokensTmp.GetTokens();
    int tmpEndPos = -1;
    int nLineCurScope = -1;
    FindMatchDeclarationsInCurrentScope(name, ed, *resultTmp, false, tmpEndPos, &nLineCurScope);

    if (nLineCurScope == -1)
        return false; // something wrong with finding current scope

    wxString myFilename = UnixFilename(ed->GetFilename());
    unsigned int myScopeLine = 0;
    if (resultTmp->Count() > 0)
        myScopeLine = resultTmp->Item(0)->m_LineStart;
    else // (resultTmp->Count() == 0)
        FindUseAssociatedTokens(onlyPublicNames, ed, name, false, *resultTmp, tkVariable, false);

    if (resultTmp->Count() == 0)
        return false; //Type was not found.

    bool found = FindMatchTypeComponents2(resultTmp, myScopeLine, myFilename, parts, result, partialMatch, onlyPublicNames, getAsProcedure);
    return found;
}

bool ParserF::FindMatchTypeComponents2(TokensArrayFlat* foundVariables, unsigned int myScopeLine, wxString& myFilename, wxArrayString& parts,
                                        TokensArrayFlat& result, bool partialMatch, bool onlyPublicNames, bool getAsProcedure)
{
    wxArrayString address;
    wxString nameType;
    bool nameType_found = false;
    for (size_t i=0; i<foundVariables->Count(); i++)
    {
        TokenFlat* tok = foundVariables->Item(i);
        if ( tok->m_TokenKind == tkVariable )
        {
            wxString tDefLow = tok->m_TypeDefinition.Lower();
            if ( tDefLow.StartsWith("type") || tDefLow.StartsWith("class") )
            {
                nameType = tDefLow;
                int idx_a = nameType.Find(")");
                int idx_b = nameType.Find("(");
                if (idx_a != wxNOT_FOUND && idx_b != wxNOT_FOUND && idx_a > idx_b+1)
                {
                    nameType = nameType.Mid(idx_b+1,idx_a-idx_b-1);
                    idx_a = nameType.Find("(");
                    if (idx_a != wxNOT_FOUND) // parametrized type
                        nameType = nameType.Mid(0,idx_a).Trim();
                    FindAddress(tok, address);
                    nameType_found = true;
                    break;
                }
            }
        }
    }
    if (!nameType_found)
        return false; // something is wrong

    int nTypes = parts.Count() - 1;
    wxString searchName = parts.Item(parts.Count()-1);
    wxString nameTypeCom = nameType;
    TokenF* typeToken = NULL;
    bool isHostAssociated = false;
    for (int i=1; i<=nTypes; i++)
    {
        TokensArrayFlatClass typesTmp;
        TokensArrayFlat* resultTypesTmp = typesTmp.GetTokens();
        FindUseAssociatedTokens(onlyPublicNames, address, nameType, false, *resultTypesTmp, tkType, false);

        if (resultTypesTmp->Count() == 0)
            return false; // type was not found

        {
            wxCriticalSectionLocker locker(s_CritSect);
            typeToken = GetTypeInFile(resultTypesTmp->Item(0)->m_Filename, resultTypesTmp->Item(0)->m_LineStart,
                                      resultTypesTmp->Item(0)->m_Name);
            if (!typeToken)
                return false;

            if (i == nTypes)
            {
                isHostAssociated = resultTypesTmp->Item(0)->m_HostAssociated;
                break;
            }

            if (!GetTypeOfComponent(&typeToken, parts.Item(i), nameTypeCom))
                return false; // something is wrong
            address.Clear();
            GetAddressOfToken(typeToken, address);
            address.Add(parts.Item(i));
        }
        nameType = nameTypeCom;
    }

    if (!typeToken)
        return false;

    for (int icyc=0; icyc < 30; icyc++)  // if icyc >= 30, definitely something is wrong
    {
        bool inSameModule = false;
        if ( typeToken->m_Filename.IsSameAs(myFilename) &&
            (myScopeLine > typeToken->m_pParent->m_LineStart) && (myScopeLine <= typeToken->m_pParent->m_LineEnd) )
            inSameModule = true;

        for (size_t i=0; i<typeToken->m_Children.GetCount(); i++)
        {
            TokenF* tokenCh = typeToken->m_Children.Item(i);
            if ( (partialMatch && (tokenCh->m_Name.StartsWith(searchName))) ||
                 (!partialMatch && (tokenCh->m_Name.IsSameAs(searchName))) )
            {
                TokenFlat* tokTmp=0;
                if (tokenCh->m_TokenKind == tkVariable)
                {
                    tokTmp = new TokenFlat(tokenCh);
                }
                else if (tokenCh->m_TokenKind == tkProcedure)
                {
                    tokTmp = new TokenFlat(tokenCh);
                    if (!getAsProcedure)
                    {
                        wxString tokName;
                        if (!tokenCh->m_PartLast.IsEmpty())
                            tokName = tokenCh->m_PartLast;
                        else
                            tokName = tokenCh->m_Name;

                        // what is the kind of procedure?
                        TokensArrayFlatClass tokensProc;
                        TokensArrayFlat* resultProc = tokensProc.GetTokens();
                        int kindMask = tkFunction | tkSubroutine;

                        int noInChildren = tkInterface | tkFunction | tkSubroutine;
                        bool found = FindMatchTokenInSameModule(tokenCh, tokName, *resultProc, kindMask, noInChildren);
                        if (!found)
                            FindMatchTokensDeclared(tokName, *resultProc, kindMask, false, noInChildren);
                        if (resultProc->GetCount() > 0)
                        {
                            tokTmp->m_TokenKind = resultProc->Item(0)->m_TokenKind;
                            ChangeArgumentsTypeBoundProc(*tokTmp, *(resultProc->Item(0)));
                            tokTmp->m_PartLast.Empty();
                        }
                    }
                }
                else if (tokenCh->m_TokenKind == tkInterface)
                {
                    tokTmp = new TokenFlat(tokenCh);
                }

                if (tokTmp)
                {
                    if ( !onlyPublicNames ||
                         tokenCh->m_TokenAccess != taPrivate ||
                         isHostAssociated ||
                         inSameModule )
                    {
                        result.Add(tokTmp);
                    }
                    else
                        delete tokTmp;
                }
            }
        }
        if ( (partialMatch && !typeToken->m_ExtendsType.empty() && typeToken->m_ExtendsType.Lower().StartsWith(searchName)) ||
            (!partialMatch && !typeToken->m_ExtendsType.empty() && typeToken->m_ExtendsType.Lower().IsSameAs(searchName)) )
        {
            TokenF* newToken = new TokenF;
            newToken->m_Name = typeToken->m_ExtendsType.Lower();
            newToken->m_DisplayName = typeToken->m_ExtendsType;
            newToken->m_TokenKind = tkType;
            newToken->m_pParent = typeToken;
            if (typeToken->m_WasIncluded)
            {
                newToken->m_Filename = typeToken->m_IncludeFilename;
                newToken->m_LineStart = typeToken->m_IncludeLineStart;
            }
            result.Add(new TokenFlat(newToken));
        }
        if (!typeToken->m_ExtendsType.IsEmpty())
        {
            typeToken = GetType(typeToken->m_ExtendsType.Lower());
            if(!typeToken)
                break; // type was not found
        }
        else
        {
            break;
        }
    }

    return true;
}


bool ParserF::CutLineIntoParts(const wxString& lineCur, bool& isAfterPercent, wxArrayString& parts)
{
    wxString line = lineCur.Lower();
    isAfterPercent = false;
    line = line.AfterLast(';');
    int idx = line.Find("%");
    if (idx == wxNOT_FOUND)
        return true;
    if (line.EndsWith(" "))
    {
        wxString tmpString = line.Trim();
        if (!tmpString.EndsWith("%"))
            return true;
    }
    else if (line.EndsWith(")") || line.EndsWith("(") || line.EndsWith(",")
             || line.EndsWith("[") || line.EndsWith("]"))
        return true;
    int idx_a = line.Find('(', true);
    int idx_b = line.Find(')', true);
    if ((idx_a != wxNOT_FOUND && idx_b == wxNOT_FOUND) || (idx_a > idx_b))
        line = line.Mid(idx_a+1);

    idx_a = line.Find('[', true);
    idx_b = line.Find(']', true);
    if ((idx_a != wxNOT_FOUND && idx_b == wxNOT_FOUND) || (idx_a > idx_b))
        line = line.Mid(idx_a+1);

    CutBlocks('(', line);
    CutBlocks('[', line);

    idx_a = line.Find(',', true);
    if (idx_a != wxNOT_FOUND)
        line = line.Mid(idx_a+1);
    idx = line.Find('=',true);
    if (idx != wxNOT_FOUND)
        line = line.Mid(idx+1);
    idx = line.Find('>',true);
    if (idx != wxNOT_FOUND)
        line = line.Mid(idx+1);
    idx = line.Find('<',true);
    if (idx != wxNOT_FOUND)
        line = line.Mid(idx+1);
    idx = line.Find('.',true);
    if (idx != wxNOT_FOUND)
        line = line.Mid(idx+1);
    idx = line.Find('/',true);
    if (idx != wxNOT_FOUND)
        line = line.Mid(idx+1);
    idx = line.Find('*',true);
    if (idx != wxNOT_FOUND)
        line = line.Mid(idx+1);
    idx = line.Find('-',true);
    if (idx != wxNOT_FOUND)
        line = line.Mid(idx+1);
    idx = line.Find('+',true);
    if (idx != wxNOT_FOUND)
        line = line.Mid(idx+1);
    idx = line.Find(':',true);
    if (idx != wxNOT_FOUND)
        line = line.Mid(idx+1);
    idx = line.Find('(',true);
    if (idx != wxNOT_FOUND)
        line = line.Mid(idx+1);
    idx = line.Find('%');
    if (idx == wxNOT_FOUND)
        return true;

    isAfterPercent = true;

    wxStringTokenizer tkz(line, "%", wxTOKEN_RET_EMPTY_ALL);
    while ( tkz.HasMoreTokens() )
    {
        wxString str = tkz.GetNextToken();
        wxStringTokenizer tkz2(str, " \t\r\n", wxTOKEN_STRTOK);
        if (tkz2.CountTokens() > 1)
        {
            // something is wrong. Try further
            while ( tkz2.HasMoreTokens() )
            {
                str = tkz2.GetNextToken();
            }
            parts.Empty();
        }
        parts.Add(str.Trim(false).Trim());
    }
    if (parts.Count() == 1)
        return false; // something wrong
    for (size_t i=0; i<parts.Count()-1; i++)
    {
        if (parts.Item(i).IsEmpty())
            return false; // something wrong
    }
    return true;
}


void ParserF::FindMatchDeclarationsInCurrentScope(const wxString& search, cbEditor* ed, TokensArrayFlat& result, bool partialMatch, int endPos, int* nLineStart)
{
    int lineStart = -1;
    TokenFlat* tokFl = NULL;
    FindLineScopeLN(ed, lineStart, tokFl, endPos);

    wxString searchLw = search.Lower();

    if (tokFl)
    {
        if ((tokFl->m_TokenKind == tkAssociateConstruct) ||
            (tokFl->m_TokenKind == tkSelectTypeChild) ||
            (tokFl->m_TokenKind == tkSelectTypeDefault))
        {
            wxString args = tokFl->m_Args;
            std::map<wxString,wxString> assocMap;
            ParserThreadF::SplitAssociateConstruct(args, assocMap);

            std::map<wxString,wxString>::iterator it;
            for ( it=assocMap.begin(); it != assocMap.end(); ++it )
            {
                if ((partialMatch && (*it).first.Lower().StartsWith(searchLw)) ||
                    (!partialMatch && (*it).first.Lower().IsSameAs(searchLw)))
                {
                    TokenFlat* newToken = new TokenFlat();
                    newToken->m_Name = (*it).first.Lower();

                    newToken->m_TokenKind = tkVariable;
                    newToken->m_pParent = NULL;
                    newToken->m_Filename = ed->GetFilename();
                    newToken->m_LineStart = lineStart;
                    newToken->m_DisplayName = (*it).first;
                    newToken->m_Args << " => " << (*it).second;
                    if (tokFl->m_TokenKind == tkAssociateConstruct)
                        newToken->m_TypeDefinition = "AssociateConstruct";
                    else if (tokFl->m_TokenKind == tkSelectTypeDefault)
                        newToken->m_TypeDefinition = "SelectTypeConstruct";
                    else // tkSelectTypeChild
                        newToken->m_TypeDefinition = tokFl->m_TypeDefinition;
                    newToken->m_DefinitionLength = 1;

                    result.Add(newToken);
                }
            }
            if (nLineStart)
                *nLineStart = lineStart;
            delete tokFl;
            return;
        }
        delete tokFl;
    }

    if (nLineStart)
        *nLineStart = lineStart;

    if (lineStart == -1)
        return;

    {
        cbStyledTextCtrl* control = ed->GetControl();
        if (!control)
            return;

        int curPos = control->GetCurrentPos();
        unsigned int curLine = control->LineFromPosition(curPos) + 1;
        int tokenKindMask = tkFunction | tkProgram | tkSubroutine | tkModule | tkBlockConstruct |
                        tkAssociateConstruct | tkSubmodule | tkSelectTypeChild | tkSelectTypeDefault |
                        tkType | tkProcedure;

        wxCriticalSectionLocker locker(s_CritSect);
        TokensArrayF* fileChildren = FindFileTokens(ed->GetFilename());
        if (!fileChildren)
            return;

        TokenF* pToken = NULL;
        if (!FindLineScope(curLine, lineStart, tokenKindMask, *fileChildren, pToken))
            return;
        if(!pToken)
            return;
        int filterMask = tkVariable;
        TokensArrayF* pChildren = &pToken->m_Children;
        //Add results
        for (size_t i=0; i<pChildren->GetCount(); i++)
        {
            if ((partialMatch && pChildren->Item(i)->m_Name.StartsWith(searchLw)) ||
                (!partialMatch && pChildren->Item(i)->m_Name.IsSameAs(searchLw)))
            {
                if (pChildren->Item(i)->m_TokenKind & filterMask)
                {
                    result.Add(new TokenFlat(pChildren->Item(i)));
                }
            }
        }
        if (pToken->m_TokenKind == tkType && pToken->m_pParent && (pToken->m_pParent->m_TokenKind & tokenKindMask))
        {
            pChildren = &pToken->m_pParent->m_Children;
            for (size_t i=0; i<pChildren->GetCount(); i++)
            {
                if ((partialMatch && pChildren->Item(i)->m_Name.StartsWith(searchLw)) ||
                    (!partialMatch && pChildren->Item(i)->m_Name.IsSameAs(searchLw)))
                {
                    if (pChildren->Item(i)->m_TokenKind & filterMask)
                    {
                        result.Add(new TokenFlat(pChildren->Item(i)));
                    }
                }
            }
        }
        if (pToken->m_TokenKind == tkFunction && pToken->m_ResultVariable.IsEmpty())
        {
            // Take function name as variable if 'result(var_name)' was not given
            if ((partialMatch && pToken->m_Name.StartsWith(searchLw)) ||
                (!partialMatch && pToken->m_Name.IsSameAs(searchLw)))
            {
                // Check if name was not added already as a local variable
                bool alreadyHave = false;
                for (size_t i=0; i<result.GetCount(); i++)
                {
                    if (result.Item(i)->m_Name.IsSameAs(pToken->m_Name))
                    {
                        alreadyHave = true;
                        break;
                    }
                }
                if (!alreadyHave)
                    result.Add(new TokenFlat(pToken));
            }
        }
    }
    return;
}

void ParserF::FindMachDefineTokens(const wxString& search, cbEditor* ed, TokensArrayFlat& result)
{
    wxString filename = ed->GetFilename();
    FortranSourceForm fsForm;
    if (!IsFileFortran(filename, fsForm))
        return;

    //Find scope between file tokens
    wxCriticalSectionLocker locker(s_CritSect);
    TokensArrayF* children = FindFileTokens(filename);
    if (!children)
        return;

    wxString searchLw = search.Lower();
    size_t nChildren = children->GetCount();
    for (size_t i=0; i<nChildren; i++)
    {
        if ((children->Item(i)->m_TokenKind == tkMacroDefine) && children->Item(i)->m_Name.StartsWith(searchLw))
        {
            result.Add(new TokenFlat(children->Item(i)));
        }
    }
}

bool ParserF::FindLineScope(unsigned int line, int& lineStart, int tokenKindMask, TokensArrayF& children, TokenF* &pToken)
{
    bool found = false;
    for (size_t i=0; i<children.GetCount(); i++)
    {
        if ((children.Item(i)->m_LineStart <= line) && (children.Item(i)->m_LineEnd >= line) && (children.Item(i)->m_TokenKind & tokenKindMask))
        {
            lineStart = children.Item(i)->m_LineStart;
            pToken = children.Item(i);
            FindLineScope(line, lineStart, tokenKindMask, children.Item(i)->m_Children, pToken);
            found = true;
            break;
        }
        else if (children.Item(i)->m_LineStart > line)
        {
            found = true;
            break;
        }
    }
    return found;
}

void ParserF::FindLineScopeLN(cbEditor* ed, int& lineStart, TokenFlat* &token, int endPos)
{
    lineStart = -1;

    wxString filename = ed->GetFilename();
    FortranSourceForm fsForm;
    if (!IsFileFortran(filename, fsForm))
        return;

    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return;

    int curPos;
    if (endPos == -1)
        curPos = control->GetCurrentPos();
    else
        curPos = endPos;

    unsigned int curLine = control->LineFromPosition(curPos) + 1;
    int tokenKindMask = tkFunction | tkProgram | tkSubroutine | tkModule | tkBlockConstruct |
                        tkAssociateConstruct | tkSubmodule | tkSelectTypeChild | tkSelectTypeDefault |
                        tkInterfaceExplicit | tkInterface | tkProcedure; // | tkType;

    //Parse to find a scope
    unsigned int parseStartLine;
    if (curLine <= 100)
        parseStartLine = 1;
    else
        parseStartLine = curLine - 100;

    wxString strRange;
    int linesUntil;
    if (parseStartLine == 1)
    {
        strRange = control->GetTextRange(0,curPos);
        linesUntil = 0;
    }
    else
    {
        linesUntil = parseStartLine - 2;
        strRange = control->GetTextRange(control->GetLineEndPosition(linesUntil),curPos);
    }
    curLine -= linesUntil;

    TokenF* pToken = NULL;
    int chUntil = 0;
    TokensArrayClass tTemp;
    TokensArrayF* pRes = tTemp.GetTokens();
    ParserThreadF parsTh = ParserThreadF(wxEmptyString, strRange, pRes, fsForm, true, NULL, m_InterpretCPP);
    bool res = parsTh.Parse();
    if (res)
    {
        FindLineScope(curLine, lineStart, tokenKindMask, *pRes, pToken);

        if (pToken && pToken->m_Name.empty() && (pToken->m_TokenKind != tkBlockConstruct) &&
            (pToken->m_TokenKind != tkAssociateConstruct) &&
            (pToken->m_TokenKind != tkSelectTypeChild) && (pToken->m_TokenKind != tkSelectTypeDefault))
        {
            if (pToken->m_pParent && (pToken->m_pParent->m_TokenKind & tokenKindMask))
            {
                pToken = pToken->m_pParent;
                lineStart = pToken->m_LineStart;
            }
            else
            {
                lineStart = -1;
            }
        }

        if (pToken)
            pToken->m_Filename = UnixFilename(filename);
    }

    if (lineStart == -1)
    {
        //Find scope between file tokens
        wxCriticalSectionLocker locker(s_CritSect);
        TokensArrayF* children = FindFileTokens(filename);
        if (!children)
            return;

        for (size_t i=0; i<children->GetCount(); i++)
        {
            if ((children->Item(i)->m_LineStart <= parseStartLine) && (children->Item(i)->m_TokenKind & tokenKindMask))
            {
                lineStart = children->Item(i)->m_LineStart;
                pToken = children->Item(i);
                if (FindLineScope(parseStartLine, lineStart, tokenKindMask, children->Item(i)->m_Children, pToken))
                {
                    break;
                }
            }
            else if (children->Item(i)->m_LineStart > parseStartLine)
            {
                break;
            }
        }
    }
    else
    {
        lineStart += linesUntil;
        chUntil = linesUntil;
    }

    if (lineStart == -1)
        return;

    if (pToken)
    {
        token = new TokenFlat(pToken);
        token->m_LineStart += chUntil;
    }
}

TokensArrayF* ParserF::FindFileTokens(const wxString& filename)
{
    TokensArrayF* children=NULL;
    if (m_pBufferTokens)
    {
        for (size_t i=0; i<m_pBufferTokens->GetCount(); i++)
        {
            if (m_pBufferTokens->Item(i)->m_Filename.IsSameAs(filename))
            {
                children = &m_pBufferTokens->Item(i)->m_Children;
                break;
            }
        }
    }
    if (!children)
    {
        size_t nTok = m_pTokens->GetCount();
        for (size_t i=0; i<nTok; ++i)
        {
            TokenF* tok = m_pTokens->Item(i);
            if (tok->m_TokenKind == tkFile && (tok->m_Filename.IsSameAs(filename)))
            {
                children = &tok->m_Children;
                break;
            }
        }
    }
    if (!children && m_pIntrinsicModuleTokens)
    {
        for (size_t i=0; i<m_pIntrinsicModuleTokens->GetCount(); ++i)
        {
            if ((m_pIntrinsicModuleTokens->Item(i)->m_TokenKind == tkFile) && (m_pIntrinsicModuleTokens->Item(i)->m_Filename.IsSameAs(filename)))
            {
                children = &m_pIntrinsicModuleTokens->Item(i)->m_Children;
                break;
            }
        }
    }
    if (!children && m_pAdditionalDirTokens)
    {
        for (size_t i=0; i<m_pAdditionalDirTokens->GetCount(); ++i)
        {
            if ((m_pAdditionalDirTokens->Item(i)->m_TokenKind == tkFile) && (m_pAdditionalDirTokens->Item(i)->m_Filename.IsSameAs(filename)))
            {
                children = &m_pAdditionalDirTokens->Item(i)->m_Children;
                break;
            }
        }
    }
    return children;
}

TokenF* ParserF::FindFile(const wxString& filename)
{
    wxString fn = UnixFilename(filename);
    TokenF* fileToken=0;
    size_t nTok = m_pTokens->GetCount();
    for (size_t i=0; i<nTok; ++i)
    {
        TokenF* tok = m_pTokens->Item(i);
        if (tok->m_TokenKind == tkFile && (tok->m_Filename.IsSameAs(fn)))
        {
            fileToken = m_pTokens->Item(i);
            break;
        }
    }
    return fileToken;
}

void ParserF::FindFile(const wxString& filename, TokensArrayFlat& result)
{
    wxCriticalSectionLocker locker(s_CritSect);

    for (size_t i=0; i<m_pTokens->GetCount(); i++)
    {
        if (m_pTokens->Item(i)->m_TokenKind == tkFile &&
            m_pTokens->Item(i)->m_Name.IsSameAs(filename))
        {
            result.Add(new TokenFlat(m_pTokens->Item(i)));
        }
    }
}

TokenF* ParserF::FindFileTokenWithName(const wxString& filename)
{
    wxString fn = UnixFilename(filename);
    TokenF* fileToken=0;
    for (size_t i=0; i<m_pTokens->GetCount(); i++)
    {
        if (m_pTokens->Item(i)->m_TokenKind == tkFile)
        {
            wxFileName tfn(m_pTokens->Item(i)->m_Filename);
            if (tfn.GetFullName().IsSameAs(filename))
            {
                fileToken = m_pTokens->Item(i);
                break;
            }
        }
    }
    return fileToken;
}

TokenF* ParserF::FindModuleSubmoduleToken(const wxString& moduleName)
{
    wxString moduleNameLw = moduleName.Lower();
    TokenF* module = 0;
    if (m_pBufferTokens)
    {
        size_t nTok = m_pBufferTokens->GetCount();
        for (size_t i=0; i<nTok; ++i)
        {
            TokenF* tok = m_pBufferTokens->Item(i);
            if (tok->m_TokenKind == tkFile)
            {
                TokensArrayF* children = &tok->m_Children;
                size_t nChil = children->GetCount();
                for (size_t j=0; j<nChil; ++j)
                {
                    TokenF* chil = children->Item(j);
                    if ((chil->m_TokenKind == tkModule || chil->m_TokenKind == tkSubmodule) &&
                         chil->m_Name.IsSameAs(moduleNameLw))
                    {
                        module = chil;
                        break;
                    }
                }
                if (module)
                    break;
            }
        }
    }

    if (!module)
    {
        size_t nTok = m_pTokens->GetCount();
        for (size_t i=0; i<nTok; ++i)
        {
            TokenF* tok = m_pTokens->Item(i);
            if (tok->m_TokenKind == tkFile)
            {
                TokensArrayF* children = &tok->m_Children;
                size_t nChil = children->GetCount();
                for (size_t j=0; j<nChil; ++j)
                {
                    TokenF* chil = children->Item(j);
                    if ((chil->m_TokenKind == tkModule || chil->m_TokenKind == tkSubmodule) &&
                         chil->m_Name.IsSameAs(moduleNameLw))
                    {
                        module = chil;
                        break;
                    }
                }
                if (module)
                    break;
            }
        }
    }

    if (!module && m_pIntrinsicModuleTokens)
    {
        for (size_t i=0; i<m_pIntrinsicModuleTokens->GetCount(); ++i)
        {
            if (m_pIntrinsicModuleTokens->Item(i)->m_TokenKind == tkFile)
            {
                TokensArrayF* children = &m_pIntrinsicModuleTokens->Item(i)->m_Children;
                for (size_t j=0; j<children->GetCount(); ++j)
                {
                    if (children->Item(j)->m_TokenKind == tkModule && children->Item(j)->m_Name.IsSameAs(moduleNameLw))
                    {
                        module = children->Item(j);
                        break;
                    }
                }
                if (module)
                    break;
            }
        }
    }

    if (!module && m_pAdditionalDirTokens)
    {
        for (size_t i=0; i<m_pAdditionalDirTokens->GetCount(); i++)
        {
            if (m_pAdditionalDirTokens->Item(i)->m_TokenKind == tkFile)
            {
                TokensArrayF* children = &m_pAdditionalDirTokens->Item(i)->m_Children;
                for (size_t j=0; j<children->GetCount(); j++)
                {
                    if (children->Item(j)->m_TokenKind == tkModule && children->Item(j)->m_Name.IsSameAs(moduleNameLw))
                    {
                        module = children->Item(j);
                        break;
                    }
                }
                if (module)
                    break;
            }
        }
    }
    return module;
}


size_t ParserF::FindMatchTokens(wxString filename, wxString search, TokensArrayF& result)
{
    filename = UnixFilename(filename);
    search = search.Lower();

    TokensArrayF* filechildren = FindFileTokens(filename);
    if (filechildren)
        FindMatchChildren(*filechildren, search, result);
    else
        Manager::Get()->GetLogManager()->DebugLog("Can not find file # tokens:" + filename);

    return result.GetCount();
}

void ParserF::FindMatchChildren(TokensArrayF &children, wxString search, TokensArrayF& result, bool exact)
{
    for (size_t i=0; i<children.GetCount(); i++)
    {
        if (exact)
        {
            if (children.Item(i)->m_Name.IsSameAs(search))
                result.Add(children.Item(i));
        }
        else
        {
            if (!(children.Item(i)->m_Name.Find(search) == wxNOT_FOUND))
                result.Add(children.Item(i));
        }
        if (children.Item(i)->m_Children.GetCount() > 0)
            FindMatchChildren(children.Item(i)->m_Children, search, result, exact);
    }
}

void ParserF::Clear()
{
    m_Done = false;
    wxCriticalSectionLocker locker(s_CritSect);

    if (m_pTokens)
        ClearTokens(m_pTokens);

    if (m_pIntrinsicModuleTokens)
        ClearTokens(m_pIntrinsicModuleTokens);

    m_VisitedModules.Clear();
    ClearPassedTokensArray2D(m_PassedTokensVisited);
    ClearArrOfSizeT2D(m_ChildrenIdxVisited);
    ClearBoolArray3D(m_CanBeSeenVisited);

    if (m_pIncludeDB)
        m_pIncludeDB->Clear();

    if (m_pTokensNew)
        ClearTokens(m_pTokensNew);
    if (m_pIncludeDBNew)
        m_pIncludeDBNew->Clear();

    if (m_pBufferTokens)
        ClearTokens(m_pBufferTokens);

    if (m_pCurrentBufferTokensNew)
        ClearTokens(m_pCurrentBufferTokensNew);

    if (m_pAdditionalDirTokens)
        ClearTokens(m_pAdditionalDirTokens);
    if (m_pIncludeDBADir)
        m_pIncludeDBADir->Clear();

    if (m_pTokensADirNew)
        ClearTokens(m_pTokensADirNew);
    if (m_pIncludeDBADirNew)
        m_pIncludeDBADirNew->Clear();

    m_Done = true;
}

void ParserF::ObtainUsedDeclaredModules(const wxString& fileName, StringSet* fileUseModules, StringSet* fileDeclaredModules,
                                        StringSet* fileExtendsSModules, StringSet* fileDeclaredSubmodules, StringSet* fileIncludes)
{
    wxCriticalSectionLocker locker(s_CritSect);

    int idx = GetFileIndex(fileName);
    if (idx == -1)
        return;
    TokenF* tok = m_pTokens->Item(idx);

    ObtainUDModulesToken(tok, fileUseModules, fileDeclaredModules, fileExtendsSModules, fileDeclaredSubmodules, fileIncludes);
}

void ParserF::ObtainUDModulesToken(TokenF* token, StringSet* fileUseModules, StringSet* fileDeclaredModules,
                                   StringSet* fileExtendsSModules, StringSet* fileDeclaredSubmodules, StringSet* fileIncludes)
{
    for (size_t i=0; i < token->m_Children.GetCount(); i++)
    {
        if (token->m_Children.Item(i)->m_TokenKind == tkUse)
        {
            fileUseModules->insert(token->m_Children.Item(i)->m_Name);
        }
        else if (token->m_Children.Item(i)->m_TokenKind == tkModule)
        {
            fileDeclaredModules->insert(token->m_Children.Item(i)->m_Name);
        }
        else if (token->m_Children.Item(i)->m_TokenKind == tkSubmodule)
        {
            wxString smodName = token->m_Children.Item(i)->m_Name;
            SubmoduleTokenF* submod = static_cast<SubmoduleTokenF*>(token->m_Children.Item(i));
            wxString parentModName = submod->m_AncestorModuleName;
            int icol = smodName.Find(':');
            if (icol != wxNOT_FOUND)
                smodName = smodName.Mid(icol+1);
            smodName << "(" << parentModName << ")";
            fileDeclaredSubmodules->insert(smodName);

            wxString extName;
            if (!submod->m_ParentSubmoduleName.IsEmpty())
                extName = submod->m_ParentSubmoduleName + "(" + parentModName + ")";
            else
                extName = parentModName;
            fileExtendsSModules->insert(extName);
        }
        else if (token->m_Children.Item(i)->m_TokenKind == tkInclude)
        {
            fileIncludes->insert(token->m_Children.Item(i)->m_Name);
        }

        if (token->m_Children.Item(i)->m_Children.GetCount() > 0)
            ObtainUDModulesToken(token->m_Children.Item(i), fileUseModules, fileDeclaredModules,
                                 fileExtendsSModules, fileDeclaredSubmodules, fileIncludes);
    }
}

size_t ParserF::GetFileIndex(const wxString& filename)
{
    wxString fn = UnixFilename(filename);
    for (size_t i=0; i<m_pTokens->GetCount(); i++)
    {
        if (m_pTokens->Item(i)->m_Filename.IsSameAs(fn))
        {
            return i;
        }
    }
    return -1;
}

bool ParserF::IsFileFortran(const wxString& filename, FortranSourceForm& fsForm)
{
    return g_FortranFileExt.IsFileFortran(filename, fsForm);
}

void ParserF::RereadOptions()
{
    g_FortranFileExt.RereadOptions();
}

void ParserF::FindMatchTokensForToolTip(const wxString& nameUnder, int posEndOfWord, cbEditor* ed,
                                        bool onlyUseAssoc, bool onlyPublicNames, TokensArrayFlat& result, bool& isAfterPercent)
{
    isAfterPercent = false;
    if (!ed)
        return;
    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return;
    int lineStartPos = control->GetLineEndPosition(control->LineFromPosition(posEndOfWord) - 1) + 1;
    wxString curLine = control->GetTextRange(lineStartPos,posEndOfWord);

    TokensArrayFlatClass tokensTemp;
    TokensArrayFlat* resultTemp = tokensTemp.GetTokens();
    if (!FindMatchTypeComponents(ed, curLine, *resultTemp, false, onlyPublicNames, isAfterPercent, true))
        return;
    if (resultTemp->GetCount() > 0)
    {
        TokenFlat* token = resultTemp->Item(0); // we take only first added item
        result.Add( new TokenFlat(token) );
        if (token->m_TokenKind == tkProcedure)
        {
            wxString tokName;
            if (!token->m_PartLast.IsEmpty())
                tokName = token->m_PartLast;
            else
                tokName = token->m_Name;

            TokensArrayFlatClass tokensTmp;
            TokensArrayFlat* resultTmp = tokensTmp.GetTokens();
            int kindMask = tkFunction | tkSubroutine;
            int noInChildren = tkInterface | tkFunction | tkSubroutine;
            bool found = FindMatchTokenInSameModule(token, tokName, *resultTmp, kindMask, noInChildren);
            if (!found)
                FindMatchTokensDeclared(tokName, *resultTmp, kindMask, false, noInChildren);
            if (resultTmp->GetCount() > 0)
                result.Add( new TokenFlat(resultTmp->Item(0)) );
        }
        else if (token->m_TokenKind == tkInterface)
        {
            FindGenericTypeBoudComponents(token, result);
            for (size_t i=1; i<resultTemp->GetCount(); i++)
            {
                if (resultTemp->Item(i)->m_TokenKind == tkInterface)
                {
                    result.Add( new TokenFlat(resultTemp->Item(i)));
                    FindGenericTypeBoudComponents(resultTemp->Item(i), result);
                }
            }
        }
    }

    if (!isAfterPercent)
    {
        int tokKind = tkModule | tkFunction | tkProgram | tkSubroutine | tkPreprocessor | tkInterface | tkBlockData | tkType;
        if (onlyUseAssoc)
        {
            int noChildrenOf = tkInterface | tkModule | tkSubmodule | tkFunction | tkSubroutine | tkProgram;
            tokKind = tokKind | tkVariable;
            FindUseAssociatedTokens(onlyPublicNames, ed, nameUnder, false, result, tokKind, false);
            FindMatchTokensDeclared(nameUnder, result, tokKind, false, noChildrenOf, false, true); // take global procedures only
        }
        else
        {
            int noChildrenOf = tkInterface | tkFunction | tkSubroutine | tkProgram;
            FindMatchTokensDeclared(nameUnder, result, tokKind, false, noChildrenOf, onlyPublicNames);
            FindMatchVariablesInModules(nameUnder, result, false);
        }
        FindMatchDeclarationsInCurrentScope(nameUnder, ed, result, false, posEndOfWord);
    }
}

void ParserF::FindGenericTypeBoudComponents(TokenFlat* token, TokensArrayFlat& result)
{
    if (token->m_TokenKind != tkInterface)
        return;

    if (token->m_PartLast.IsEmpty())
        return;

    wxArrayString specNames;
    wxStringTokenizer tkz(token->m_PartLast, " \t\r\n", wxTOKEN_STRTOK);
    while ( tkz.HasMoreTokens() )
    {
        specNames.Add(tkz.GetNextToken().Lower());
    }
    TokensArrayFlatClass procTokenArrTmp;
    TokensArrayFlat* procTokenArr = procTokenArrTmp.GetTokens();
    if (!FindTypeBoundProcedures(token, specNames, *procTokenArr))
        return;
    int kindMask = tkFunction | tkSubroutine;
    int noInChildren = tkInterface | tkFunction | tkSubroutine;
    for (size_t i=0; i<procTokenArr->Count(); i++)
    {
        wxString tokName;
        if (!procTokenArr->Item(i)->m_PartLast.IsEmpty())
            tokName = procTokenArr->Item(i)->m_PartLast;
        else
            tokName = procTokenArr->Item(i)->m_Name;

        TokensArrayFlatClass tokensTmp;
        TokensArrayFlat* resultTmp = tokensTmp.GetTokens();
        bool found = FindMatchTokenInSameModule(procTokenArr->Item(i), tokName, *resultTmp, kindMask, noInChildren);
        if (!found)
            FindMatchTokensDeclared(tokName, *resultTmp, kindMask, false, noInChildren);
        if (resultTmp->GetCount() > 0)
        {
            result.Add( new TokenFlat(procTokenArr->Item(i)) );
            result.Add( new TokenFlat(resultTmp->Item(0)) );
        }
    }
}


void ParserF::FindMatchOperatorTokensForJump(wxString& nameOperator, TokensArrayFlat& result)
{
    wxString nameFind;
    if (nameOperator.IsSameAs("="))
        nameFind = "%%assignment";
    else
        nameFind = "%%operator";

    int noChildrenOf = tkFunction | tkSubroutine | tkProgram;
    int tokKind = tkInterface;
    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* tokensTmpFl = tokensTmp.GetTokens();
    FindMatchTokensDeclared(nameFind, *tokensTmpFl, tokKind, true, noChildrenOf);

    wxString regExStr = "^" + nameFind + "[\\s\\t]*\\([\\s\\t]*\\" + nameOperator + "[\\s\\t]*\\).*";
#if wxCHECK_VERSION(3, 1, 6)
    int opt = wxRE_EXTENDED | wxRE_ICASE | wxRE_NOSUB;
#else
    int opt = wxRE_ADVANCED | wxRE_ICASE | wxRE_NOSUB;
#endif // wxCHECK_VERSION
    wxRegEx opRegEx;
    if(!opRegEx.Compile(regExStr, opt))
        return;

    for (size_t i=0; i<tokensTmpFl->size(); i++)
    {
        if (opRegEx.Matches(tokensTmpFl->Item(i)->m_Name))
        {
            TokenFlat* newTok = new TokenFlat(tokensTmpFl->Item(i));
            newTok->m_DisplayName = newTok->m_DisplayName.Mid(2);
            newTok->m_Name = newTok->m_Name.Mid(2);
            result.Add(newTok);
        }
    }
}


void ParserF::FindMatchTokensForJump(cbEditor* ed, bool onlyUseAssoc, bool onlyPublicNames, TokensArrayFlat& result)
{
    bool isAfterPercent = false;
    if (!ed)
        return;
    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return;
    int pos = control->GetCurrentPos();
    int posEndOfWord = control->WordEndPosition(pos, true);
    int posStartOfWord = control->WordStartPosition(pos, true);
    wxString nameUnder = control->GetTextRange(posStartOfWord, posEndOfWord);
    if (nameUnder.IsEmpty())
        return;
    int lineStartPos = control->GetLineEndPosition(control->LineFromPosition(posEndOfWord) - 1) + 1;
    wxString curLine = control->GetTextRange(lineStartPos,posEndOfWord);

    ChangeLineIfRequired(ed, curLine);

    if (!FindMatchTypeComponents(ed, curLine, result, false, onlyPublicNames, isAfterPercent, true))
        return;

    if (isAfterPercent)
    {
        ChangeAddressWithInclude(result);
        return;
    }

    int tokKind = tkModule | tkSubmodule | tkFunction | tkProgram | tkSubroutine | tkPreprocessor | tkInterface |
                  tkBlockData | tkType | tkVariable | tkProcedure;
    if (onlyUseAssoc)
    {
        TokensArrayFlatClass tokensTmp;
        TokensArrayFlat* resultTmp = tokensTmp.GetTokens();
        TokensArrayFlatClass tokensTmpU;
        TokensArrayFlat* resultTmpU = tokensTmpU.GetTokens();
        FindUseAssociatedTokens(onlyPublicNames, ed, nameUnder, false, *resultTmp, tokKind, false, resultTmpU);
        FindImplementedProcInMySubmodules(ed, nameUnder, *resultTmp);
        for (size_t i=0; i<resultTmpU->GetCount(); i++)
        {
            AddUniqueResult(result, resultTmpU->Item(i));
        }
        for (size_t i=0; i<resultTmp->GetCount(); i++)
        {
            result.Add(new TokenFlat(resultTmp->Item(i)));
        }
        int noChildrenOf = tkInterface | tkModule | tkSubmodule | tkFunction | tkSubroutine | tkProgram;
        FindMatchTokensDeclared(nameUnder, result, tokKind, false, noChildrenOf, false, true); // take global procedures only
    }
    else
    {
        int noChildrenOf = tkFunction | tkSubroutine | tkProgram;
        FindMatchTokensDeclared(nameUnder, result, tokKind, false, noChildrenOf);
        FindMatchVariablesInModules(nameUnder, result, false);
    }
    FindMatchDeclarationsInCurrentScope(nameUnder, ed, result, false, posEndOfWord);

    if (result.GetCount() == 0 && IsIncludeFile(ed->GetFilename()))
    {
        FindMatchTokensAtInclude(ed, nameUnder, onlyPublicNames, false, result);
    }

    ChangeAddressWithInclude(result);
}


bool ParserF::FindMatchTokensForCodeCompletion(bool useSmartCC, bool onlyUseAssoc, bool onlyPublicNames, const wxString& nameUnderCursor, cbEditor* ed,
                                               TokensArrayFlat& result, bool& isAfterPercent, int& tokKind, wxArrayString& firstWords)
{
    wxString curLine;
    if (!FindWordsBefore(ed, 100, curLine, firstWords))  //get words on the line
        return false;

    ChangeLineIfRequired(ed, curLine);

    isAfterPercent = false;
    if (!FindMatchTypeComponents(ed, curLine, result, true, onlyPublicNames, isAfterPercent, false))
        return true;

    if (isAfterPercent)
        return true;

    bool allowVariables;
    kindOfCCList kindCC = kccOther;
    if (!useSmartCC)
    {
        tokKind = tkFunction | tkProgram | tkSubroutine | tkPreprocessor | tkInterface | tkBlockData | tkType;
        allowVariables = true;
    }
    else
    {
        CCSmartFilter::GetTokenKind(firstWords, tokKind, allowVariables, kindCC);
    }

    if (kindCC == kccUseAssocTokens)
    {
        // if we are after "use" statement
        wxString nameUnderCursorLw = nameUnderCursor.Lower();
        FindTokensForUse(nameUnderCursorLw, firstWords, result, onlyPublicNames); // we are on line with: use mod_name subr_name...
        tokKind = 0; // no keywords
    }
    else if (kindCC == kccAccessList)
    {
        // if we are after "private" or "public" or "protected" statement
        FindUseAssociatedTokens(onlyPublicNames, ed, nameUnderCursor, true, result, tokKind, true);
        FindMatchDeclarationsInCurrentScope(nameUnderCursor, ed, result, true);
        tokKind = 0; // no keywords
    }
    else if (onlyUseAssoc)
    {
        bool classVar = false;
        if (allowVariables)
        {
            tokKind = tokKind | tkVariable;
        }
        else if (firstWords.GetCount() > 0 && firstWords.Item(0).IsSameAs("call"))
        {
            tokKind = tokKind | tkVariable;
            classVar = true;
        }

        bool wasTkOtherRemoved = false;
        if (tokKind & tkOther)
        {
            tokKind = tokKind ^ tkOther;
            wasTkOtherRemoved = true;
        }
        FindUseAssociatedTokens(onlyPublicNames, ed, nameUnderCursor, true, result, tokKind, true);

        int noChildrenOf = tkInterface | tkModule | tkSubmodule | tkFunction | tkSubroutine | tkProgram;
        FindMatchTokensDeclared(nameUnderCursor, result, tokKind, true, noChildrenOf, false, true); // take global procedures only

        if (allowVariables || classVar)
        {
            FindMatchDeclarationsInCurrentScope(nameUnderCursor, ed, result, true);
        }

        if (classVar)
        {
            int i = 0;
            while (true)
            {
                if (i >= int(result.GetCount()))
                    break;
                TokenF* tok = result.Item(i);
                if ( tok->m_TokenKind == tkVariable )
                {
                    wxString tDefLow = tok->m_TypeDefinition.Lower();
                    if ( !tDefLow.StartsWith("type") && !tDefLow.StartsWith("class") )
                    {
                        result.Item(i)->Clear();
                        delete result.Item(i);
                        result.RemoveAt(i);
                        i--;
                    }
                }
                i++;
            }
        }

        if (wasTkOtherRemoved)
            tokKind = tokKind | tkOther;
    }
    else // not onlyUseAssoc
    {
        int noChildrenOf = tkInterface | tkFunction | tkSubroutine | tkProgram;
        FindMatchTokensDeclared(nameUnderCursor, result, tokKind, true, noChildrenOf, onlyPublicNames);

        if (allowVariables)
        {
            FindMatchVariablesInModules(nameUnderCursor, result, true);
            FindMatchDeclarationsInCurrentScope(nameUnderCursor, ed, result, true);
        }

        if (tokKind & tkSubroutine)
        {
            if (firstWords.GetCount() > 0 && firstWords.Item(0).IsSameAs("call"))
            {
                TokensArrayFlatClass tokensTmp;
                TokensArrayFlat* resTmp = tokensTmp.GetTokens();

                FindMatchVariablesInModules(nameUnderCursor, *resTmp, true);
                FindMatchDeclarationsInCurrentScope(nameUnderCursor, ed, *resTmp, true);

                for (size_t i=0; i<resTmp->Count(); i++)
                {
                    TokenF* tok = resTmp->Item(i);
                    if ( tok->m_TokenKind == tkVariable )
                    {
                        wxString tDefLow = tok->m_TypeDefinition.Lower();
                        if ( tDefLow.StartsWith("type") || tDefLow.StartsWith("class") )
                        {
                            result.Add(new TokenFlat(tok));
                        }
                    }
                }
            }
        }
    }

    if (result.Count() == 0 && IsIncludeFile(ed->GetFilename()))
    {
        FindMatchTokensAtInclude(ed, nameUnderCursor, onlyPublicNames, true, result);
    }

    if (tokKind & tkSubmodule)
    {
        for (size_t i=0; i<result.Count(); i++)
        {
            TokenFlat* tok = result.Item(i);
            if (tok->m_TokenKind == tkSubmodule)
            {
                wxString name = tok->m_DisplayName.BeforeFirst('(');
                tok->m_DisplayName = name.Trim();
            }
        }
    }

    if (allowVariables)
    {
        // Add #define tokens.
        FindMachDefineTokens(nameUnderCursor, ed, result);
    }

    return true;
}

bool ParserF::FindWordsBefore(cbEditor* ed, int numberOfWordsMax, wxString &curLine, wxArrayString &firstWords)
{
    /* Finds word before current word (first word).
    */
    if (!ed)
        return false;
    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return false;
    int pos   = control->GetCurrentPos();
    int lineCur = control->LineFromPosition(pos);
    int lineStartPos = control->PositionFromLine(lineCur);
    curLine = control->GetTextRange(lineStartPos,pos);
//    if (curLine.Find('!') != wxNOT_FOUND)
//        return false; // we are in comments
    wxString line = curLine;

    for (int i=lineCur-1; i>=0; i--)
    {
        wxString tmpLine = control->GetLine(i).BeforeFirst('!').Trim();
        if (tmpLine.EndsWith("&"))
        {
            // current line is continuation line
            tmpLine = tmpLine.BeforeLast('&').Trim();
            if (!tmpLine.IsEmpty())
            {
                line.Prepend(" ");
                line.Prepend(tmpLine);
            }
        }
        else if (!tmpLine.IsEmpty())
        {
            break;
        }
    }

    // end of current word
    bool found = false;
    int idx;
    for (int i=line.Len()-1; i>=0; i--)
    {
        if (!isalnum(line.GetChar(i)) && (line.GetChar(i) != '_'))
        {
            found = true;
            idx = i;
            break;
        }
    }
    if (!found)
    {
        firstWords.Add(wxEmptyString);
        return true;
    }

    for (int nword=0; nword<numberOfWordsMax; nword++)
    {
        // end of first word
        int idx_end = -1;
        for (int i=idx; i>=0; i--)
        {
            if (!isspace(line.GetChar(i)))
            {
                idx_end = i;
                break;
            }
        }
        if (idx_end == -1)
        {
            if (firstWords.Count() == 0)
                firstWords.Add(wxEmptyString);
            break;
        }
        else if (!isalnum(line.GetChar(idx_end)) && (line.GetChar(idx_end) != '_'))
        {
            firstWords.Add(line.GetChar(idx_end));
            idx = idx_end - 1;
        }
        else
        {
            // start of first word
            int idx_start = 0;
            for (int i=idx_end-1; i>=0; i--)
            {
                if (!isalnum(line.GetChar(i)) && (line.GetChar(i) != '_'))
                {
                    idx_start = i + 1;
                    break;
                }
            }
            firstWords.Add(line.Mid(idx_start, idx_end-idx_start+1).Lower());
            idx = idx_start - 1;
        }
    }
    return true;
}

bool ParserF::CutBlocks(const wxChar& ch, wxString& line)
{
	// cut blocks () [] {} <>
	wxChar match;
	switch (ch)
	{
		case '(': match = ')'; break;
		case '[': match = ']'; break;
		case '{': match = '}'; break;
		case '<': match = '>'; break;
		default : return false;
	}

    std::vector<int> startAll;
    startAll.reserve(10);
	int count = 0; // counter for nested blocks (xxx())
	int i = 0;
	int end;
	while (i < (int)line.length())
	{
	    while (i < (int)line.length())
	    {
            if (line.GetChar(i) == '"' || line.GetChar(i) == '\'')
            {
                // this is the case that match is inside a string!
                char cha = line.GetChar(i);
                i++;
                while (i < (int)line.length())
                {
                    if (line.GetChar(i) == cha)
                        break;
                    else
                        i++;
                }
                i++;
            }
            else
                break;
        }
		if (line.GetChar(i) == ch)
		{
            startAll.push_back(i);
			count++;
		}
		else if (line.GetChar(i) == match)
		{
		    if (count > 0)
		    {
                end = i;
                wxString line_new = line.Mid(0,startAll[count-1]);
                if (end+1 < (int)line.length())
                    line_new.Append(line.Mid(end+1));
                line = line_new;
                i = startAll[count-1] - 1;
                startAll.pop_back();
                count--;
		    }
		}
		i++;
	}
	return true;
}

bool ParserF::GetTypeOfComponent(const wxString& nameType, const wxString& nameComponent, wxString& nameTypeComponent)
{
    for (size_t i=0; i<m_pTokens->GetCount(); i++)
    {
        TokenF* pfToken = m_pTokens->Item(i);
        for (size_t j=0; j<pfToken->m_Children.GetCount(); j++)
        {
            TokenF* pToken = pfToken->m_Children.Item(j);
            if (pToken->m_TokenKind == tkModule)
            {
                for (size_t k=0; k<pToken->m_Children.GetCount(); k++)
                {
                    if (pToken->m_Children.Item(k)->m_TokenKind == tkType)
                    {
                        TokenF* pT = pToken->m_Children.Item(k);
                        if (pT->m_Name.IsSameAs(nameType))
                        {
                            if (GetTypeOfComponent(&pT, nameComponent, nameTypeComponent))
                                return true;
                        }
                    }
                }
            }
        }
    }
    return false;
}

bool ParserF::GetTypeOfComponent(TokenF** ppT, const wxString& nameComponent, wxString& nameTypeComponent)
{
    TokenF* pT = *ppT;
    if (GetTypeOfChild(pT, nameComponent, nameTypeComponent))
        return true;

    //Maybe nameComponent is parent type?
    if (!pT->m_ExtendsType.empty() && pT->m_ExtendsType.Lower().IsSameAs(nameComponent))
    {
        nameTypeComponent = pT->m_ExtendsType.Lower();
        return true;
    }
    else if (!pT->m_ExtendsType.IsEmpty())
    {
        for (size_t l=0; l<30; l++)
        {
            TokenF* typeToken = GetType(pT->m_ExtendsType.Lower());
            if(!typeToken)
                break; // type was not found
            if (GetTypeOfChild(typeToken, nameComponent, nameTypeComponent))
            {
                *ppT = typeToken;
                return true;
            }
            else if (!typeToken->m_ExtendsType.empty() && typeToken->m_ExtendsType.Lower().IsSameAs(nameComponent))
            {
                nameTypeComponent = typeToken->m_ExtendsType.Lower();
                return true;
            }
            else if(!typeToken->m_ExtendsType.IsEmpty())
            {
                pT = typeToken;
            }
            else
            {
                break;
            }
        }
    }
    return false;
}


bool ParserF::GetTypeOfChild(TokenF* pT, const wxString& nameComponent, wxString& nameTypeComponent)
{
    for (size_t l=0; l<pT->m_Children.GetCount(); l++)
    {
        if ((pT->m_Children.Item(l)->m_Name.IsSameAs(nameComponent)) && (pT->m_Children.Item(l)->m_TokenKind == tkVariable))
        {
            wxString tdef = pT->m_Children.Item(l)->m_TypeDefinition.Lower();
            if (tdef.StartsWith("type") || tdef.StartsWith("class"))
            {
                int idx_a = tdef.Find(")");
                int idx_b = tdef.Find("(");
                if (idx_a != wxNOT_FOUND && idx_b != wxNOT_FOUND && idx_a > idx_b)
                {
                    nameTypeComponent = tdef.Mid(idx_b+1,idx_a-idx_b-1);
                    return true;
                }
            }
            else
            {
                nameTypeComponent = tdef;
                return true;
            }
        }
    }
    return false;
}

TokenF* ParserF::GetType(const wxString& nameType)
{
    for (size_t i=0; i<m_pTokens->GetCount(); i++)
    {
        TokenF* pfToken = m_pTokens->Item(i);
        for (size_t j=0; j<pfToken->m_Children.GetCount(); j++)
        {
            TokenF* pToken = pfToken->m_Children.Item(j);
            if (pToken->m_TokenKind == tkModule)
            {
                for (size_t k=0; k<pToken->m_Children.GetCount(); k++)
                {
                    if (pToken->m_Children.Item(k)->m_TokenKind == tkType)
                    {
                        TokenF* pT = pToken->m_Children.Item(k);
                        if (pT->m_Name.IsSameAs(nameType))
                        {
                            return pT;
                        }
                    }
                }
            }
        }
    }
    return NULL;
}

TokenF* ParserF::GetTypeInFile(const wxString& fileName, const unsigned int line, const wxString& nameType)
{
    TokensArrayF tokens;
    FindMatchTokens(fileName, nameType, tokens);
    for (size_t i=0; i<tokens.GetCount(); i++)
    {
        if (tokens.Item(i)->m_TokenKind == tkType && tokens.Item(i)->m_LineStart == line
            && tokens.Item(i)->m_Name.IsSameAs(nameType))
        {
            return tokens.Item(i);
        }
    }
    return NULL;
}

void ParserF::GetTypeComponentsInFile(const wxString& fileName, const unsigned int line, const wxString& nameType, TokensArrayFlat* result)
{
    wxCriticalSectionLocker locker(s_CritSect);

    TokenF* typeToken = GetTypeInFile(fileName, line, nameType);
    if (!typeToken)
        return;

    for (size_t i=0; i<typeToken->m_Children.GetCount(); i++)
    {
        TokenF* tokenCh = typeToken->m_Children.Item(i);
        result->Add(new TokenFlat(tokenCh));
    }
}

bool ParserF::FindTokenDeclaration(TokenFlat& token, const wxString& argName, wxString& argDecl, wxString& argDescription)
{
    TokenF * pTok = FindToken(token);
    if (!pTok)
        return false;
    TokensArrayF* pChildren = &pTok->m_Children;

    bool found = false;
    wxString argNameLw = argName.Lower();
    for (size_t i=0; i<pChildren->GetCount(); i++)
    {
        if (pChildren->Item(i)->m_Name.IsSameAs(argNameLw))
        {
            if (pChildren->Item(i)->m_TokenKind == tkProcedure)
            {
                argDecl << "procedure(" << pChildren->Item(i)->m_PartLast << ") :: "
                        << pChildren->Item(i)->m_DisplayName;
            }
            else
            {
                argDecl << pChildren->Item(i)->m_TypeDefinition << " :: "
                        << pChildren->Item(i)->m_DisplayName << pChildren->Item(i)->m_Args;
                argDescription << HtmlDoc::GetDocShort(pChildren->Item(i)->m_DocString);
            }
            found = true;
            break;
        }
    }
    return found;
}

bool ParserF::FindTokenRange(TokenFlat& token, wxString& txtRange)
{
    wxString buff;
    std::vector<int> lineStarts;
    return FindTokenRange(token, txtRange, buff, lineStarts);
}

bool ParserF::FindTokenRange(TokenFlat& token, wxString& txtRange, wxString& buff, std::vector<int> &lineStarts, bool withDefinition, bool readFile)
{
    if (!wxFileExists(token.m_Filename))
        return false;

    if (readFile)
    {
        lineStarts.clear();
        buff.Empty();

        if (!Manager::Get()->GetEditorManager())
            return false;

        cbEditor* ed = Manager::Get()->GetEditorManager()->IsBuiltinOpen(token.m_Filename);
        if (ed) // File is opened
        {
            cbStyledTextCtrl* control = ed->GetControl();
            if (!control)
                return false;
            buff = control->GetText();
        }
        else
        {
            wxFile file(token.m_Filename);
            if (!ReadFileToString(file,buff))
                return false;
        }
        lineStarts.push_back(0);
    }
    else
    {
        //use buff content
    }

    //get range of token
    size_t pos_start=0;
    size_t pos_end=0;
    size_t line = 1;
    unsigned int lStart = token.m_LineStart;
    if (!withDefinition)
        lStart += token.m_DefinitionLength;


    bool startFound = false;
    bool endFound = false;
    for (size_t i=0; i<buff.length(); i++)
    {
        if (!startFound && lStart <= line)
        {
            pos_start = i;
            startFound = true;
        }
        else if (!endFound && token.m_LineEnd < line)
        {
            pos_end = i;
            endFound = true;
            if (!readFile)
                break;
        }

        if (buff.GetChar(i) == '\n')
        {
            line++;
            if (readFile)
                lineStarts.push_back(i+1);
        }
    }
    if (!startFound)
        return false;

    if (pos_start > pos_end)
    {
        pos_end = buff.length();
    }
    txtRange = buff.Mid(pos_start, pos_end - pos_start);
    return true;
}

bool ParserF::FindInfoLog(TokenFlat& token, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg)
{
    wxString argsNew = wxEmptyString;
    bool readFile = true;
    return FindInfoLog(token, logComAbove, logComBelow, logDeclar, logComVariab, msg, argsNew, readFile);
}

bool ParserF::FindInfoLog(TokenFlat& token, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg,
                          bool readFile)
{
    wxString argsNew = wxEmptyString;
    return FindInfoLog(token, logComAbove, logComBelow, logDeclar, logComVariab, msg, argsNew, readFile);
}

bool ParserF::FindInfoLog(TokenFlat& token, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg,
                          wxString& argsNew)
{
    bool readFile = true;
    return FindInfoLog(token, logComAbove, logComBelow, logDeclar, logComVariab, msg, argsNew, readFile);
}


bool ParserF::FindInfoLog(TokenFlat& token, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg,
                          const wxString& argsNew, bool readFile)
{
    wxString txtRange;
    if (!FindTokenRange(token, txtRange, m_Buff, m_LineStarts, false, readFile))
        return false;

    FortranSourceForm fsForm;
    if (!IsFileFortran(token.m_Filename, fsForm))
        return false;

    //Parse
    TokensArrayClass tokensTmp;
    TokensArrayF* parsResult = tokensTmp.GetTokens();
    ParserThreadF thread = ParserThreadF(wxEmptyString, txtRange, parsResult, fsForm, true, NULL, m_InterpretCPP);

    if (logComAbove)
    {
        // insert comments above
        wxArrayString comAbove;
        bool startDoxy = false;
        bool allowSimple = true;
        int endFor = std::max(int(token.m_LineStart)-100, 0);
        for (int i=token.m_LineStart-1; i>endFor; i--)
        {
            wxString str1 = m_Buff.Mid(m_LineStarts[i-1], m_LineStarts[i]-m_LineStarts[i-1]).Trim(false);
            if ( str1.empty() && startDoxy )
            {
                break;
            }
            else if ( str1.StartsWith("!>") || str1.StartsWith("!<") || str1.StartsWith("!!") )
            {
                comAbove.Add(str1);
                startDoxy = true;
            }
            else if ( allowSimple && str1.StartsWith("!") )
            {
                comAbove.Add(str1);
            }
            else if ( str1.empty() )
            {
                allowSimple = false;
            }
            else
            {
                break;
            }
        }

        for (int i=comAbove.GetCount()-1; i>=0; i--)
        {
            msg << comAbove.Item(i);
        }
    }

    if (token.m_TokenKind != tkType)
        thread.ParseDeclarations();

    if (token.m_TokenKind == tkSubroutine)
    {
        if (token.m_Name.StartsWith("__fortran_statement_"))
        {
            msg << token.m_Name.Mid(20).Upper();
        }
        else
            msg << "subroutine " << token.m_DisplayName;
        if (argsNew.IsEmpty())
            msg << token.m_Args << "\n";
        else
            msg << argsNew << "\n";
    }
    else if (token.m_TokenKind == tkFunction)
    {
        if (!token.m_PartFirst.IsEmpty())
        {
            msg << token.m_PartFirst << " ";
        }
        msg << "function " << token.m_DisplayName;
        if (argsNew.IsEmpty())
            msg << token.m_Args;
        else
            msg << argsNew;
        if (!token.m_PartLast.IsEmpty())
        {
            msg << " " << token.m_PartLast;
        }
        msg << "\n";
    }
    else if (token.m_TokenKind == tkType)
    {
        for (size_t i=token.m_LineStart-1; i<token.m_LineEnd; i++)
        {
            size_t slen;
            if (i+1 < m_LineStarts.size())
                slen = m_LineStarts[i+1] - m_LineStarts[i];
            else
                slen = m_Buff.length() - m_LineStarts[i];
            wxString str1 = m_Buff.Mid(m_LineStarts[i], slen).Trim(false).Trim();
            if (i+1 == token.m_LineStart || i+1 == token.m_LineEnd)
                msg << str1 << "\n";
            else if (str1.BeforeFirst('!').Trim().Lower().IsSameAs("contains"))
                msg << str1 << "\n";
            else
                msg << "    " << str1 << "\n";
        }
    }

    if (logComBelow)
    {
        // insert comments below
        unsigned int lStart = token.m_LineStart + token.m_DefinitionLength;
        for (unsigned int i=lStart; i<token.m_LineEnd; i++)
        {
            wxString str1 = m_Buff.Mid(m_LineStarts[i-1], m_LineStarts[i]-m_LineStarts[i-1]).Trim(false);
            if (str1.StartsWith("!"))
            {
                msg << "    " << str1;
            }
            else
            {
                break;
            }
        }
    }

    wxArrayString argMsgArr;
    int maxLenArg = 0;
    std::vector<size_t> idxOrder;

    if (logDeclar && token.m_TokenKind != tkType)
    {
        wxArrayString argArr;
        wxStringTokenizer tkz(token.m_Args, "(),[] \t\r\n", wxTOKEN_STRTOK );
        while ( tkz.HasMoreTokens() )
        {
            argArr.Add(tkz.GetNextToken());
        }

        if (token.m_TokenKind == tkFunction)
        {
            wxString arg1;
            if (!token.m_ResultVariable.IsEmpty())
            {
                arg1 = token.m_ResultVariable.Lower();
            }
            else
            {
                arg1 = token.m_Name.Lower();
            }
            for (size_t i=0; i<parsResult->GetCount(); i++)
            {
                if (parsResult->Item(i)->m_Name.IsSameAs(arg1))
                {
                    msg << "    " << parsResult->Item(i)->m_TypeDefinition << " :: "
                        << parsResult->Item(i)->m_DisplayName << parsResult->Item(i)->m_Args;
                    if (parsResult->Item(i)->m_DocString.length() > 0)
                        msg << " ! " << parsResult->Item(i)->m_DocString;
                    msg << "\n";
                    break;
                }
            }
        }

        for (size_t j=0; j<argArr.Count(); j++)
        {
            wxString msg1;
            wxString arg1 = argArr.Item(j).Lower();
            for (size_t i=0; i<parsResult->GetCount(); i++)
            {
                if (parsResult->Item(i)->m_Name.IsSameAs(arg1))
                {
                    if (parsResult->Item(i)->m_TokenKind == tkProcedure)
                    {
                        msg1 << "    " << parsResult->Item(i)->m_TypeDefinition
                             << " :: " << parsResult->Item(i)->m_DisplayName;
                        if (parsResult->Item(i)->m_DocString.length() > 0)
                             msg1 << " ! " << parsResult->Item(i)->m_DocString;
                    }
                    else
                    {
                        msg1 << "    " << parsResult->Item(i)->m_TypeDefinition << " :: "
                             << parsResult->Item(i)->m_DisplayName << parsResult->Item(i)->m_Args;
                        if (parsResult->Item(i)->m_DocString.length() > 0)
                             msg1 << " ! " << parsResult->Item(i)->m_DocString;
                    }
                    idxOrder.push_back(i);
                    argMsgArr.Add(msg1);
                    int ln = msg1.Len();
                    if (ln > maxLenArg)
                        maxLenArg = ln;
                    break;
                }
            }
        }
    }

    if (token.m_TokenKind != tkType)
    {
        if (maxLenArg >= 60)
            maxLenArg = 60;

        for (size_t j=0; j<argMsgArr.Count(); j++)
        {
            msg << argMsgArr.Item(j);
            if (logComVariab &&
                parsResult->Item(idxOrder[j])->m_TokenKind != tkProcedure)
            {
                wxString spaces;
                int nspaces = maxLenArg - argMsgArr.Item(j).Len() + 1;
                if (nspaces < 1)
                    nspaces = 1;
                spaces.Append(' ',nspaces);
                msg << spaces << parsResult->Item(idxOrder[j])->m_PartLast << "\n";
            }
            else
            {
                msg << "\n";
            }
        }
    }

    if (token.m_ParentTokenKind == tkModule)
    {
        msg << "!" + _("Module") + ": " << token.m_ParentDisplayName << ". " + _("File") + ": ";
    }
    else
    {
        msg << "!" + _("File") + ": " ;
    }
    msg << token.m_Filename.AfterLast(wxFILE_SEP_PATH) << ":" << token.m_LineStart;
    return true;
}

bool ParserF::FindTooltipForTypeBoundProc(wxString& msg, TokenFlat* token1, TokenFlat* token2)
{
    if (!token1 || token1->m_TokenKind != tkProcedure)
        return false;
    wxString txtRange;
    wxString buff;
    std::vector<int> lineStarts;
    if (!FindTokenRange(*token1, txtRange, buff, lineStarts, true))
        return false;
    int ic = txtRange.Find("::");
    if (ic == wxNOT_FOUND)
    {
        msg << "procedure " << token1->m_DisplayName;
        if (!token1->m_Args.IsEmpty())
        {
            msg << "(" << token1->m_Args << ")";
        }
    }
    else
    {
        msg << txtRange.Mid(0,ic+2).Trim(false) << " " << token1->m_DisplayName;
    }
    if (!token1->m_PartLast.IsEmpty())
    {
        msg << " => " << token1->m_PartLast;
    }
    msg << "\n";

    if (token2)
    {
        if (token2->m_TokenKind == tkSubroutine || token2->m_TokenKind == tkFunction)
        {
            wxString pass_arg = token1->m_Args;
            int start = 0;
            int end = 0;
            if (token1->m_Pass && !pass_arg.IsEmpty())
            {
                GetPossitionOfDummyArgument(token2->m_Args, pass_arg, start, end);
            }
            else if (token1->m_Pass)
            {
                GetCallTipHighlight(token2->m_Args, 0, start, end);
            }
            wxString argNew;
            if (end > start)
            {
                argNew << token2->m_Args.Mid(0,start) << "[";
                wxString secPart = token2->m_Args.Mid(start);
                int icom = secPart.Find(",");
                if (icom != wxNOT_FOUND)
                {
                    argNew << secPart.Mid(0,icom+1) << "]" << secPart.Mid(icom+1);
                }
                else
                {
                    argNew << token2->m_Args.Mid(start,end-start) << "]" << token2->m_Args.Mid(end);
                }
            }
            else
            {
                argNew = token2->m_Args;
            }
            if (token2->m_TokenKind == tkSubroutine)
            {
                msg << "subroutine " << token2->m_DisplayName << argNew << "\n";
            }
            else if (token2->m_TokenKind == tkFunction)
            {
                if (!token2->m_PartFirst.IsEmpty())
                {
                    msg << token2->m_PartFirst << " ";
                }
                msg << "function " << token2->m_DisplayName << argNew << "\n";
            }
        }
    }
    if (!token1->m_Filename.IsEmpty())
    {
        msg << token1->m_Filename.AfterLast(wxFILE_SEP_PATH) << ":" << token1->m_LineStart;
    }
    return true;
}

bool ParserF::FindInfoLogForTypeBoundProc(TokensArrayFlat& tokenPair, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg,
                                          wxString* buff, std::vector<int>* lineStarts)
{
    if (tokenPair.GetCount() == 0)
    {
        return false;
    }

    TokenFlat* token1 = tokenPair.Item(0);
    if (token1->m_TokenKind != tkProcedure)
        return false;
    wxString txtRange;
    if (!buff)
    {
        buff= new wxString();
        lineStarts = new std::vector<int>;
        if (!FindTokenRange(*token1, txtRange, *buff, *lineStarts, true))
            return false;
        delete buff;
        delete lineStarts;
    }
    else
    {
        if (!FindTokenRange(*token1, txtRange, *buff, *lineStarts, true, false))
            return false;
    }
    int ic = txtRange.Find("::");
    if (ic == wxNOT_FOUND)
    {
        msg << "procedure " << token1->m_DisplayName;
        if (token1->m_IsAbstract)
            msg << "(" << token1->m_PartLast << ")";
    }
    else
    {
        msg << txtRange.Mid(0,ic+2).Trim(false) << " " << token1->m_DisplayName;
    }

    if (!token1->m_PartLast.empty() && !token1->m_IsAbstract)
    {
        msg << " => " << token1->m_PartLast;
    }
    msg << "\n!" + _("File") + ": " << token1->m_Filename.AfterLast(wxFILE_SEP_PATH) << ":" << token1->m_LineStart << "\n";

    if (tokenPair.GetCount() > 1)
    {
        TokenFlat* token = tokenPair.Item(1);
        if (token->m_TokenKind == tkSubroutine || token->m_TokenKind == tkFunction)
        {
            wxString pass_arg = token1->m_Args;
            int start = 0;
            int end = 0;
            if (token1->m_Pass && !pass_arg.IsEmpty())
            {
                GetPossitionOfDummyArgument(token->m_Args, pass_arg, start, end);
            }
            else if (token1->m_Pass)
            {
                GetCallTipHighlight(token->m_Args, 0, start, end);
            }
            if (end > start)
            {
                wxString argNew;
                argNew << token->m_Args.Mid(0,start) << "[";
                wxString secPart = token->m_Args.Mid(start);
                int icom = secPart.Find(",");
                if (icom != wxNOT_FOUND)
                {
                    argNew << secPart.Mid(0,icom+1) << "]" << secPart.Mid(icom+1);
                }
                else
                {
                    argNew << token->m_Args.Mid(start,end-start) << "]" << token->m_Args.Mid(end);
                }
                FindInfoLog(*token, logComAbove, logComBelow, logDeclar, logComVariab, msg, argNew);
            }
            else
            {
                FindInfoLog(*token, logComAbove, logComBelow, logDeclar, logComVariab, msg);
            }
        }
    }
    return true;
}

bool ParserF::FindInfoLogForGenericTBProc(TokensArrayFlat& tokens, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg)
{
    if (tokens.GetCount() == 0 || tokens.Item(0)->m_TokenKind != tkInterface)
    {
        return false;
    }

    wxString buff;
    std::vector<int> lineStarts;
    size_t iInt = 0;
    wxString filName;
    wxString msgProc;

    while ( iInt < tokens.GetCount() )
    {
        TokenFlat* token = tokens.Item(iInt);
        if (token->m_TokenKind != tkInterface)
            break;
        wxString tokRan;
        if (iInt == 0 || !filName.IsSameAs(token->m_Filename))
        {
            if (!FindTokenRange(*token, tokRan, buff, lineStarts, true))
                return false;
            filName = token->m_Filename;
        }
        else
        {
            if (!FindTokenRange(*token, tokRan, buff, lineStarts, true, false))
                return false;
        }
        msg.Append("\n");
        msg.Append( tokRan.Trim().Trim(false) );

        if (token->m_ParentTokenKind == tkType)
        {
            msg << "\n!" + _("Type") + ": " << token->m_ParentDisplayName << ". " + _("File") + ": ";
        }
        msg << token->m_Filename.AfterLast(wxFILE_SEP_PATH) << ":" << token->m_LineStart;

        size_t i = iInt + 1;
        while ( i < tokens.GetCount()-1 )
        {
            if ( tokens.Item(i)->m_TokenKind == tkInterface )
                break;
            msgProc << "\n!---------------------\n";
            TokensArrayFlatClass tokensTmpCl;
            TokensArrayFlat* tokensTmp = tokensTmpCl.GetTokens();
            tokensTmp->Add(new TokenFlat(tokens.Item(i)));
            tokensTmp->Add(new TokenFlat(tokens.Item(i+1)));
            FindInfoLogForTypeBoundProc(*tokensTmp, logComAbove, logComBelow, logDeclar, logComVariab, msgProc, &buff, &lineStarts);
            i += 2;
        }
        iInt = i;
    }
    msg.Trim(false).Append(msgProc);

    return true;
}

bool ParserF::GetTokenStr(TokenFlat& token, wxString& msg)
{
    wxString buff;
    std::vector<int> lineStarts;
    if (!FindTokenRange(token, msg, buff, lineStarts, true))
        return false;

    if (token.m_ParentTokenKind == tkModule)
    {
        msg << "\n!" + _("Module") + ": " << token.m_ParentDisplayName << ". " + _("File") + ": ";
    }
    else
    {
        msg << "\n!" + _("File") + ": ";
    }
    msg << token.m_Filename.AfterLast(wxFILE_SEP_PATH) << ":" << token.m_LineStart;
    return true;
}

void ParserF::FindChildrenOfInterface(TokenFlat* token, TokensArrayFlat& result)
{
    if (token->m_ParentTokenKind != tkModule)
        return;

    TokensArrayF* pFileChildren = FindFileTokens(token->m_Filename);
    if (!pFileChildren)
        return;

    for (size_t j=0; j < pFileChildren->GetCount(); j++)
    {
        if (pFileChildren->Item(j)->m_TokenKind == tkModule && pFileChildren->Item(j)->m_Name.IsSameAs(token->m_ParentName))
        {
            TokensArrayF* pModChildren = &pFileChildren->Item(j)->m_Children;
            for (size_t k=0; k < pModChildren->GetCount(); k++)
            {
                if (pModChildren->Item(k)->m_Name.IsSameAs(token->m_Name) && pModChildren->Item(k)->m_TokenKind == tkInterface)
                {
                    wxArrayString address;
                    address.Add(pFileChildren->Item(j)->m_Filename);
                    address.Add(pFileChildren->Item(j)->m_Name);
                    int tokenKindMask = tkSubroutine | tkFunction;
                    TokensArrayF* pIntChildren = &pModChildren->Item(k)->m_Children;
                    for (size_t l=0; l < pIntChildren->GetCount(); l++)
                    {
                        if (pIntChildren->Item(l)->m_TokenKind & tokenKindMask)
                        {
                            result.Add(new TokenFlat(pIntChildren->Item(l)));
                        }
                        else
                        {
                            wxString name = pIntChildren->Item(l)->m_Name;
                            FindUseAssociatedTokens(true, address, name, false, result, tokenKindMask | tkInterface, false);
                        }
                    }
                    break;
                }
            }
            break;
        }
    }
}

void ParserF::GetPossitionOfDummyArgument(const wxString& args, const wxString& arg, int& start, int& end)
{
    wxStringTokenizer tkz(args, " ,\t\r\n()", wxTOKEN_STRTOK);
    while ( tkz.HasMoreTokens() )
    {
        wxString token = tkz.GetNextToken();
        if (token.IsSameAs(arg))
        {
            end = tkz.GetPosition() - 1;
            start = end - token.length();
            break;
        }
    }
}

void ParserF::GetCallTipHighlight(const wxString& calltip, int commasWas, int& start, int& end)
{
    int pos = 1; // skip opening parenthesis
    int nest = 0;
    int commas = 0;
    start = 1;
    end = 0;
    while (true)
    {
        wxChar c = calltip.GetChar(pos++);
        if (c == '\0')
            break;
        else if (c == '(')
            ++nest;
        else if (c == ')')
            --nest;
        else if (c == ',' && nest <= 0)
        {
            ++commas;
            if (commas == commasWas)
            {
                start = pos;
            }
            else if (commas == commasWas + 1)
            {
                end = pos; // already incremented
                break;
            }
        }
    }
    if (end == 0)
        end = calltip.length() - 1;
    if (commas < commasWas)
    {
        start = end; //no highlight
    }
}

void ParserF::FindUseAssociatedTokens(bool onlyPublicNames, TokenFlat* tok, const wxString& search, bool partialMatch, TokensArrayFlat& result, int tokenKindMask,
                                      bool changeDisplayName, TokensArrayFlat* useWithRenameTok)
{
    wxArrayString address; // [file_name, module_name, function_name, etc.]
    FindAddress(tok, address);
    if (address.Count() < 2)
        return; // file only

    FindUseAssociatedTokens(onlyPublicNames, address, search, partialMatch, result, tokenKindMask, changeDisplayName, useWithRenameTok);
}

void ParserF::FindUseAssociatedTokens(bool onlyPublicNames, cbEditor* ed, const wxString& search, bool partialMatch, TokensArrayFlat& result, int tokenKindMask,
                                      bool changeDisplayName, TokensArrayFlat* useWithRenameTok)
{
    wxArrayString address; // [file_name, module_name, function_name, etc.]
    FindAddress(ed, address);
    if (address.Count() < 2)
        return; // file only

    FindUseAssociatedTokens(onlyPublicNames, address, search, partialMatch, result, tokenKindMask, changeDisplayName, useWithRenameTok);
}

void ParserF::FindUseAssociatedTokens(bool onlyPublicNames, wxArrayString& address, const wxString& search, bool partialMatch, TokensArrayFlat& result, int tokenKindMask,
                                      bool changeDisplayName, TokensArrayFlat* useWithRenameTok)
{
    wxString searchLw = search.Lower();
    wxCriticalSectionLocker locker(s_CritSect);
    if (address.Count() == 0)
        return;
    TokensArrayF* children = FindFileTokens(address.Item(0));
    if (!children)
        return;

    int callMask = tkCallFunction | tkCallSubroutine;
    std::vector<TokensArrayF*> vpChildren;
    TokensArrayF  useTokens;
    bool found = false;
    TokenF* subModToken = NULL;
    TokenF* procedureToken = NULL;
    for (size_t j=1; j<address.Count(); j++)
    {
        if (address.Item(j).IsEmpty())
            break;

        bool isInterfaceExp = address.Item(j).IsSameAs("%%tkInterfaceExplicit");
        found = false;
        for (size_t i=0; i<children->GetCount(); i++)
        {
            if (children->Item(i)->m_Name.IsSameAs(address.Item(j)) && !(children->Item(i)->m_TokenKind & callMask))
            {
                vpChildren.push_back(&children->Item(i)->m_Children);
                found = true;
                if (children->Item(i)->m_TokenKind == tkSubmodule)
                    subModToken = children->Item(i);
                else if (children->Item(i)->m_TokenKind == tkProcedure)
                    procedureToken = children->Item(i);
                children = &children->Item(i)->m_Children;
                break;
            }
            else if (isInterfaceExp && children->Item(i)->m_TokenKind == tkInterfaceExplicit
                     && children->Item(i)->m_Children.GetCount() > 0
                     && j+1 < address.Count())
            {
                TokensArrayF* childrenIntExp = &children->Item(i)->m_Children;
                for (size_t k=0; k<childrenIntExp->GetCount(); k++)
                {
                    if (childrenIntExp->Item(k)->m_Name.IsSameAs(address.Item(j+1)))
                    {
                        vpChildren.push_back(&children->Item(i)->m_Children);
                        found = true;
                        children = childrenIntExp;
                        break;
                    }
                }
            }
        }
        if (!found)
            break;
    }

    bool found_full_adress = (vpChildren.size() == address.size()-1);
    size_t sizeChildren = vpChildren.size();

    if (subModToken)
    {
        m_SubmodDeep = 0;
        size_t oldCountCh = vpChildren.size();
        GetSubmoduleHostTokens(subModToken, vpChildren);

        if (procedureToken)
        {
            found = false;
            int prTMask = tkFunction | tkSubroutine;
            for (size_t ichil=oldCountCh; ichil<vpChildren.size(); ichil++)
            {
                TokensArrayF* pParChildren = vpChildren[ichil];
                for (size_t i=0; i<pParChildren->GetCount(); i++)
                {
                    if ((pParChildren->Item(i)->m_TokenKind == tkInterfaceExplicit) ||
                        (pParChildren->Item(i)->m_TokenKind == tkInterface))
                    {
                        TokensArrayF* pIntCh = &pParChildren->Item(i)->m_Children;
                        for (size_t k=0; k<pIntCh->GetCount(); k++)
                        {
                            if ((pIntCh->Item(k)->m_TokenKind & prTMask) &&
                                pIntCh->Item(k)->m_Name.IsSameAs(procedureToken->m_Name) &&
                                pIntCh->Item(k)->m_Children.GetCount())
                            {
                                vpChildren.push_back(&pIntCh->Item(k)->m_Children);
                                found = true;
                                break;
                            }
                        }
                        if (found)
                            break;
                    }
                }
                if (found)
                    break;
            }
        }
    }

    int numInclude = 0;
    for (size_t ichil=0; ichil<vpChildren.size(); ichil++)
    {
        TokensArrayF* pParChildren = vpChildren[ichil];
        for (size_t i=0; i<pParChildren->GetCount(); i++)
        {
            if (pParChildren->Item(i)->m_TokenKind == tkUse)
            {
                useTokens.Add(pParChildren->Item(i));
            }
            else if (pParChildren->Item(i)->m_TokenKind == tkInclude)
            {
                TokensArrayF* includedTokens = new TokensArrayF();
                AddIncludeFileChildren(pParChildren->Item(i), *includedTokens);
                vpChildren.push_back(includedTokens);
                numInclude++;
            }
            else if ((ichil == sizeChildren-1) && found_full_adress &&
                     pParChildren->Item(i)->m_TokenKind == tkVariable)
            {
                //don't take locally declared variables
            }
            else if (pParChildren->Item(i)->m_TokenKind & tokenKindMask)
            {
                if ((partialMatch && pParChildren->Item(i)->m_Name.StartsWith(searchLw)) ||
                    (!partialMatch && pParChildren->Item(i)->m_Name.IsSameAs(searchLw)))
                {
                    AddUniqueResult(result, pParChildren->Item(i), true);
                }
            }

            if (pParChildren->Item(i)->m_TokenKind == tkInterfaceExplicit ||
                     pParChildren->Item(i)->m_TokenKind == tkInterface)
            {
                TokensArrayF* pEICh = &pParChildren->Item(i)->m_Children;
                for (size_t ie=0; ie<pEICh->GetCount(); ie++)
                {
                    if (pEICh->Item(ie)->m_TokenKind & tokenKindMask)
                    {
                        if ((partialMatch && pEICh->Item(ie)->m_Name.StartsWith(searchLw)) ||
                            (!partialMatch && pEICh->Item(ie)->m_Name.IsSameAs(searchLw)))
                        {
                            AddUniqueResult(result, pEICh->Item(ie), true);
                        }
                    }
                }
            }
        }
    }
    if (numInclude > 0)
    {
        size_t origSize = vpChildren.size() - numInclude;
        for (size_t ichil=origSize; ichil<vpChildren.size(); ichil++)
        {
            delete vpChildren[ichil];
        }
    }

    m_RecursiveDeep = 0;
    m_UseRenameArrays = false;
    m_RenameDeep = 0;
    m_IncludeDeep = 0;

    for (size_t i=0; i<useTokens.Count(); i++)
    {
        ArrOfSizeT resChildrenIdx;
        BoolArray2D resCanBeSeen2D;
        TokensArrayFlatClass renTokCl;
        TokensArrayFlat* renamedTokens = renTokCl.GetTokens();

        FindUseAssociatedTokens2(useTokens.Item(i), searchLw, resChildrenIdx, resCanBeSeen2D, tokenKindMask, partialMatch,
                                  changeDisplayName, onlyPublicNames, *renamedTokens, useWithRenameTok);

        for (size_t ia=0; ia<resChildrenIdx.GetCount(); ia++)
        {
            TokensArrayFlat* pasTokens = m_PassedTokensVisited[resChildrenIdx.Item(ia)];
            BoolArray1D* canSee = resCanBeSeen2D[ia];
            for (size_t j=0; j<canSee->size(); j++)
            {
                if ((*canSee)[j])
                {
                    AddUniqueResult(result, pasTokens->Item(j));
                }
            }
        }
        for (size_t ia=0; ia<renamedTokens->GetCount(); ia++)
        {
            AddUniqueResult(result, renamedTokens->Item(ia));
        }
        ClearBoolArray2D(resCanBeSeen2D);
    }
    m_VisitedModules.Clear();
    ClearPassedTokensArray2D(m_PassedTokensVisited);
    ClearArrOfSizeT2D(m_ChildrenIdxVisited);
    ClearBoolArray3D(m_CanBeSeenVisited);
}

//void ParserF::GetAddress(TokenF* token, wxArrayString& address)
//{
////    if (token->m_TokenKind == tkFile)
////        address.Insert(token->m_Filename, 0);
////    else
////    {
////        address.Insert(token->m_Name,0);
////        GetAddress(token->m_pParent, address);
////    }
//
//    GetAddressOfToken(token, address);
//}

void ParserF::FindAddress(cbEditor* ed, wxArrayString& address)
{
    // Address is: fileName, module_name, sub_name and etc.
    int lineStart;
    TokenFlat* tokFl=NULL;
    FindLineScopeLN(ed, lineStart, tokFl, -1);
    if (!tokFl)
    {
        address.Add(UnixFilename(ed->GetFilename()));
        return;
    }

    FindAddress(tokFl, address);
    if (tokFl)
        delete tokFl;
}

void ParserF::FindAddress(TokenFlat* tokFl, wxArrayString& address)
{
    if (!tokFl)
        return;

    // Address is: fileName, module_name, sub_name and etc.
    address.Add(tokFl->m_Filename);
    if (tokFl->m_TokenKind == tkModule || tokFl->m_TokenKind == tkSubmodule)
    {
        address.Add(tokFl->m_Name);
    }
    else if (!tokFl->m_ParentName.empty() && tokFl->m_ParentTokenKind == tkFile)
    {
        address.Add(tokFl->m_Name);
    }
    else if (!tokFl->m_ParentName.empty() && (tokFl->m_ParentTokenKind == tkModule || tokFl->m_ParentTokenKind == tkSubmodule))
    {
        address.Add(tokFl->m_ParentName);
        address.Add(tokFl->m_Name);
    }
    else if (!tokFl->m_ParentName.IsEmpty())
    {
        wxArrayString guess;
        TokensArrayF* fileChildren = FindFileTokens(tokFl->m_Filename);
        if (fileChildren)
        {
            TokenF* token = FindToken(*tokFl, fileChildren);
            guess.Clear();
            while (token)
            {
                if (token->m_TokenKind != tkFile)
                    guess.Add(token->m_Name);
                token = token->m_pParent;
            }
        }

        if (guess.Count() > 0)
        {
            for (int i=guess.GetCount()-1; i>=0; i--)
            {
                address.Add(guess.Item(size_t(i)));
            }
        }
    }
    else // no parent name
    {
        bool found = false;
        wxArrayString guess;
        int lineDifStart = 0;
        bool foundGuess = false;
        wxString tokFlname = tokFl->m_Name;
        bool operIntf = tokFlname.StartsWith("%%operator (");
        if (operIntf)
            tokFlname = tokFlname.BeforeFirst('#').Trim();
        int tokenKindMask = tkFunction | tkProgram | tkSubroutine | tkModule | tkSubmodule |
                            tkInterfaceExplicit | tkProcedure;
        TokensArrayF* fileChildren = FindFileTokens(tokFl->m_Filename);
        if (fileChildren)
        {
            for (size_t i=0; i<fileChildren->GetCount(); i++)
            {
                if (fileChildren->Item(i)->m_TokenKind == tokFl->m_TokenKind && fileChildren->Item(i)->m_Name.IsSameAs(tokFlname))
                {
                    if (fileChildren->Item(i)->m_LineStart == tokFl->m_LineStart)
                    {
                        guess.Clear();
                        guess.Add(tokFlname);
                        found = true;
                        break;
                    }
                    else
                    {
                        int lds = abs(int(fileChildren->Item(i)->m_LineStart) - int(tokFl->m_LineStart));
                        if ((foundGuess && lineDifStart > lds) || !foundGuess)
                        {
                            guess.Clear();
                            guess.Add(tokFlname);
                            lineDifStart = lds;
                            foundGuess = true;
                        }
                    }
                }
                else if (fileChildren->Item(i)->m_TokenKind & tokenKindMask)
                {
                    TokensArrayF* childL1 = &(fileChildren->Item(i)->m_Children);
                    for (size_t j=0; j<childL1->GetCount(); j++)
                    {
                        bool isInterfaceExp = childL1->Item(j)->m_TokenKind == tkInterfaceExplicit;
                        wxString childL1name;
                        if (operIntf && childL1->Item(j)->m_TokenKind == tkInterface &&
                            childL1->Item(j)->m_Name.StartsWith("%%operator ("))
                        {
                            childL1name = childL1->Item(j)->m_Name.BeforeFirst('#').Trim();
                        }
                        else
                        {
                            childL1name = childL1->Item(j)->m_Name;
                        }
                        if (childL1->Item(j)->m_TokenKind == tokFl->m_TokenKind && childL1name.IsSameAs(tokFlname))
                        {
                            if (childL1->Item(j)->m_LineStart == tokFl->m_LineStart)
                            {
                                guess.Clear();
                                guess.Add(fileChildren->Item(i)->m_Name);
                                guess.Add(tokFlname);
                                found =  true;
                                break;
                            }
                            else
                            {
                                int lds = abs(int(childL1->Item(j)->m_LineStart) - int(tokFl->m_LineStart));
                                if ((foundGuess && lineDifStart > lds) || !foundGuess)
                                {
                                    guess.Clear();
                                    guess.Add(fileChildren->Item(i)->m_Name);
                                    guess.Add(tokFlname);
                                    lineDifStart = lds;
                                    foundGuess = true;
                                }
                            }
                        }
                        else if (childL1->Item(j)->m_TokenKind & tokenKindMask)
                        {
                            TokensArrayF* childL2 = &(childL1->Item(j)->m_Children);
                            for (size_t k=0; k<childL2->Count(); k++)
                            {
                                if (childL2->Item(k)->m_TokenKind == tokFl->m_TokenKind && childL2->Item(k)->m_Name.IsSameAs(tokFlname))
                                {
                                    if (childL2->Item(k)->m_LineStart == tokFl->m_LineStart)
                                    {
                                        guess.Clear();
                                        guess.Add(fileChildren->Item(i)->m_Name);
                                        if (isInterfaceExp && childL1->Item(j)->m_Name.IsEmpty())
                                            guess.Add("%%tkInterfaceExplicit");
                                        else
                                            guess.Add(childL1->Item(j)->m_Name);
                                        guess.Add(tokFlname);
                                        found = true;
                                        break;
                                    }
                                    else
                                    {
                                        int lds = abs(int(childL2->Item(k)->m_LineStart) - int(tokFl->m_LineStart));
                                        if ((foundGuess && lineDifStart > lds) || !foundGuess)
                                        {
                                            guess.Clear();
                                            guess.Add(fileChildren->Item(i)->m_Name);
                                            guess.Add(childL1->Item(j)->m_Name);
                                            guess.Add(tokFlname);
                                            lineDifStart = lds;
                                            foundGuess = true;
                                        }
                                    }
                                }
                            }
                            if (found)
                                break;
                        }
                    }
                    if (found)
                        break;
                }
            }
        }
        for (size_t i=0; i<guess.GetCount(); i++)
        {
            address.Add(guess.Item(i));
        }
    }
}

void ParserF::FindTokensForUse(const wxString& search, wxArrayString& firstWords, TokensArrayFlat& result, bool onlyPublicNames)
{
    int woCount = firstWords.GetCount();
    if (woCount < 2 || !firstWords.Item(woCount-1).IsSameAs("use"))
        return;

    bool hasColon2 = false;
    int idx;
    bool firstC = false;

    for (size_t i=0; i<firstWords.GetCount()-1; i++)
    {
        if (firstWords.Item(i).IsSameAs(":"))
        {
            if (firstC)
            {
                hasColon2 = true;
                idx = i - 2;
                break;
            }
            else
            {
                firstC = true;
            }
        }
        else if (firstC)
        {
            firstC = false;
        }
    }

    wxString modName;
    if (hasColon2 && idx >= 0)
    {
        modName = firstWords.Item(idx);
    }
    else if (!hasColon2)
    {
        modName = firstWords.Item(woCount-2);
    }
    else
    {
        return;
    }

    int tokenKindMask = tkSubroutine | tkFunction | tkInterface | tkOther | tkVariable | tkType;
    int noChildrenOf = tokenKindMask;
    TokensArrayFlat* useWithRenameTok = NULL;
    m_RecursiveDeep = 0;
    m_UseRenameArrays = false;
    m_RenameDeep = 0;
    m_IncludeDeep = 0;

    ArrOfSizeT* resChildrenIdx = NULL;
    BoolArray2D* resCanBeSeen2D = NULL;
    FindMatchTokensInModuleAndUse2(modName, search, resChildrenIdx, resCanBeSeen2D, tokenKindMask, noChildrenOf, true,
                                  onlyPublicNames, true, useWithRenameTok);

    if (resChildrenIdx && resCanBeSeen2D)
    {
        for (size_t ia=0; ia<resChildrenIdx->GetCount(); ia++)
        {
            TokensArrayFlat* pasTokens = m_PassedTokensVisited[resChildrenIdx->Item(ia)];
            BoolArray1D* canSee = (*resCanBeSeen2D)[ia];
            for (size_t j=0; j<canSee->size(); j++)
            {
                if ((*canSee)[j])
                {
                    AddUniqueResult(result, pasTokens->Item(j));
                }
            }
        }
    }

    m_VisitedModules.Clear();
    ClearPassedTokensArray2D(m_PassedTokensVisited);
    ClearArrOfSizeT2D(m_ChildrenIdxVisited);
    ClearBoolArray3D(m_CanBeSeenVisited);
}


void ParserF::AddUniqueResult(TokensArrayFlat& result, const TokenF* token, bool isHostAssociated)
{
    bool have = false;
    for (size_t i=0; i<result.GetCount(); i++)
    {
        if (result.Item(i)->m_LineStart == token->m_LineStart &&
            result.Item(i)->m_DisplayName.IsSameAs(token->m_DisplayName) &&
            result.Item(i)->m_Filename.IsSameAs(token->m_Filename))
        {
            have = true;
            break;
        }
    }

    if (!have)
    {
        result.Add(new TokenFlat(token));
        result.Item(result.size()-1)->m_HostAssociated = isHostAssociated;
    }
}

void ParserF::AddUniqueResult(TokensArrayFlat& result, const TokenFlat* token)
{
    bool have = false;
    for (size_t i=0; i<result.GetCount(); i++)
    {
        if (result.Item(i)->m_LineStart == token->m_LineStart &&
            result.Item(i)->m_DisplayName.IsSameAs(token->m_DisplayName) &&
            result.Item(i)->m_Filename.IsSameAs(token->m_Filename))
        {
            have = true;
            break;
        }
    }
    if (!have)
    {
        result.Add(new TokenFlat(token));
    }
}


void ParserF::FindUseAssociatedTokens2(TokenF* useToken, const wxString &searchLw, ArrOfSizeT &resChildrenIdx, BoolArray2D &resCanBeSeen2D, int tokenKindMask, bool partialMatch,
                                      bool changeDisplayName, bool onlyPublicNames, TokensArrayFlat &renamedTokens, TokensArrayFlat* useWithRenameTok)
{
    if (m_RecursiveDeep > 20)
        return;  // deep limit was reached

    if (!useToken)
        return;
    if (useToken->m_TokenKind != tkUse)
        return;

    int noChildrenOf = tkInterface | tkFunction | tkSubroutine | tkType;
    UseTokenF* uTok = static_cast<UseTokenF*>(useToken);
//    if (uTok->GetModuleNature() == mnIntrinsic)
//        return;

    m_RecursiveDeep++;

    ArrOfSizeT* childrenIdx = NULL;
    BoolArray2D* canBeSeen2D = NULL;
    int midx;
    if (!m_UseRenameArrays)
        midx = m_VisitedModules.Index(useToken->m_Name);
    else
        midx = m_VisitedModulesRen.Index(useToken->m_Name);
    if (midx != wxNOT_FOUND)
    {
        if (!m_UseRenameArrays)
        {
            childrenIdx = m_ChildrenIdxVisited[midx];
            canBeSeen2D = m_CanBeSeenVisited[midx];
        }
        else
        {
            childrenIdx = m_ChildrenIdxVisitedRen[midx];
            canBeSeen2D = m_CanBeSeenVisitedRen[midx];
        }
    }
    else
    {
        FindMatchTokensInModuleAndUse2(uTok->m_Name, searchLw, childrenIdx, canBeSeen2D, tokenKindMask, noChildrenOf, partialMatch,
                                       onlyPublicNames, changeDisplayName, useWithRenameTok);
    }
    if (!childrenIdx || !canBeSeen2D)
    {
        m_RecursiveDeep--;
        return;
    }

    std::list<wxArrayString> *renameList = uTok->GetRenameList();
    std::set<wxString> *namesList = uTok->GetNamesList();
    if (uTok->HasOnly())
    {
        //with ONLY: keyword
        if (!namesList->empty())
        {
            // has names without rename
            for (size_t i=0; i<childrenIdx->GetCount(); i++)
            {
                TokensArrayFlat* pT;
                if (!m_UseRenameArrays)
                    pT = m_PassedTokensVisited[childrenIdx->Item(i)];
                else
                    pT = m_PassedTokensVisitedRen[childrenIdx->Item(i)];
                BoolArray1D* canSee = (*canBeSeen2D)[i];
                bool has = false;
                BoolArray1D* canSeeTmp = NULL;
                for (size_t j=0; j<pT->GetCount(); j++)
                {
                    //if ((*canSee)[j] && namesList->count(pT->Item(j)->m_Name) > 0)
                    if ((*canSee)[j] &&
                        ((pT->Item(j)->m_Rename.empty() && namesList->count(pT->Item(j)->m_Name) > 0) ||
                         (!pT->Item(j)->m_Rename.empty() && namesList->count(pT->Item(j)->m_Rename.Lower()) > 0)))
                    {
                        if (!has)
                        {
                            canSeeTmp = new BoolArray1D(canSee->size(),false);
                            has = true;
                        }
                        (*canSeeTmp)[j] = true;
                    }
                }
                if (has)
                {
                    resChildrenIdx.Add(childrenIdx->Item(i));
                    resCanBeSeen2D.push_back(canSeeTmp);
                }
            }
        }

        for(std::list<wxArrayString>::iterator pos=renameList->begin(); pos != renameList->end(); ++pos)
        {
            // through rename
            // pos->Item(0) -local name
            // pos->Item(1) -external name
            wxString locNamLw = pos->Item(0).Lower();
            if ( (partialMatch && locNamLw.StartsWith(searchLw)) ||
                 (!partialMatch && locNamLw.IsSameAs(searchLw)) )
            {
                wxString impNamLw = pos->Item(1).Lower();

                if ( (partialMatch && impNamLw.StartsWith(searchLw)) ||
                 (!partialMatch && impNamLw.IsSameAs(searchLw)) )
                {
                    for (size_t i=0; i<childrenIdx->GetCount(); i++)
                    {
                        TokensArrayFlat* pT;
                        if (!m_UseRenameArrays)
                            pT = m_PassedTokensVisited[childrenIdx->Item(i)];
                        else
                            pT = m_PassedTokensVisitedRen[childrenIdx->Item(i)];
                        BoolArray1D* canSee = (*canBeSeen2D)[i];
                        for (size_t j=0; j<pT->GetCount(); j++)
                        {
                            if ((*canSee)[j] && pT->Item(j)->m_Name.IsSameAs(impNamLw))
                            {
                                TokenFlat* tf = new TokenFlat(pT->Item(j));
                                if (changeDisplayName)
                                {
                                    tf->Rename(pos->Item(0));
                                }
                                tf->m_Rename << pos->Item(0);
                                renamedTokens.Add(tf);

                                if (useWithRenameTok)
                                {
                                    TokenFlat* tfu = new TokenFlat(useToken);
                                    tfu->m_Rename = pos->Item(0) + " => " + pos->Item(1);
                                    useWithRenameTok->Add(tfu);
                                }
                            }
                        }
                    }
                }
                else if (m_RenameDeep == 0)
                {
                    ArrOfSizeT* renChIdx = NULL;
                    BoolArray2D* renCBS2D = NULL;
                    m_UseRenameArrays = true;
                    m_RenameDeep++;
                    FindMatchTokensInModuleAndUse2(uTok->m_Name, impNamLw, renChIdx, renCBS2D, tokenKindMask, noChildrenOf, false,
                                                  onlyPublicNames, changeDisplayName, useWithRenameTok);
                    m_UseRenameArrays = false;
                    m_RenameDeep--;
                    if (renChIdx && renCBS2D)
                    {
                        bool have = false;
                        for (size_t ia=0; ia<renChIdx->GetCount(); ia++)
                        {
                            TokensArrayFlat* pasTokens = m_PassedTokensVisitedRen[renChIdx->Item(ia)];
                            BoolArray1D* canSee = (*renCBS2D)[ia];
                            for (size_t j=0; j<canSee->size(); j++)
                            {
                                if ((*canSee)[j])
                                {
                                    TokenFlat* tf = new TokenFlat(pasTokens->Item(j));
                                    if (changeDisplayName)
                                    {
                                        tf->Rename(pos->Item(0));
                                    }
                                    tf->m_Rename << pos->Item(0);
                                    renamedTokens.Add(tf);
                                    if (!have)
                                        have = true;
                                }
                            }
                        }
                        if (have && useWithRenameTok)
                        {
                            TokenFlat* tfu = new TokenFlat(useToken);
                            tfu->m_Rename = pos->Item(0) + " => " + pos->Item(1);
                            useWithRenameTok->Add(tfu);
                        }
                    }
                    if (m_RenameDeep == 0)
                    {
                        m_VisitedModulesRen.Clear();
                        ClearPassedTokensArray2D(m_PassedTokensVisitedRen);
                        ClearArrOfSizeT2D(m_ChildrenIdxVisitedRen);
                        ClearBoolArray3D(m_CanBeSeenVisitedRen);
                    }
                }
            }
        }
    }
    else if (!renameList->empty())
    {
        //no ONLY keyword. Has rename list.
        size_t oldCount = resChildrenIdx.GetCount();
        for (size_t i=0; i<childrenIdx->GetCount(); i++)
        {
            resChildrenIdx.Add(childrenIdx->Item(i));
            BoolArray1D* canSee = (*canBeSeen2D)[i];
            BoolArray1D* canSeeTmp = new BoolArray1D(*canSee);
            resCanBeSeen2D.push_back(canSeeTmp);
        }

        for (std::list<wxArrayString>::iterator pos=renameList->begin(); pos != renameList->end(); ++pos)
        {
            if (pos->Item(0).empty() || pos->Item(1).IsEmpty())
                continue; //some mistake

            wxString locNamLw = pos->Item(0).Lower();
            if ( (partialMatch && locNamLw.StartsWith(searchLw)) ||
                 (!partialMatch && locNamLw.IsSameAs(searchLw)) )
            {
                wxString impNamLw = pos->Item(1).Lower();

                if ( (partialMatch && impNamLw.StartsWith(searchLw)) ||
                 (!partialMatch && impNamLw.IsSameAs(searchLw)) )
                {
                    for (size_t i=oldCount; i<resChildrenIdx.GetCount(); i++)
                    {
                        TokensArrayFlat* pT;
                        if (!m_UseRenameArrays)
                            pT = m_PassedTokensVisited[resChildrenIdx.Item(i)];
                        else
                            pT = m_PassedTokensVisitedRen[resChildrenIdx.Item(i)];
                        BoolArray1D* canSeeTmp = resCanBeSeen2D[i];
                        for (size_t j=0; j<pT->GetCount(); j++)
                        {
                            if ((*canSeeTmp)[j] && pT->Item(j)->m_Name.IsSameAs(impNamLw))
                            {
                                TokenFlat* tf = new TokenFlat(pT->Item(j));
                                if (changeDisplayName)
                                {
                                    tf->Rename(pos->Item(0));
                                }
                                tf->m_Rename << pos->Item(0);
                                renamedTokens.Add(tf);

                                if (useWithRenameTok)
                                {
                                    TokenFlat* tfu = new TokenFlat(useToken);
                                    tfu->m_Rename = pos->Item(0) + " => " + pos->Item(1);
                                    useWithRenameTok->Add(tfu);
                                }
                                (*canSeeTmp)[j] = false;
                            }
                        }
                    }
                }
                else if (m_RenameDeep == 0)
                {
                    ArrOfSizeT* renChIdx = NULL;
                    BoolArray2D* renCBS2D = NULL;
                    m_UseRenameArrays = true;
                    FindMatchTokensInModuleAndUse2(uTok->m_Name, impNamLw, renChIdx, renCBS2D, tokenKindMask, noChildrenOf, false,
                                                  onlyPublicNames, changeDisplayName, useWithRenameTok);
                    m_UseRenameArrays = false;
                    if (renChIdx && renCBS2D)
                    {
                        bool have = false;
                        for (size_t ia=0; ia<renChIdx->GetCount(); ia++)
                        {
                            TokensArrayFlat* pasTokens = m_PassedTokensVisitedRen[renChIdx->Item(ia)];
                            BoolArray1D* canSee = (*renCBS2D)[ia];
                            for (size_t j=0; j<canSee->size(); j++)
                            {
                                if ((*canSee)[j])
                                {
                                    TokenFlat* tf = new TokenFlat(pasTokens->Item(j));
                                    if (changeDisplayName)
                                    {
                                        tf->Rename(pos->Item(0));
                                    }
                                    tf->m_Rename << pos->Item(0);
                                    renamedTokens.Add(tf);
                                    if (!have)
                                        have = true;
                                }
                            }
                        }
                        if (have && useWithRenameTok)
                        {
                            TokenFlat* tfu = new TokenFlat(useToken);
                            tfu->m_Rename = pos->Item(0) + " => " + pos->Item(1);
                            useWithRenameTok->Add(tfu);
                        }
                    }
                    if (m_RenameDeep == 0)
                    {
                        m_VisitedModulesRen.Clear();
                        ClearPassedTokensArray2D(m_PassedTokensVisitedRen);
                        ClearArrOfSizeT2D(m_ChildrenIdxVisitedRen);
                        ClearBoolArray3D(m_CanBeSeenVisitedRen);
                    }
                }
            }
            else
            {
                wxString impNamLw = pos->Item(1).Lower();
                if ( (partialMatch && impNamLw.StartsWith(searchLw)) ||
                     (!partialMatch && impNamLw.IsSameAs(searchLw)) )
                {
                    for (size_t i=oldCount; i<resChildrenIdx.GetCount(); i++)
                    {
                        TokensArrayFlat* pT;
                        if (!m_UseRenameArrays)
                            pT = m_PassedTokensVisited[resChildrenIdx.Item(i)];
                        else
                            pT = m_PassedTokensVisitedRen[resChildrenIdx.Item(i)];
                        BoolArray1D* canSeeTmp = resCanBeSeen2D[i];
                        for (size_t j=0; j<pT->GetCount(); j++)
                        {
                            if ((*canSeeTmp)[j] && pT->Item(j)->m_Name.IsSameAs(impNamLw))
                            {
                                (*canSeeTmp)[j] = false;
                            }
                        }
                    }
                }
            }
        }

    }
    else // no ONLY or rename list
    {
        for (size_t i=0; i<childrenIdx->GetCount(); i++)
        {
            resChildrenIdx.Add(childrenIdx->Item(i));
            BoolArray1D* canSee = (*canBeSeen2D)[i];
            BoolArray1D* canSeeTmp = new BoolArray1D(*canSee);
            resCanBeSeen2D.push_back(canSeeTmp);
        }
    }
    m_RecursiveDeep--;
}


void ParserF::FindMatchTokensInModuleAndUse2(const wxString& modName, const wxString& searchLw, ArrOfSizeT* &childrenIdx, BoolArray2D* &canBeSeen2D, int tokenKindMask,
                                             int noChildrenOf, bool partialMatch, bool onlyPublicNames, bool changeDisplayName, TokensArrayFlat* useWithRenameTok)
{
    TokenF* modTok = FindModuleSubmoduleToken(modName);

    if (!modTok || modTok->m_TokenKind != tkModule)
        return;
    ModuleTokenF* mToken = static_cast<ModuleTokenF*>(modTok);
    TokensArrayF* children = &modTok->m_Children;
    if (!children)
        return;

    TokensArrayF useTokens;

    std::vector<TokensArrayF*> vpChildren;
    vpChildren.push_back(children);
    int numInclude = 0;
    for (size_t ichil=0; ichil<vpChildren.size(); ++ichil)
    {
        TokensArrayF* pParChildren = vpChildren[ichil];
        size_t npch = pParChildren->GetCount();

        for (size_t i=0; i<npch; ++i)
        {
            TokenF* chtok = pParChildren->Item(i);
            if (chtok->m_TokenKind == tkUse)
            {
                useTokens.Add(chtok);
            }
            else if (chtok->m_TokenKind == tkInclude)
            {
                TokensArrayF* includedTokens = new TokensArrayF();
                AddIncludeFileChildren(chtok, *includedTokens);
                vpChildren.push_back(includedTokens);
                numInclude++;
            }
            else if (chtok->m_TokenKind == tkSubroutine || chtok->m_TokenKind == tkFunction)
            {
                break; // 'use' statments must be located above procedures
            }
        }
    }
    if (numInclude > 0)
    {
        size_t origSize = vpChildren.size() - numInclude;
        for (size_t ichil=origSize; ichil<vpChildren.size(); ichil++)
        {
            delete vpChildren[ichil];
        }
    }

    childrenIdx = new ArrOfSizeT; // indexes of associated modules and this module
    canBeSeen2D = new BoolArray2D;

    TokensArrayFlat* passedTokens = new TokensArrayFlat;
    FindMatchChildrenDeclared(*children, searchLw, *passedTokens, tokenKindMask, partialMatch, noChildrenOf, onlyPublicNames);

    BoolArray1D* canSeeLocal = new BoolArray1D;

    if (onlyPublicNames)
    {
        wxString nameT;
        for (size_t i=0; i<useTokens.Count(); i++)
        {
            ArrOfSizeT childrenIdxTmp;
            BoolArray2D canBeSeen2DTmp;
            TokensArrayFlat renamedTokensTmp;
            FindUseAssociatedTokens2(useTokens.Item(i), searchLw, childrenIdxTmp, canBeSeen2DTmp, tokenKindMask, partialMatch,
                                      changeDisplayName, onlyPublicNames, renamedTokensTmp, useWithRenameTok);
            bool defPub = (useTokens.Item(i)->m_TokenAccess == taPublic);
            size_t nChIdx = childrenIdxTmp.GetCount();
            for (size_t j=0; j<nChIdx; ++j)
            {
                size_t chIdx = childrenIdxTmp.Item(j);
                TokensArrayFlat* passTokTmp;
                if (!m_UseRenameArrays)
                    passTokTmp = m_PassedTokensVisited[chIdx];
                else
                    passTokTmp = m_PassedTokensVisitedRen[chIdx];
                BoolArray1D* canSeeTmp = canBeSeen2DTmp[j];
                int ind = childrenIdx->Index(chIdx);

                if (ind == wxNOT_FOUND)
                {
                    bool hasPub = false;
                    size_t cSs = canSeeTmp->size();
                    for (size_t k=0; k<cSs; k++)
                    {
                        if ((*canSeeTmp)[k])
                        {
                            TokenFlat* passTokIt = passTokTmp->Item(k);
                            if (!changeDisplayName)
                            {
                                if (passTokIt->m_Rename.IsEmpty())
                                    nameT = passTokIt->m_Name;
                                else
                                    nameT = passTokIt->m_Rename.Lower();
                            }

                            if ( (changeDisplayName && ((defPub && !mToken->HasNameInPrivateList(passTokIt->m_Name)) ||
                                                        (!defPub && mToken->HasNameInPublicList(passTokIt->m_Name)))) ||
                                 (!changeDisplayName && ((defPub && !mToken->HasNameInPrivateList(nameT)) ||
                                                        (!defPub && mToken->HasNameInPublicList(nameT)))) )
                            {
                                hasPub = true;
                            }
                            else
                            {
                                (*canSeeTmp)[k] = false;
                            }
                        }
                    }
                    if (hasPub)
                    {
                        childrenIdx->Add(chIdx);
                        canBeSeen2D->push_back(canSeeTmp);
                    }
                }
                else
                {
                    BoolArray1D* canSee = (*canBeSeen2D)[ind];
                    for (size_t k=0; k<canSeeTmp->size(); k++)
                    {
                        if (!(*canSee)[k] && (*canSeeTmp)[k])
                        {
                            TokenFlat* passTokIt = passTokTmp->Item(k);
                            if (!changeDisplayName)
                            {
                                if (passTokIt->m_Rename.IsEmpty())
                                    nameT = passTokIt->m_Name;
                                else
                                    nameT = passTokIt->m_Rename.Lower();
                            }
                            if ( (changeDisplayName && ((defPub && !mToken->HasNameInPrivateList(passTokIt->m_Name)) ||
                                                        (!defPub && mToken->HasNameInPublicList(passTokIt->m_Name)))) ||
                                 (!changeDisplayName && ((defPub && !mToken->HasNameInPrivateList(nameT)) ||
                                                        (!defPub && mToken->HasNameInPublicList(nameT)))) )
                            {
                                (*canSee)[k] = true;
                            }
                            else
                            {
                                //canSee->Item(k) = false;
                            }
                        }
                    }
                    delete canSeeTmp;
                }
            }

            for (size_t j=0; j<renamedTokensTmp.GetCount(); j++)
            {
                if (!changeDisplayName)
                {
                    if (renamedTokensTmp.Item(j)->m_Rename.IsEmpty())
                        nameT = renamedTokensTmp.Item(j)->m_Name;
                    else
                        nameT = renamedTokensTmp.Item(j)->m_Rename.Lower();
                }

                if ( (changeDisplayName && ((defPub && !mToken->HasNameInPrivateList(renamedTokensTmp.Item(j)->m_Name)) ||
                                            (!defPub && mToken->HasNameInPublicList(renamedTokensTmp.Item(j)->m_Name)))) ||
                     (!changeDisplayName && ((defPub && !mToken->HasNameInPrivateList(nameT)) ||
                                            (!defPub && mToken->HasNameInPublicList(nameT)))) )
                {
                    passedTokens->Add(renamedTokensTmp.Item(j));
                }
                else
                {
                    renamedTokensTmp.Item(j)->Clear();
                    delete renamedTokensTmp.Item(j);
                }
            }
        }
    }
    else // !onlyPublicNames
    {
        for (size_t i=0; i<useTokens.Count(); i++)
        {
            ArrOfSizeT childrenIdxTmp;
            BoolArray2D canBeSeen2DTmp;
            TokensArrayFlat renamedTokensTmp;
            FindUseAssociatedTokens2(useTokens.Item(i), searchLw, childrenIdxTmp, canBeSeen2DTmp, tokenKindMask, partialMatch,
                                      changeDisplayName, onlyPublicNames, renamedTokensTmp, useWithRenameTok);

            for (size_t j=0; j<childrenIdxTmp.GetCount(); j++)
            {
                BoolArray1D* canSeeTmp = canBeSeen2DTmp[j];
                int ind = childrenIdx->Index(childrenIdxTmp.Item(j));
                if (ind == wxNOT_FOUND)
                {
                    childrenIdx->Add(childrenIdxTmp.Item(j));
                    canBeSeen2D->push_back(canSeeTmp);
                }
                else
                {
                    BoolArray1D* canSee = (*canBeSeen2D)[ind];
                    for (size_t k=0; k<canSeeTmp->size(); k++)
                    {
                        if (!(*canSee)[k] && (*canSeeTmp)[k])
                            (*canSee)[k] = true;
                    }
                    delete canSeeTmp;
                }
            }

            for (size_t j=0; j<renamedTokensTmp.GetCount(); j++)
            {
                passedTokens->Add(renamedTokensTmp.Item(j));
            }
        }
    }

    canSeeLocal->resize(passedTokens->GetCount(),true);
    canBeSeen2D->push_back(canSeeLocal);
    if (!m_UseRenameArrays)
    {
        m_CanBeSeenVisited.push_back(canBeSeen2D);
        m_VisitedModules.Add(modName);
        m_PassedTokensVisited.push_back(passedTokens);
        childrenIdx->Add(m_VisitedModules.GetCount()-1);
        m_ChildrenIdxVisited.push_back(childrenIdx);
    }
    else
    {
        m_CanBeSeenVisitedRen.push_back(canBeSeen2D);
        m_VisitedModulesRen.Add(modName);
        m_PassedTokensVisitedRen.push_back(passedTokens);
        childrenIdx->Add(m_VisitedModulesRen.GetCount()-1);
        m_ChildrenIdxVisitedRen.push_back(childrenIdx);
    }
}


void ParserF::ChangeAssociatedName(wxString& line, TokenFlat* token)
{
    if (!token)
        return;
    if ((token->m_TokenKind != tkAssociateConstruct) && (token->m_TokenKind != tkSelectTypeDefault))
        return;

    wxString args = token->m_Args.Lower();
    std::map<wxString,wxString> assocMap;
    ParserThreadF::SplitAssociateConstruct(args, assocMap);

    //change names in the line
    wxString lineLw = line.Lower();
    line.Empty();
    const wxString delim = " ()[]{}&;,*./+-><=%\t\r\n";
    size_t idx1 = 0;
    wxString block;
    bool wasDeli = false;
    for(size_t i=0; i<lineLw.Len(); i++)
    {
        if (!wasDeli && delim.Find(lineLw.GetChar(i)) != wxNOT_FOUND)
        {
            block = lineLw.Mid(idx1,i-idx1);
            wasDeli = true;
            idx1 = i;
            std::map<wxString,wxString>::iterator it = assocMap.find(block);
            if (it != assocMap.end())
                line << it->second;
            else
                line << block;
        }
        else if (wasDeli && delim.Find(lineLw.GetChar(i)) == wxNOT_FOUND)
        {
            line << lineLw.Mid(idx1,i-idx1);
            wasDeli = false;
            idx1 = i;
        }
    }
    line << lineLw.Mid(idx1);
}


void ParserF::AddIncludeFileChildren(const TokenF* include, TokensArrayF& tokens)
{
    if (include->m_TokenKind != tkInclude)
        return;

    bool withExt = (include->m_Name.Find('.',true) != wxNOT_FOUND);
    #ifdef __WXMSW__
        wxString iname = include->m_Name;
    #else
        wxString iname = include->m_DisplayName;
    #endif

    for (size_t i=0; i<m_pTokens->GetCount(); i++)
    {
        if (m_pTokens->Item(i)->m_TokenKind != tkFile)
            continue;

        #ifdef __WXMSW__
            wxString ffname = m_pTokens->Item(i)->m_Filename.AfterLast('\\');
            ffname.MakeLower();
        #else
            wxString ffname = m_pTokens->Item(i)->m_Filename.AfterLast('/');
        #endif

        if ( (withExt && ffname.IsSameAs(iname)) ||
             (!withExt && ffname.BeforeFirst('.').IsSameAs(iname)) )
        {
            tokens.Alloc(tokens.GetCount() + m_pTokens->Item(i)->m_Children.GetCount());
            for (size_t j=0; j<m_pTokens->Item(i)->m_Children.GetCount(); j++)
            {
                tokens.Add(m_pTokens->Item(i)->m_Children.Item(j));
            }
            break;
        }
    }
}

bool ParserF::IsIncludeFile(wxString fileName)
{
    bool isInclude = false;
    wxChar sep = wxFileName::GetPathSeparator();
    if (m_pIncludeDB->IsIncludeFile(fileName.AfterLast(sep)))
        isInclude = true;
    return isInclude;
}

bool ParserF::HasIncludeFiles()
{
    return !m_pIncludeDB->IsEmpty();
}

void ParserF::GetSubmoduleHostTokens(TokenF* subModToken, std::vector<TokensArrayF*> &vpChildren)
{
    m_SubmodDeep++;
    if (!subModToken || subModToken->m_TokenKind != tkSubmodule)
        return;
    if (m_SubmodDeep > 10)  // the limit of recursive call. Should be more than enough.
        return;

    SubmoduleTokenF* submod = static_cast<SubmoduleTokenF*>(subModToken);
    wxString parentName = submod->m_AncestorModuleName;
    if (!submod->m_ParentSubmoduleName.IsEmpty())
        parentName << ":" << submod->m_ParentSubmoduleName;

    TokenF* modTok = FindModuleSubmoduleToken(parentName);
    if (!modTok)
        return;

    if (modTok->m_Children.Count() > 0)
        vpChildren.push_back(&modTok->m_Children);

    if (modTok->m_TokenKind == tkSubmodule)
        GetSubmoduleHostTokens(modTok, vpChildren);
}

void ParserF::SetNewTokens(TokensArrayF* pTokens)
{
    if (m_pTokensNew)
    {
        ClearTokens(m_pTokensNew);
        delete m_pTokensNew;
    }
    m_pTokensNew = pTokens;
}

void ParserF::SetNewIncludeDB(IncludeDB* pIncludeDB)
{
    if (m_pIncludeDBNew)
    {
        m_pIncludeDBNew->Clear();
        delete m_pIncludeDBNew;
    }
    m_pIncludeDBNew = pIncludeDB;
}

void ParserF::SetNewADirTokens(TokensArrayF* pTokens)
{
    if (m_pTokensADirNew)
    {
        ClearTokens(m_pTokensADirNew);
        delete m_pTokensADirNew;
    }
    m_pTokensADirNew = pTokens;
}

void ParserF::SetNewADirIncludeDB(IncludeDB* pIncludeDB)
{
    if (m_pIncludeDBADirNew)
    {
        m_pIncludeDBADirNew->Clear();
        delete m_pIncludeDBADirNew;
    }
    m_pIncludeDBADirNew = pIncludeDB;
}

void ParserF::ClearTokens(TokensArrayF* pTokens)
{
    if (!pTokens)
        return;

    for (size_t i=0; i<pTokens->GetCount(); i++)
    {
        pTokens->Item(i)->Clear();
        delete pTokens->Item(i);
    }
    pTokens->Clear();
}

void ParserF::ConnectToNewTokens()
{
    wxCriticalSectionLocker cslocker(s_CritSect);
    wxMutexLocker mlocker(s_NewTokensMutex);
    if (m_pBufferTokens)
    {
        ClearTokens(m_pBufferTokens);
    }
    if (m_pTokens)
    {
        ClearTokens(m_pTokens);
        delete m_pTokens;
    }
    m_pTokens = m_pTokensNew;
    m_pTokensNew = NULL;
    if (m_pIncludeDB)
    {
        m_pIncludeDB->Clear();
        delete m_pIncludeDB;
    }
    m_pIncludeDB = m_pIncludeDBNew;
    m_pIncludeDBNew = NULL;
}

void ParserF::ConnectToNewADirTokens()
{
    wxCriticalSectionLocker cslocker(s_CritSect);
    wxMutexLocker mlocker(s_AdditionalDirNewTokensMutex);
    if (m_pAdditionalDirTokens)
    {
        ClearTokens(m_pAdditionalDirTokens);
        delete m_pAdditionalDirTokens;
    }
    m_pAdditionalDirTokens = m_pTokensADirNew;
    m_pTokensADirNew = NULL;

    if (m_pIncludeDBADir)
    {
        m_pIncludeDBADir->Clear();
        delete m_pIncludeDBADir;
    }
    m_pIncludeDBADir = m_pIncludeDBADirNew;
    m_pIncludeDBADirNew = NULL;
}

void ParserF::SetNewCurrentTokens(TokensArrayF* pTokens)
{
    // function called from secondary thread (bufferparserthread)
    wxCriticalSectionLocker locker(s_CurrentBTokensCritSect);
    if (m_pCurrentBufferTokensNew)
    {
        ClearTokens(m_pCurrentBufferTokensNew);
        delete m_pCurrentBufferTokensNew;
    }
    m_pCurrentBufferTokensNew = pTokens;
}

void ParserF::ConnectToNewCurrentTokens()
{
    wxCriticalSectionLocker locker(s_CurrentBTokensCritSect);
    if (m_pBufferTokens && m_pCurrentBufferTokensNew && m_pCurrentBufferTokensNew->size() > 0)
    {
        for (size_t i=0; i<m_pBufferTokens->size(); i++)
        {
            if (m_pBufferTokens->Item(i)->m_Filename.IsSameAs(m_pCurrentBufferTokensNew->Item(0)->m_Filename))
            {
                m_pBufferTokens->Item(i)->Clear();
                delete m_pBufferTokens->Item(i);
                m_pBufferTokens->RemoveAt(i);
                break;
            }
        }
        m_pBufferTokens->Add(m_pCurrentBufferTokensNew->Item(0));
    }
    if (m_pCurrentBufferTokensNew)
        delete m_pCurrentBufferTokensNew;
    m_pCurrentBufferTokensNew = NULL;
}

void ParserF::ParseIntrinsicModules()
{
    if (!m_pIntrinsicModuleTokens)
        return;
    int dispCase = 0;
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");
    if (cfg)
        dispCase = cfg->ReadInt("/keywords_case", 0);

    wxString filename = ConfigManager::GetDataFolder() + "/images/fortranproject/fortran_intrinsic_modules.f90";
    if (!wxFileExists(filename))
    {
        Manager::Get()->GetLogManager()->Log("FortranProject plugin error: file " + filename + " was not found.");
        return;
    }
    wxString fn = UnixFilename(filename);
    ParserThreadF* thread = new ParserThreadF(fn, fn, m_pIntrinsicModuleTokens, fsfFree);
    thread->Parse();
    delete thread;

    ChangeCaseChildren(m_pIntrinsicModuleTokens->Item(0)->m_Children, dispCase);
}

void ParserF::ChangeCaseChildren(TokensArrayF &children, int dispCase)
{
    for (size_t i=0; i<children.GetCount(); i++)
    {
        wxString* dn = &children.Item(i)->m_DisplayName;
        switch (dispCase)
        {
            case 0:
            {
                break;
            }
            case 1:
            {
                *dn = dn->MakeUpper();
                break;
            }
            case 2:
            {
                *dn = dn->Mid(0,1).MakeUpper() + dn->Mid(1).MakeLower();
                break;
            }
            default :
            {
                *dn = dn->MakeLower();
                break;
            }
        }
        if (children.Item(i)->m_Children.GetCount() > 0)
        {
            ChangeCaseChildren(children.Item(i)->m_Children, dispCase);
        }
    }
}


TokenF* ParserF::FindToken(const TokenFlat &token, TokensArrayF* children)
{
    if (!children)
        children = FindFileTokens(token.m_Filename);

    if (!children)
        return NULL;

    TokenF* pFoundToken = NULL;

    size_t childrenCount = children->GetCount();
    for (size_t i=0; i<childrenCount; i++)
    {
		if ( (!token.m_WasIncluded && children->Item(i)->m_LineStart == token.m_LineStart &&
              children->Item(i)->m_Name == token.m_Name) ||
             (token.m_WasIncluded && children->Item(i)->m_IncludeLineStart == token.m_IncludeLineStart &&
              children->Item(i)->m_Name == token.m_Name) )
        {
            pFoundToken = children->Item(i);
            break;
        }
        else if ( children->Item(i)->m_LineStart <= token.m_LineStart &&
                  children->Item(i)->m_LineEnd >= token.m_LineStart &&
                  children->Item(i)->m_Children.GetCount() > 0)
        {
            pFoundToken = FindToken(token, &children->Item(i)->m_Children);
            if (pFoundToken)
                break;
        }
        else if ( (!token.m_WasIncluded && children->Item(i)->m_LineStart > token.m_LineStart) ||
                  (token.m_WasIncluded && children->Item(i)->m_IncludeLineStart > token.m_IncludeLineStart) )
            break;
    }
    return pFoundToken;
}

void ParserF::ChangeLineIfRequired(cbEditor* ed, wxString& curLine)
{
    int lineStart = -1;
    TokenFlat* tokFl = NULL;
    FindLineScopeLN(ed, lineStart, tokFl, -1);
    if (tokFl)
    {
        if (tokFl->m_TokenKind == tkAssociateConstruct || tokFl->m_TokenKind == tkSelectTypeDefault)
        {
            ChangeAssociatedName(curLine, tokFl);
        }
        delete tokFl;
    }
}

void ParserF::GetAddressOfToken(TokenF* token, wxArrayString& address)
{
    if (token->m_TokenKind != tkFile && token->m_pParent)
        GetAddressOfToken(token->m_pParent, address);

    if (token->m_TokenKind == tkFile)
        address.Add(token->m_Filename);
    else
        address.Add(token->m_Name);
}

TokenF* ParserF::FindTokenBetweenChildren(TokenF* pToken, const wxString& name)
{
    TokensArrayF* pChildren = &pToken->m_Children;

    wxString nameLw = name.Lower();
    for (size_t i=0; i<pChildren->GetCount(); i++)
    {
        if (pChildren->Item(i)->m_Name.IsSameAs(nameLw))
            return pChildren->Item(i);
    }
    return NULL;
}

void ParserF::FindImplementedProcInMySubmodules(TokenFlat* tok, const wxString& search, TokensArrayFlat& result)
{
    wxArrayString address; // [file_name, module_name, function_name, etc.]
    FindAddress(tok, address);
    if (address.Count() < 3)
        return; // not in an interface

    FindImplementedProcInMySubmodules(address, search, result);
}

void ParserF::FindImplementedProcInMySubmodules(cbEditor* ed, const wxString& search, TokensArrayFlat& result)
{
    wxArrayString address; // [file_name, module_name, function_name, etc.]
    FindAddress(ed, address);
    if (address.Count() < 3)
        return; // not in an interface

    FindImplementedProcInMySubmodules(address, search, result);
}

void ParserF::FindImplementedProcInMySubmodules(wxArrayString& address, const wxString& search, TokensArrayFlat& result)
{
    wxString searchLw = search.Lower();
    wxCriticalSectionLocker locker(s_CritSect);
    TokensArrayF* fileChildren = FindFileTokens(address.Item(0));
    if (!fileChildren)
        return;

    std::vector<TokensArrayF*> vpChildren;
    TokenF* subModToken = NULL;
    TokensArrayF* subModTokenCh = NULL;

    for (size_t i=0; i<fileChildren->GetCount(); i++)
    {
        if ((fileChildren->Item(i)->m_TokenKind == tkModule || fileChildren->Item(i)->m_TokenKind == tkSubmodule) &&
             fileChildren->Item(i)->m_Name.IsSameAs(address.Item(1)))
        {
            subModToken = fileChildren->Item(i);
            subModTokenCh = &fileChildren->Item(i)->m_Children;
            break;
        }
    }
    if (!subModTokenCh)
        return;

    bool inInterface = false;
    bool isInterfaceExp = address.Item(2).IsSameAs("%%tkInterfaceExplicit");
    for (size_t i=0; i<subModTokenCh->GetCount(); i++)
    {
        if ((subModTokenCh->Item(i)->m_TokenKind == tkInterface || subModTokenCh->Item(i)->m_TokenKind == tkInterfaceExplicit) &&
            (subModTokenCh->Item(i)->m_Name.IsSameAs(address.Item(2)) || isInterfaceExp))
        {
            inInterface = true;
            break;
        }
    }
    if (!inInterface)
        return;

    wxString modName;
    if (subModToken->m_TokenKind == tkModule)
        modName = subModToken->m_Name;
    else
    {
        SubmoduleTokenF* submod = static_cast<SubmoduleTokenF*>(subModToken);
        modName = submod->m_AncestorModuleName;
    }

    TokensArrayF* submodTokens = new TokensArrayF();
    FindSubmodulesWhichExtends(modName, submodTokens);
    int mask = tkFunction | tkSubroutine | tkProcedure;

    for (size_t j=0; j<submodTokens->GetCount(); j++)
    {
        TokensArrayF* smCh = &submodTokens->Item(j)->m_Children;
        for (size_t i=0; i<smCh->GetCount(); i++)
        {
            if ((smCh->Item(i)->m_TokenKind & mask) &&
                smCh->Item(i)->m_Name.IsSameAs(searchLw))
            {
                result.Add(new TokenFlat(smCh->Item(i)));
            }
        }
    }
}

void ParserF::FindSubmodulesWhichExtends(const wxString& moduleName, TokensArrayF* result)
{
    for (size_t i=0; i<m_pTokens->GetCount(); i++)
    {
        if (m_pTokens->Item(i)->m_TokenKind == tkFile)
        {
            TokensArrayF* children = &m_pTokens->Item(i)->m_Children;
            for (size_t j=0; j<children->GetCount(); j++)
            {
                if (children->Item(j)->m_TokenKind == tkSubmodule)
                {
                    SubmoduleTokenF* submod = static_cast<SubmoduleTokenF*>(children->Item(j));
                    if (submod->m_AncestorModuleName.IsSameAs(moduleName))
                        result->Add(children->Item(j));
                }
            }
        }
    }
}

void ParserF::ChangeArgumentsTypeBoundProc(TokenFlat& tbProcTok, const TokenFlat& procTok)
{
    if (tbProcTok.m_Pass)
    {
        wxString pass_arg = tbProcTok.m_Args;
        wxString args = procTok.m_Args;
        int start = 0;
        int end = 0;
        if (!pass_arg.IsEmpty())
            GetPossitionOfDummyArgument(args, pass_arg, start, end);
        else
            GetCallTipHighlight(args, 0, start, end);
        if (end <= start)
        {
            tbProcTok.m_Args = args; // was not found?
            return;
        }

        wxString fpart = args.Mid(0,start);
        int compos = fpart.Find(',',true);
        if (compos != wxNOT_FOUND)
            fpart = fpart.Mid(0,compos+1);

        wxString spart = args.Mid(start);
        compos = spart.Find(',');
        if (compos != wxNOT_FOUND)
            spart = spart.Mid(compos+1).Trim(false);
        else
            spart = args.Mid(end).Trim(false);

        tbProcTok.m_Args = fpart.Append(spart);
    }
    else
    {
        tbProcTok.m_Args = procTok.m_Args;
    }
}

void ParserF::GetChildren(TokenFlat* token, int tokenKindMask, TokensArrayFlat& result, int levelMax)
{
    TokenF* pToken = NULL;
    TokensArrayF* pChildren = FindFileTokens(token->m_Filename);

    for (size_t i=0; i<8; i++)
    {
        bool newChildren = false;
        for (size_t j=0; j < pChildren->GetCount(); j++)
        {
            if (pChildren->Item(j)->m_LineStart == token->m_LineStart && pChildren->Item(j)->m_Name.IsSameAs(token->m_Name))
            {
                pToken = pChildren->Item(j);
                break;
            }
            else if (pChildren->Item(j)->m_LineStart <= token->m_LineStart && pChildren->Item(j)->m_LineEnd >= token->m_LineEnd)
            {
                pChildren = &pChildren->Item(j)->m_Children;
                newChildren = true;
                break;
            }
            else if (pChildren->Item(j)->m_LineStart > token->m_LineStart)
            {
                break;
            }
        }
        if (!newChildren)
            break;
    }

    if (!pToken)
        return;

    GetChildren(pToken, tokenKindMask, result, 1, levelMax);
}

void ParserF::GetChildren(TokenF* pToken, int tokenKindMask, TokensArrayFlat& result, int level, int levelMax)
{
    if (!pToken)
        return;

    TokensArrayF* pChildren = &pToken->m_Children;
    size_t nChild = pChildren->GetCount();
    for (size_t i=0; i<nChild; i++)
    {
        if (pChildren->Item(i)->m_TokenKind & tokenKindMask)
        {
            result.Add(new TokenFlat(pChildren->Item(i)));
        }
        else if (pChildren->Item(i)->m_TokenKind == tkAssociateConstruct)
        {
            GetChildrenAssociateConstruct(pChildren->Item(i), tokenKindMask, result);
        }
        if (level < levelMax || pChildren->Item(i)->m_TokenKind == tkBlockConstruct)
            GetChildren(pChildren->Item(i), tokenKindMask, result, level+1, levelMax);
    }
}

void ParserF::GetChildrenAssociateConstruct(TokenF* token, int tokenKindMask, TokensArrayFlat& result)
{
    TokensArrayF* pACCh = &token->m_Children;
    for (size_t j=0; j<pACCh->GetCount(); j++)
    {
        if (pACCh->Item(j)->m_TokenKind & tokenKindMask)
        {
            result.Add(new TokenFlat(pACCh->Item(j)));
        }
        else if (pACCh->Item(j)->m_TokenKind == tkAssociateConstruct)
        {
            GetChildrenAssociateConstruct(pACCh->Item(j), tokenKindMask, result);
        }
    }
}


void ParserF::FindMatchTokensAtInclude(cbEditor* ed, const wxString& findName, bool onlyPublicNames, bool partialMach, TokensArrayFlat& result)
{
    wxChar sep = wxFileName::GetPathSeparator();
    wxString fname = ed->GetFilename().AfterLast(sep);
    wxString parFName = m_pIncludeDB->GetOneParentFile(fname);

    if (parFName.IsEmpty())
        return;

    TokenF* pFileToken = FindFileTokenWithName(parFName);
    if (!pFileToken)
        return;

    TokensArrayFlatClass includeTmp;
    TokensArrayFlat* includeToks = includeTmp.GetTokens();
    int mask = tkInclude;

    FindMatchChildrenDeclared(pFileToken->m_Children, fname.Lower(), *includeToks, mask, false, mask, onlyPublicNames);

    if (includeToks->Count() == 0)
        return;

    int tokKind = tkModule | tkFunction | tkProgram | tkSubroutine | tkPreprocessor | tkInterface |
                  tkBlockData | tkType | tkVariable | tkProcedure;
    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* resultTmp = tokensTmp.GetTokens();
    TokensArrayFlatClass tokensTmpU;
    TokensArrayFlat* resultTmpU = tokensTmpU.GetTokens();
    FindUseAssociatedTokens(onlyPublicNames, includeToks->Item(0), findName, partialMach, *resultTmp, tokKind, false, resultTmpU);
    FindImplementedProcInMySubmodules(ed, findName, *resultTmp);
    for (size_t i=0; i<resultTmpU->GetCount(); i++)
    {
        AddUniqueResult(result, resultTmpU->Item(i));
    }
    for (size_t i=0; i<resultTmp->GetCount(); i++)
    {
        result.Add(new TokenFlat(resultTmp->Item(i)));
    }
}

void ParserF::ChangeAddressWithInclude(TokensArrayFlat& tokArr)
{
    size_t arrSize = tokArr.size();
    for (size_t i=0; i<arrSize; ++i)
    {
        if (tokArr.Item(i)->m_WasIncluded)
        {
            tokArr.Item(i)->m_Filename = tokArr.Item(i)->m_IncludeFilename;
            tokArr.Item(i)->m_LineStart = tokArr.Item(i)->m_IncludeLineStart;
        }
    }
}

void ParserF::BuildCalledByDict(CalledByDict& cByDict)
{
    cByDict.Build(m_pTokens);
}

void ParserF::SetInterpretCPP(bool interpretCPP)
{
    m_InterpretCPP = interpretCPP;
}

void ParserF::SetSkippedLines(const wxString& fileName, std::vector<int>& skipLineStart, std::vector<int>& skipLineEnd)
{
    FillSkippedLines(m_SkippedLinesMap, fileName, skipLineStart, skipLineEnd);
}

void ParserF::SetNewSkippedLines(const wxString& fileName, std::vector<int>& skipLineStart, std::vector<int>& skipLineEnd)
{
    FillSkippedLines(m_NewSkippedLinesMap, fileName, skipLineStart, skipLineEnd);
}

void ParserF::FillSkippedLines(std::map<wxString,std::vector<int>*>& fileLineMap, const wxString& fileName,
                                 std::vector<int>& skipLineStart, std::vector<int>& skipLineEnd)
{
    if (fileLineMap.count(fileName) > 0)
    {
        std::vector<int> *oldSkippedLines = fileLineMap[fileName];
        delete oldSkippedLines;
        fileLineMap[fileName] = NULL;
    }
    if (skipLineStart.empty() || skipLineEnd.empty())
        return;

    size_t nSkip = skipLineStart.size();
    if (nSkip != skipLineEnd.size())
        return; // It should not happen.

    std::vector<int> *skippedLines = new std::vector<int>();
    for (size_t i=0; i<nSkip; ++i)
    {
        int jEnd = skipLineEnd[i];
        for (int j=skipLineStart[i]; j<=jEnd; ++j)
        {
            skippedLines->push_back(j);
        }
    }
    fileLineMap[fileName] = skippedLines;
}

void ParserF::ConnectToNewSkippedLines()
{
    wxMutexLocker mlocker(s_NewSkippedLinesMutex);
    for (auto const& mPair : m_SkippedLinesMap)
    {
        delete mPair.second;
    }
    m_SkippedLinesMap.clear();
    for (auto const& mPair : m_NewSkippedLinesMap)
    {
        m_SkippedLinesMap[mPair.first] = mPair.second;
    }
    m_NewSkippedLinesMap.clear();
}

std::vector<int>* ParserF::GetSkippedLines(const wxString& fileName)
{
    if (fileName.empty() || m_SkippedLinesMap.count(fileName) == 0)
        return NULL;

    return m_SkippedLinesMap[fileName];
}
