/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#include "parserthreadf.h"

#include <sdk.h>
#ifndef CB_PRECOMP
#endif
#include <set>

#include <wx/tokenzr.h>

#include "usetokenf.h"

ParserThreadF::ParserThreadF(const wxString& projectFilename,
                             const wxString& bufferOrFilename,
                             TokensArrayF* tokens,
                             FortranSourceForm fsForm,
                             bool isBuffer,
                             IncludeDB* includeDB,
                             bool interpretCPP,
                             std::map<wxString,wxString>* aIncludeFiles,
                             const std::vector<wxString>* projPPDefineTokens)
    :
    m_pTokens(tokens),
    m_pLastParent(0L),
    m_pIncludeDB(includeDB),
    m_interpretCPP(interpretCPP),
    m_pAIncludeFiles(aIncludeFiles),
    m_Briefend("@brief_end@")
{
    m_InterfaceOperator = 0;
    m_InterfaceAssignment = 0;
    m_InterfaceRead = 0;
    m_InterfaceWrite = 0;

    m_pPPDefineTokens = new TokensArrayF();
    m_inIfdef = 0;
    m_addPPDefineTokens = false;

    if (!isBuffer)
    {
        if (!bufferOrFilename.IsEmpty())
        {
            m_Filename = bufferOrFilename;
            m_Tokens.Init(m_Filename, fsForm, m_interpretCPP);
            wxChar sep = wxFileName::GetPathSeparator();
            m_pLastParent = DoAddFileToken(bufferOrFilename.AfterLast(sep), projectFilename);
        }
    }
    else
    {
        m_Tokens.InitFromBuffer(bufferOrFilename, fsForm, m_interpretCPP);
    }
    m_Tokens.SetParent(this);
    InitSecondEndPart();

    if (projPPDefineTokens)
    {
        m_ProjPPDefineTokens = *projPPDefineTokens;
    }
}

ParserThreadF::ParserThreadF(const wxString& projectFilename,
                             const wxString& filename,
                             TokensArrayF* tokens,
                             FortranSourceForm fsForm,
                             IncludeDB* includeDB,
                             bool interpretCPP,
                             std::map<wxString,wxString>* aIncludeFiles,
                             const std::vector<wxString>* projPPDefineTokens,
                             const wxString& buffer)
    :
    m_pTokens(tokens),
    m_pLastParent(0L),
    m_pIncludeDB(includeDB),
    m_interpretCPP(interpretCPP),
    m_pAIncludeFiles(aIncludeFiles),
    m_Briefend("@brief_end@")
{
    m_InterfaceOperator = 0;
    m_InterfaceAssignment = 0;
    m_InterfaceRead = 0;
    m_InterfaceWrite = 0;

    m_pPPDefineTokens = new TokensArrayF();
    m_inIfdef = 0;
    m_addPPDefineTokens = true;

    m_Filename = filename;
    m_Tokens.InitFromBuffer(buffer, fsForm, m_interpretCPP);
    m_Tokens.SetFilename(filename);
    m_Tokens.SetParent(this);
    wxChar sep = wxFileName::GetPathSeparator();
    m_pLastParent = DoAddFileToken(filename.AfterLast(sep), projectFilename);

    InitSecondEndPart();

    if (projPPDefineTokens)
    {
        m_ProjPPDefineTokens = *projPPDefineTokens;
    }
}

ParserThreadF::~ParserThreadF()
{
    //dtor
    delete m_pPPDefineTokens;
}

void ParserThreadF::InitSecondEndPart()
{
    m_KnownEndSecPart.insert("subroutine");
    m_KnownEndSecPart.insert("function");
    m_KnownEndSecPart.insert("module");
    m_KnownEndSecPart.insert("submodule");
    m_KnownEndSecPart.insert("type");
    m_KnownEndSecPart.insert("interface");
    m_KnownEndSecPart.insert("program");
    m_KnownEndSecPart.insert("block");
    m_KnownEndSecPart.insert("blockdata");
    m_KnownEndSecPart.insert("associate");
    m_KnownEndSecPart.insert("procedure");

    m_NumberOfBlockConstruct = 0;
}

bool ParserThreadF::Parse()
{
    if (!m_pTokens || !m_Tokens.IsOK())
    {
        return false;
    }

    while (1)
    {
        wxString token = m_Tokens.GetToken();
        if (token.IsEmpty())
            break;

        wxString tok_low = token.Lower();
        wxString next = m_Tokens.PeekToken();
        wxString nex_low = next.Lower();

        if (tok_low.Matches("use"))
        {
            HandleUse();
        }
        else if (tok_low.Matches("module") && !nex_low.Matches("procedure")
                 && !nex_low.Matches("function")  && !nex_low.Matches("subroutine")
                 && !nex_low.Matches("pure") && !nex_low.Matches("elemental"))
        {
            HandleModule();
        }
        else if (tok_low.Matches("submodule") && !nex_low.Matches("procedure"))
        {
            HandleSubmodule();
        }
        else if (tok_low.Matches("program"))
        {
            HandleFunction(tkProgram);
        }
        else if (tok_low.Matches("function"))
        {
            HandleFunction(tkFunction);
        }
        else if (tok_low.Matches("subroutine"))
        {
            HandleFunction(tkSubroutine);
        }
        else if (tok_low.Matches("type") && !nex_low(0,1).Matches("(") && !nex_low.Matches("is"))
        {
            HandleType();
        }
        else if (tok_low.Matches("block"))
        {
            if (nex_low.Matches("data"))
            {
                token = m_Tokens.GetToken();
                tok_low = token.Lower();
                next = m_Tokens.PeekToken();
                nex_low = next.Lower();
                HandleBlockData();
            }
            else
            {
                HandleBlockConstruct();
            }
        }
        else if (tok_low.Matches("blockdata"))
        {
            HandleBlockData();
        }
        else if (tok_low.Matches("include"))
        {
            HandleInclude();
        }
        else if (tok_low.GetChar(0) == '#')
        {
            HandlePPDirective(token);
        }
        else if (tok_low.Matches("interface"))
        {
            HandleInterface();
        }
        else if (tok_low.Matches("associate"))
        {
            HandleAssociateConstruct();
        }
        else if ((tok_low.Matches("select") && nex_low.Matches("type")) ||
                 tok_low.Matches("selecttype"))
        {
            HandleSelectTypeConstruct();
        }
        else if ((tok_low.Matches("select") && nex_low.Matches("case")) ||
                 tok_low.Matches("selectcase"))
        {
            HandleSelectCaseConstruct();
        }
        else if (tok_low.Matches("end"))
        {
            // something is wrong with code or parser
            m_Tokens.SkipToOneOfChars(";", true);
        }
        else if (tok_low.Matches("procedure") && nex_low(0,1).Matches("("))
        {
            ParseTypeBoundProcedures(token, true, false);
        }
        else
        {
            bool needDefault = true;
            bool hasFunctionInLine;
            TokensArrayF tokTmpArr;
            CheckParseOneDeclaration(token, tok_low, next, nex_low, needDefault, tokTmpArr, hasFunctionInLine);
        }
    }

    if (!m_Filename.empty() && m_pIncludeDB)
    {
        //update IncludeDB
        wxChar sep = wxFileName::GetPathSeparator();
        m_pIncludeDB->SetInclude(m_Filename.AfterLast(sep), m_IncludeList);
    }

    if (m_addPPDefineTokens)
    {
        // Append "#define abc" tokens.
        std::set<wxString>* pDefTok = m_Tokens.GetDefinedTokens();
        for (auto defTokName : *pDefTok)
        {
            DoAddDefineToken(defTokName);
        }
        size_t nDef = m_ProjPPDefineTokens.size();
        for (size_t i=0; i<nDef; ++i)
        {
            DoAddDefineToken(m_ProjPPDefineTokens[i]);
        }
    }

    return true;
}

TokenF* ParserThreadF::DoAddToken(TokenKindF kind, const wxString& name, const wxString& args, const wxString& typeDefinition)
{
    TokenF* newToken = new TokenF;
    newToken->m_Name = name.Lower();

    newToken->m_TokenKind = kind;
    newToken->m_pParent = m_pLastParent;
    newToken->m_Filename = m_Tokens.GetParentFilename();
    newToken->m_LineStart = m_Tokens.GetParentLineNumber();
    newToken->m_DisplayName = name;
    newToken->m_Args = args;
    newToken->m_TypeDefinition = typeDefinition;
    newToken->m_DefinitionLength = 1;
    if (m_Tokens.IsInInclude())
    {
        newToken->m_WasIncluded = true;
        newToken->m_IncludeFilename = m_Tokens.GetFilename();
        newToken->m_IncludeLineStart = m_Tokens.GetLineNumber();
        newToken->m_IncludeLineEnd = newToken->m_IncludeLineStart;
    }

    if (m_pLastParent)
    {
        m_pLastParent->AddChild(newToken);
    }
    else
    {
        m_pTokens->Add(newToken);
    }

    return newToken;
}

TokenF* ParserThreadF::DoAddToken(TokenKindF kind, const wxString& name, const wxString& args, const unsigned int defStartLine)
{
    TokenF* newToken = new TokenF;
    newToken->m_Name = name.Lower();

    newToken->m_TokenKind = kind;
    newToken->m_pParent = m_pLastParent;
    newToken->m_Filename = m_Tokens.GetParentFilename();
    newToken->m_DisplayName = name;
    newToken->m_Args = args;
    newToken->m_TypeDefinition = wxEmptyString;

    newToken->m_LineStart = m_Tokens.GetParentLineNumber();
    newToken->m_DefinitionLength = m_Tokens.GetLineNumber() - defStartLine + 1;
    if (m_Tokens.IsInInclude())
    {
        newToken->m_WasIncluded = true;
        newToken->m_IncludeFilename = m_Tokens.GetFilename();
        newToken->m_IncludeLineStart = defStartLine;
        newToken->m_IncludeLineEnd = newToken->m_IncludeLineStart;
    }

    if (m_pLastParent)
    {
        m_pLastParent->AddChild(newToken);
    }
    else
    {
        m_pTokens->Add(newToken);
    }

    return newToken;
}

FileTokenF* ParserThreadF::DoAddFileToken(const wxString& filename, const wxString& projectFilename)
{
    FileTokenF* newToken = new FileTokenF;
    newToken->m_Name = filename.Lower();
    newToken->m_TokenKind = tkFile;
    newToken->m_pParent = m_pLastParent;
    newToken->m_Filename = m_Tokens.GetFilename();
    newToken->m_LineStart = 0;
    newToken->m_DisplayName = filename;
    newToken->m_DefinitionLength = 1;

    m_pTokens->Add(newToken);

    newToken->m_ProjectFilename = projectFilename;

    return newToken;
}

void ParserThreadF::DoAddDefineToken(const wxString& defTokName)
{
    TokenF* newToken = new TokenF;
    newToken->m_Name = defTokName.Lower();
    newToken->m_TokenKind = tkMacroDefine;
    newToken->m_Filename = m_Filename;
    newToken->m_DisplayName = defTokName;
    newToken->m_DefinitionLength = 1;
    newToken->m_LineStart = 1;
    newToken->m_LineEnd = m_Tokens.GetLineNumber();

    TokenF* parent = m_pLastParent;
    while (parent)
    {
        if (parent->m_TokenKind == tkFile)
            break;

        parent = parent->m_pParent;
    }

    if (parent) // tkFile was found.
    {
        parent->AddChild(newToken);
        newToken->m_pParent = parent;
    }
}

void ParserThreadF::HandleUse()
{
    wxString modName;
    wxArrayString lineTok = m_Tokens.GetTokensToEOL();
    ModuleNature modNature = mnNonIntrinsic;
    int ltCount = lineTok.GetCount();
    int idx = lineTok.Index("::");
    if (idx != wxNOT_FOUND)
    {
        if (idx > 0)
        {
            if (lineTok.Item(idx-1).Lower().IsSameAs("intrinsic"))
            {
                modNature = mnIntrinsic;
            }
        }
        idx++;
    }
    else
    {
        idx = 0;
    }
    if (ltCount > idx)
    {
        modName = lineTok.Item(idx);
    }
    else
    {
        return; //something wrong
    }
    UseTokenF* pUseTok = DoAddUseToken(modName);
    pUseTok->SetModuleNature(modNature);

    idx++;
    if (ltCount <= idx)
    {
        return; // no more on the line
    }
    if (lineTok.Item(idx).Lower().IsSameAs("only"))
    {
        pUseTok->SetOnly(true);
        idx++;
        while (true)
        {
            idx++;
            if (ltCount <= idx)
                break;
            wxString localName = lineTok.Item(idx);
            wxString externalName;

            if (localName.Lower().IsSameAs("operator"))
            {
                idx += 4; // operator (.st.) => operator (.kt.)
                continue;
            }
            if (ltCount > idx+1 && lineTok.Item(idx+1).IsSameAs("=>"))
            {
                //it is rename
                if (ltCount > idx+2)
                {
                    idx += 2;
                    externalName = lineTok.Item(idx);
                }
                else
                {
                    break; // '=>' on end of line
                }
            }
            if (externalName.IsEmpty())
                pUseTok->AddToNamesList(localName);
            else
                pUseTok->AddToRenameList(localName, externalName);
        }
    }
    else
    {
        pUseTok->SetOnly(false);
        // rename-list
        while (true)
        {
            if (lineTok.Item(idx).Lower().IsSameAs("operator"))
            {
                idx += 5; // operator (.st.) => operator (.kt.)
            }
            if (ltCount > idx+1 && lineTok.Item(idx+1).IsSameAs("=>"))
            {
                wxString localName = lineTok.Item(idx);
                wxString externalName;
                if (ltCount > idx+2)
                {
                    idx += 2;
                    externalName = lineTok.Item(idx);
                }
                else
                {
                    break; // '=>' on end of line
                }
                pUseTok->AddToRenameList(localName, externalName);
                idx++;
                if (ltCount <= idx)
                    break;
            }
            else
            {
                break;
            }
        }
    }
}


UseTokenF* ParserThreadF::DoAddUseToken(const wxString& modName)
{
    UseTokenF* newToken = new UseTokenF();
    newToken->m_Name = modName.Lower();

    newToken->m_TokenKind = tkUse;
    newToken->m_pParent = m_pLastParent;
    newToken->m_Filename = m_Tokens.GetParentFilename();
    newToken->m_DisplayName = modName;
    newToken->m_TypeDefinition = wxEmptyString;
    newToken->m_LineStart = m_Tokens.GetParentLineNumber();
    newToken->m_DefinitionLength = 1;
    if (m_Tokens.IsInInclude())
    {
        newToken->m_WasIncluded = true;
        newToken->m_IncludeFilename = m_Tokens.GetFilename();
        newToken->m_IncludeLineStart = m_Tokens.GetLineNumber();
        newToken->m_IncludeLineEnd = newToken->m_IncludeLineStart;
    }

    if (m_pLastParent)
    {
        m_pLastParent->AddChild(newToken);
    }
    else
    {
        m_pTokens->Add(newToken);
    }

    return newToken;
}

void ParserThreadF::HandleModule()
{
    TokenKindF kind = tkModule;
    TokenF* old_parent = m_pLastParent;

    int countAccessList = 0;
    wxString token = m_Tokens.GetTokenSameLine();
    TokenAccessKind taDefKind = taPublic;
    ModuleTokenF* modToken;
    if (token.IsEmpty())
        modToken = DoAddModuleToken("unnamed");
    else
        modToken = DoAddModuleToken(token);
    m_pLastParent = modToken;

    // Parse documentation
    m_ParentDocs.Clear();
    DocBlock docs;
    GetDocBlock(docs, false, modToken->m_LineStart, true);
    if (docs.HasBrief() || docs.HasDescription())
        modToken->m_DocString << docs.GetBrief() + m_Briefend + docs.GetDescription();

    wxArrayString privateNameList;
    wxArrayString publicNameList;
    wxArrayString protectedNameList;

    TokensArrayF typeTokensAll;
    TokensArrayF interfGenTokens;
    TokensArrayF interfTokens;

    while (1)
    {
        token = m_Tokens.GetToken();
        if (token.IsEmpty())
            break;
        wxString tok_low = token.Lower();

        wxString next = m_Tokens.PeekToken();
        wxString nex_low = next.Lower();
        if ( ((m_Tokens.GetLineNumber() == m_Tokens.GetPeekedLineNumber()) && IsEnd(tok_low, nex_low)) ||
             ((m_Tokens.GetLineNumber() != m_Tokens.GetPeekedLineNumber()) && IsEnd(tok_low, "")) )
        {
            m_Tokens.SkipToOneOfChars(";", true);
            break;
        }
        else if (tok_low.Matches("type") && !nex_low(0,1).Matches("("))
        {
            bool needDefault=true;
            TokenF* tokTmp = 0;
            HandleType(needDefault, tokTmp);
            if (tokTmp)
                typeTokensAll.Add(tokTmp);
            if (!needDefault && tokTmp)
            {
                if (tokTmp->m_TokenAccess == taPrivate)
                    privateNameList.Add(tokTmp->m_Name);
                else
                    publicNameList.Add(tokTmp->m_Name);
            }
        }
        else if (tok_low.Matches("subroutine"))
        {
            HandleFunction(tkSubroutine, taDefKind);
        }
        else if (tok_low.Matches("function"))
        {
            HandleFunction(tkFunction, taDefKind);
        }
        else if (tok_low.Matches("use"))
        {
            HandleUse();
        }
        else if (tok_low.Matches("interface"))
        {
            TokenF* tokTmp = 0;
            bool isGeneric = false;
            HandleInterface(taDefKind, tokTmp, isGeneric);
            if (isGeneric && tokTmp)
                interfGenTokens.Add(tokTmp);
            if (tokTmp)
                interfTokens.Add(tokTmp);
        }
        else if (tok_low.Matches("include"))
        {
            HandleInclude();
        }
        else if (tok_low.GetChar(0) == '#')
        {
            HandlePPDirective(token);
        }
        else if (tok_low.Matches("private"))
        {
            bool changeDefault;
            HandleAccessList(taPrivate, changeDefault, countAccessList, privateNameList);
            if (changeDefault)
            {
                modToken->SetDefaultPublic(false);
                taDefKind = taPrivate;
            }
        }
        else if (tok_low.Matches("public"))
        {
            bool changeDefault;
            HandleAccessList(taPublic, changeDefault, countAccessList, publicNameList);
            if (changeDefault)
            {
                modToken->SetDefaultPublic(true);
                taDefKind = taPublic;
            }
        }
        else if (tok_low.Matches("protected"))
        {
            bool tmpB;
            HandleAccessList(taProtected, tmpB, countAccessList, protectedNameList);
        }
        else if (kind == tkModule)
        {
            bool needDefault=true;
            bool hasFunctionInLine;
            TokensArrayF tokTmpArr;
            CheckParseOneDeclaration(token, tok_low, next, nex_low, needDefault, tokTmpArr, hasFunctionInLine);
            if (!needDefault)
            {
                for (size_t i=0; i<tokTmpArr.Count(); i++)
                {
                    if (tokTmpArr.Item(i)->m_TokenAccess == taPrivate)
                        privateNameList.Add(tokTmpArr.Item(i)->m_Name);
                    else
                        publicNameList.Add(tokTmpArr.Item(i)->m_Name);
                }
            }
        }
    }

    modToken->AddLineEnd(m_Tokens.GetParentLineNumber());
    if (modToken->m_WasIncluded)
    {
        modToken->m_IncludeLineEnd = m_Tokens.GetLineNumber();
    }

    modToken->AddLineEnd(m_Tokens.GetLineNumber());
    m_pLastParent = old_parent;

    size_t interfTokensCount = interfTokens.GetCount();
    for (size_t i=0; i<interfTokensCount; i++)
    {
        interfTokens.Item(i)->m_TokenAccess = taDefKind;
        if (interfTokens.Item(i)->m_TokenKind == tkInterfaceExplicit)
        {
            TokensArrayF* chs = &interfTokens.Item(i)->m_Children;
            for (size_t j=0; j<chs->GetCount(); j++)
            {
                chs->Item(j)->m_TokenAccess = taDefKind;
            }
        }
    }

    size_t nameListCount = publicNameList.GetCount();
    for (size_t i=0; i<nameListCount; i++)
    {
        modToken->AddToPublicList(publicNameList.Item(i));
    }
    nameListCount = privateNameList.GetCount();
    for (size_t i=0; i<nameListCount; i++)
    {
        modToken->AddToPrivateList(privateNameList.Item(i));
    }

    TokensArrayF* toks = &modToken->m_Children;
    if (toks)
    {
        size_t toksCount = toks->GetCount();
        for (size_t i=0; i<toksCount; i++)
        {
            SetTokenAccess(modToken, toks->Item(i), taDefKind);

            if (protectedNameList.Index(toks->Item(i)->m_Name) != wxNOT_FOUND)
            {
                toks->Item(i)->m_TokenAccess = taProtected;
            }
            else if (toks->Item(i)->m_TokenKind == tkInterfaceExplicit)
            {
                TokensArrayF* chs = &toks->Item(i)->m_Children;
                if (chs)
                {
                    for (size_t j=0; j<chs->GetCount(); j++)
                    {
                        SetTokenAccess(modToken, chs->Item(j), taDefKind);
                    }
                }
            }
        }
    }

    // find kind of children of GenericInterfaces
    for (size_t i=0; i<interfGenTokens.GetCount(); i++)
    {
        TokensArrayF* chs = &interfGenTokens.Item(i)->m_Children;
        for (size_t j=0; j<chs->GetCount(); j++)
        {
            wxString intfname = chs->Item(j)->m_Name;
            TokensArrayF* modChs = &modToken->m_Children;
            TokenKindF tk;
            bool found = false;
            size_t modChsCount = modChs->GetCount();
            for (size_t k=0; k<modChsCount; k++)
            {
                if ((modChs->Item(k)->m_TokenKind == tkSubroutine || modChs->Item(k)->m_TokenKind == tkFunction) &&
                        modChs->Item(k)->m_Name.IsSameAs(intfname))
                {
                    tk = modChs->Item(k)->m_TokenKind;
                    found = true;
                    break;
                }
                else if (modChs->Item(k)->m_TokenKind == tkInterfaceExplicit ||
                         modChs->Item(k)->m_TokenKind == tkInterface)
                {
                    TokensArrayF* intfExpChs = &modChs->Item(k)->m_Children;
                    if (intfExpChs)
                    {
                        for (size_t m=0; m<intfExpChs->GetCount(); m++)
                        {
                            if ((intfExpChs->Item(m)->m_TokenKind == tkSubroutine || intfExpChs->Item(m)->m_TokenKind == tkFunction) &&
                                    intfExpChs->Item(m)->m_Name.IsSameAs(intfname))
                            {
                                tk = intfExpChs->Item(m)->m_TokenKind;
                                found = true;
                                break;
                            }
                        }
                        if (found)
                            break;
                    }
                }
            }

            if (found)
            {
                // write kind for GenericInterface
                if (tk == tkFunction)
                    interfGenTokens.Item(i)->m_TypeDefinition = "function";
                else
                    interfGenTokens.Item(i)->m_TypeDefinition = "subroutine";
                break;
            }
        }
    }

    for (size_t i=0; i<typeTokensAll.GetCount(); i++)
    {
        TokensArrayF* chs = &typeTokensAll.Item(i)->m_Children;
        TokensArrayF genericProc;
        size_t chsCount = chs->GetCount();
        for (size_t j=0; j<chsCount; j++)
        {
            if (chs->Item(j)->m_TokenKind == tkProcedure)
            {
                wxString procName;
                if (chs->Item(j)->m_PartLast.IsEmpty())
                    procName = chs->Item(j)->m_Name;
                else
                    procName = chs->Item(j)->m_PartLast;

                TokensArrayF* modChs = &modToken->m_Children;
                size_t modChsCount = modChs->GetCount();
                TokenKindF tk;
                bool found = false;
                for (size_t k=0; k<modChsCount; k++)
                {
                    if ((modChs->Item(k)->m_TokenKind == tkSubroutine || modChs->Item(k)->m_TokenKind == tkFunction) &&
                            modChs->Item(k)->m_Name.IsSameAs(procName))
                    {
                        tk = modChs->Item(k)->m_TokenKind;
                        found = true;
                        break;
                    }
                    else if (modChs->Item(k)->m_TokenKind == tkInterfaceExplicit)
                    {
                        TokensArrayF* intfExpChs = &modChs->Item(k)->m_Children;
                        if (intfExpChs)
                        {
                            for (size_t m=0; m<intfExpChs->GetCount(); m++)
                            {
                                if ((intfExpChs->Item(m)->m_TokenKind == tkSubroutine || intfExpChs->Item(m)->m_TokenKind == tkFunction) &&
                                        intfExpChs->Item(m)->m_Name.IsSameAs(procName))
                                {
                                    tk = intfExpChs->Item(m)->m_TokenKind;
                                    found = true;
                                    break;
                                }
                            }
                            if (found)
                                break;
                        }
                    }
                }

                if (found)
                {
                    if (tk == tkFunction)
                        chs->Item(j)->m_TypeDefinition = "function";
                    else
                        chs->Item(j)->m_TypeDefinition = "subroutine";
                }
            }
            else if (chs->Item(j)->m_TokenKind == tkInterface)
                genericProc.Add(chs->Item(j));
        }

        for (size_t k=0; k<genericProc.GetCount(); k++)
        {
            wxStringTokenizer tkz(genericProc.Item(k)->m_PartLast);
            while (tkz.HasMoreTokens())
            {
                wxString pron = tkz.GetNextToken().Lower();
                for (size_t j=0; j<chs->GetCount(); j++)
                {
                    if (chs->Item(j)->m_TokenKind == tkProcedure && chs->Item(j)->m_Name.IsSameAs(pron))
                    {
                        genericProc.Item(k)->m_TypeDefinition = chs->Item(j)->m_TypeDefinition;
                        break;
                    }
                }
                break;
            }
        }
    }

    if (modToken->m_DocString.IsEmpty())
    {
        unsigned int ln = modToken->m_LineStart + modToken->m_DefinitionLength - 1;
        docs.Clear();
        GetDocBlock(docs, true, ln, true); // look bellow the declaration for the non-doxyblocks documentation.
        if (docs.HasBrief() || docs.HasDescription())
            modToken->m_DocString << docs.GetBrief() + m_Briefend + docs.GetDescription();
    }
}

void ParserThreadF::HandleSubmodule()
{
    TokenF* old_parent = m_pLastParent;
    unsigned int defStartLine = m_Tokens.GetLineNumber();
    wxString ancestorModule;
    wxString parentSubmodule;
    wxString submName;

    wxString token = m_Tokens.GetTokenSameFortranLine();

    if (!token.empty() && token(0,1).Matches("("))
    {
        token = token.Mid(1).BeforeFirst(')');
        int i = token.Find(':');
        if (i != wxNOT_FOUND)
        {
            ancestorModule = token.Mid(0,i).Trim().Trim(false);
            if (i+1 < int(token.length()))
                parentSubmodule = token.Mid(i+1).Trim().Trim(false);
        }
        else
            ancestorModule = token.Trim().Trim(false);

        token = m_Tokens.GetTokenSameFortranLine();
        if (!token.IsEmpty())
            submName = token;
        else
            submName = "unnamed";
    }
    else if(token.IsEmpty())
        submName = "unnamed";
    else
        submName = token;

    SubmoduleTokenF* pSubmodToken = DoAddSubmoduleToken(submName, ancestorModule, parentSubmodule, defStartLine);
    m_pLastParent = pSubmodToken;

    while (1)
    {
        token = m_Tokens.GetToken();
        if (token.IsEmpty())
            break;
        wxString tok_low = token.Lower();

        wxString next = m_Tokens.PeekToken();
        wxString nex_low = next.Lower();
        if ( ((m_Tokens.GetLineNumber() == m_Tokens.GetPeekedLineNumber()) && IsEnd(tok_low, nex_low)) ||
                ((m_Tokens.GetLineNumber() != m_Tokens.GetPeekedLineNumber()) && IsEnd(tok_low, "")) )
        {
            m_Tokens.SkipToOneOfChars(";", true);
            break;
        }
        else if (tok_low.Matches("type") && !nex_low(0,1).Matches("("))
        {
            HandleType();
        }
        else if (tok_low.Matches("subroutine"))
        {
            HandleFunction(tkSubroutine);
        }
        else if (tok_low.Matches("function"))
        {
            HandleFunction(tkFunction);
        }
        else if (tok_low.Matches("use"))
        {
            HandleUse();
        }
        else if (tok_low.Matches("interface"))
        {
            HandleInterface();
        }
        else if (tok_low.Matches("include"))
        {
            HandleInclude();
        }
        else if (tok_low.GetChar(0) == '#')
        {
            HandlePPDirective(token);
        }
        else if (tok_low.Matches("module") && nex_low.Matches("procedure"))
        {
            m_Tokens.GetToken();
            HandleSubmoduleProcedure();
        }
        else
        {
            bool needDefault=true;
            bool hasFunctionInLine;
            TokensArrayF tokTmpArr;
            CheckParseOneDeclaration(token, tok_low, next, nex_low, needDefault, tokTmpArr, hasFunctionInLine);
        }
    }
    pSubmodToken->AddLineEnd(m_Tokens.GetLineNumber());
    m_pLastParent = old_parent;
}

ModuleTokenF* ParserThreadF::DoAddModuleToken(const wxString& modName)
{
    ModuleTokenF* newToken = new ModuleTokenF();
    newToken->m_Name = modName.Lower();

    newToken->m_TokenKind = tkModule;
    newToken->m_pParent = m_pLastParent;
    newToken->m_Filename = m_Tokens.GetParentFilename();
    newToken->m_DisplayName = modName;
    newToken->m_TypeDefinition = wxEmptyString;
    newToken->m_LineStart = m_Tokens.GetParentLineNumber();
    newToken->m_DefinitionLength = 1;
    if (m_Tokens.IsInInclude())
    {
        newToken->m_WasIncluded = true;
        newToken->m_IncludeFilename = m_Tokens.GetFilename();
        newToken->m_IncludeLineStart = m_Tokens.GetLineNumber();
        newToken->m_IncludeLineEnd = newToken->m_IncludeLineStart;
    }

    if (m_pLastParent)
    {
        m_pLastParent->AddChild(newToken);
    }
    else
    {
        m_pTokens->Add(newToken);
    }

    return newToken;
}

SubmoduleTokenF* ParserThreadF::DoAddSubmoduleToken(const wxString& submName, const wxString& ancestorModule,
                                                    const wxString& parentSubmodule, unsigned int defStartLine)
{
    SubmoduleTokenF* newToken = new SubmoduleTokenF();
    newToken->m_Name = ancestorModule.Lower();
    newToken->m_Name << ":" << submName.Lower();
    newToken->m_TokenKind = tkSubmodule;
    newToken->m_pParent = m_pLastParent;
    newToken->m_Filename = m_Tokens.GetFilename();
    newToken->m_DisplayName = submName;
    newToken->m_DisplayName << " (" << ancestorModule;
    if (!parentSubmodule.IsEmpty())
        newToken->m_DisplayName << ":" << parentSubmodule;
    newToken->m_DisplayName << ")";
    newToken->m_TypeDefinition = wxEmptyString;

    newToken->m_LineStart = defStartLine;
    newToken->m_DefinitionLength = 1;

    newToken->m_AncestorModuleName = ancestorModule.Lower();
    newToken->m_ParentSubmoduleName = parentSubmodule.Lower();

    if (m_pLastParent)
        m_pLastParent->AddChild(newToken);
    else
        m_pTokens->Add(newToken);
//
//    TokenF* oldParent = m_pLastParent;
//    m_pLastParent = newToken;
//    wxString useName = ancestorModule.Lower();
//    useName << ":" << parentSubmodule.Lower();
//    UseTokenF* pUseTok = DoAddUseToken(useName);
//    pUseTok->SetModuleNature(mnNonIntrinsic);
//    m_pLastParent = oldParent;

    return newToken;
}

void ParserThreadF::HandleType()
{
    bool needDefault;
    TokenF* newToken = 0;
    HandleType(needDefault, newToken);
}

void ParserThreadF::HandleType(bool& needDefault, TokenF* &newToken)
{
    needDefault = true;
    TokenAccessKind taKind = taPublic;
    wxString typeName;
    wxString exTypeName;
    wxArrayString lineTok = m_Tokens.GetTokensToEOL();
    wxArrayString lineTokLw;
    wxString wholeLine;
    bool isAbstract = false;
    MakeArrayStringLower(lineTok, lineTokLw);
    int idx = lineTok.Index("::");
    if (idx != wxNOT_FOUND)
    {
        if (idx+1 < int(lineTok.GetCount()))
        {
            typeName = lineTok.Item(idx+1);
            int idex = lineTokLw.Index("extends",false);
            if (idex != wxNOT_FOUND)
            {
                if (idex <= idx-2)
                {
                    wxString ex = lineTok.Item(idex+1);
                    int idx_a = ex.Find('(');
                    int idx_b = ex.Find(')', true);
                    if ( idx_a != wxNOT_FOUND && idx_b != wxNOT_FOUND && idx_a < (idx_b-1) )
                    {
                        exTypeName = ex.Mid(idx_a+1,idx_b-idx_a-1).Trim().Trim(false);
                    }
                }
            }

            idex = lineTokLw.Index("private");
            if (idex != wxNOT_FOUND && idex < idx)
            {
                taKind = taPrivate;
                needDefault = false;
            }
            else
            {
                idex = lineTokLw.Index("public");
                if (idex != wxNOT_FOUND && idex < idx)
                {
                    taKind = taPublic;
                    needDefault = false;
                }
            }

            idex = lineTokLw.Index("abstract");
            if (idex != wxNOT_FOUND && idex < idx)
            {
                isAbstract = true;
            }
        }
        else
        {
            //something wrong
            return;
        }

        for (int i=0; i<idx; i++)
        {
            if (lineTokLw.Item(i+1).StartsWith("(") || i+1 == idx)
                wholeLine << lineTokLw.Item(i);
            else
                wholeLine << lineTokLw.Item(i) << ",";

        }
        wholeLine << "::";
        for (size_t i=idx+1; i<lineTokLw.size(); i++)
            wholeLine << lineTokLw.Item(i);
    }
    else
    {
        if (lineTok.GetCount() > 0)
        {
            typeName = lineTok.Item(0);
        }
        else
        {
            //something wrong
            return;
        }
        for (size_t i=0; i<lineTokLw.size(); i++)
            wholeLine << " " << lineTokLw.Item(i);
    }
    TokenF* old_parent = m_pLastParent;
    m_pLastParent = DoAddToken(tkType, typeName);
    m_pLastParent->m_ExtendsType = exTypeName;
    m_pLastParent->m_TokenAccess = taKind;
    m_pLastParent->m_IsAbstract = isAbstract;
    m_pLastParent->m_TypeDefinition = wholeLine;

    // Parse documentation
    DocBlock docs;
    GetDocBlock(docs, false, m_pLastParent->m_LineStart, true);
    if (docs.HasBrief() || docs.HasDescription())
        m_pLastParent->m_DocString << docs.GetBrief() + m_Briefend + docs.GetDescription();

    ParseDeclarations(true, true);

    if (m_LastTokenName.IsSameAs("contains"))
        ParseTypeBoundProcedures(wxEmptyString, false);

    if (m_pLastParent->m_DocString.IsEmpty())
    {
        docs.Clear();
        GetDocBlock(docs, true, m_pLastParent->m_LineStart, true); // look bellow the declaration for the non-doxyblocks documentation.
        if (docs.HasBrief() || docs.HasDescription())
            m_pLastParent->m_DocString << docs.GetBrief() + m_Briefend + docs.GetDescription();
    }

    m_pLastParent->AddLineEnd(m_Tokens.GetLineNumber());
    newToken = m_pLastParent;
    m_pLastParent = old_parent;
}

void ParserThreadF::CheckParseOneDeclaration(wxString& token, wxString& tok_low, wxString& next, wxString& next_low,
                                             bool& needDefault, TokensArrayF& newTokenArr, bool& hasFunctionInLine)
{
    hasFunctionInLine = false;
    if ( tok_low.IsSameAs("integer") || tok_low.IsSameAs("real")
            || tok_low.IsSameAs("doubleprecision") || tok_low.IsSameAs("character")
            || tok_low.IsSameAs("complex") || tok_low.IsSameAs("logical")
            || ( tok_low.IsSameAs("double") && next_low.IsSameAs("precision") )
            || ( tok_low.IsSameAs("type") && next_low.StartsWith("(") )
            || ( tok_low.IsSameAs("class") && next_low.StartsWith("(") )
            || tok_low.IsSameAs("enumerator")
       )
    {
        wxArrayString lineTok = m_Tokens.PeekTokensToEOL();
        if (lineTok.Index("function", false) == wxNOT_FOUND)
        {
            DocBlock docs;
            GetDocBlock(docs, false, m_Tokens.GetLineNumber(), false);

            bool found = ParseDeclarationsFirstPart(token, next);
            if (found)
            {
                int ntold = newTokenArr.size();
                ParseDeclarationsSecondPart(token, needDefault, newTokenArr);

                int ntnew = newTokenArr.size();
                if ((ntnew-ntold) > 0 && (docs.HasDescription() || docs.HasBrief()))
                {
                    for (int i=ntold; i<ntnew; i++)
                    {
                        if (newTokenArr.Item(i)->m_DocString.IsEmpty())
                            newTokenArr.Item(i)->m_DocString << docs.GetBrief() + m_Briefend + docs.GetDescription();
                    }
                }
            }
        }
        else
        {
            hasFunctionInLine = true;
        }
    }
}


void ParserThreadF::ParseDeclarations(bool breakAtEnd, bool breakAtContains)
{
    TokenAccessKind taDefKind = taPublic;
    TokensArrayF tokArr;
    while (1)
    {
        unsigned int ln_tokold = m_Tokens.GetLineNumber();
        wxString token = m_Tokens.GetToken();
        m_LastTokenName = token.Lower();
        wxString next = m_Tokens.PeekToken();
        wxString next_low = next.Lower();
        if (m_LastTokenName.IsEmpty())
        {
            break;
        }
        else if (m_LastTokenName.IsSameAs("include"))
        {
            HandleInclude();
        }
        else if (m_LastTokenName.GetChar(0) == '#')
        {
            HandlePPDirective(token);
        }
        else if (m_LastTokenName.IsSameAs("interface") && ln_tokold != m_Tokens.GetLineNumber())
        {
            HandleInterface(taDefKind);
        }
        else if (m_LastTokenName.IsSameAs("abstract") && next_low.IsSameAs("interface"))
        {
            token = m_Tokens.GetToken();
            m_LastTokenName = token.Lower();
            HandleInterface(taDefKind);
        }
        else if (breakAtEnd &&
                 ( ((m_Tokens.GetLineNumber() == m_Tokens.GetPeekedLineNumber()) && IsEnd(m_LastTokenName, next_low)) ||
                   ((m_Tokens.GetLineNumber() != m_Tokens.GetPeekedLineNumber()) && IsEnd(m_LastTokenName, "")) ))
        {
            m_Tokens.SkipToOneOfChars(";", true);
            break;
        }
        else if (breakAtContains && m_LastTokenName.IsSameAs("contains"))
        {
            m_Tokens.SkipToOneOfChars(";", true);
            break;
        }
        else if (m_LastTokenName.IsSameAs("private"))
        {
            bool changeDefault;
            int cal=0;
            wxArrayString pnList;
            HandleAccessList(taPrivate, changeDefault, cal, pnList);
            if (changeDefault)
            {
                taDefKind = taPrivate;
            }
        }
        else if (m_LastTokenName.IsSameAs("public"))
        {
            bool changeDefault;
            int cal=0;
            wxArrayString pnList;
            HandleAccessList(taPublic, changeDefault, cal, pnList);
            if (changeDefault)
            {
                taDefKind = taPublic;
            }
        }
        else if (m_LastTokenName.IsSameAs("block") && !next_low.IsSameAs("data"))
        {
            HandleBlockConstruct();
        }
        else if (m_LastTokenName.IsSameAs("procedure"))
        {
            ParseTypeBoundProcedures(m_LastTokenName, true, false);
        }

        wxArrayString lineTok = m_Tokens.PeekTokensToEOL();
        int funIdx = lineTok.Index("function", false);
        if (funIdx == wxNOT_FOUND || (funIdx > 2))
        {
            DocBlock docs;
            GetDocBlock(docs, false, m_Tokens.GetLineNumber(), false);

            bool found = ParseDeclarationsFirstPart(token, next);
            if (found)
            {
                bool nDef=true;
                TokensArrayF tokArrTmp;
                ParseDeclarationsSecondPart(token, nDef, tokArrTmp);
                if (nDef)
                {
                    for (size_t i=0; i<tokArrTmp.Count(); i++)
                    {
                        tokArr.Add(tokArrTmp.Item(i));
                    }
                }
                int tac = tokArrTmp.Count();
                if (tac > 0 && (docs.HasDescription() || docs.HasBrief()))
                {
                    for (int i=0; i<tac; i++)
                    {
                        if (tokArrTmp.Item(i)->m_DocString.IsEmpty())
                            tokArrTmp.Item(i)->m_DocString << docs.GetBrief() + m_Briefend + docs.GetDescription();
                    }
                }
            }
        }
    }
    for (size_t i=0; i<tokArr.Count(); i++)
    {
        tokArr.Item(i)->m_TokenAccess = taDefKind;
    }
    return;
}


bool ParserThreadF::ParseDeclarationsFirstPart(wxString& token, wxString& next)
{
    wxString tok_low = token.Lower();
    wxString next_low = next.Lower();
    bool found = false;

    if ( tok_low.IsSameAs("integer") || tok_low.IsSameAs("real")
            || tok_low.IsSameAs("doubleprecision") || tok_low.IsSameAs("character")
            || tok_low.IsSameAs("complex") || tok_low.IsSameAs("logical")
            || tok_low.IsSameAs("enumerator") )
    {
        if (next_low.StartsWith("("))  // 'integer(4)'
        {
            token.Append(next);
            m_Tokens.GetToken();
        }
        else if (next_low.StartsWith("*")) // 'integer*4'
        {
            token.Append(m_Tokens.GetToken());
            token.Append(m_Tokens.GetTokenSameFortranLine());
        }
        found = true;
    }
    else if (tok_low.IsSameAs("double"))
    {
        if (next_low.IsSameAs("precision"))
        {
            found = true;
            token.Append(" ");
            token.Append(next);
            m_Tokens.GetToken();
            next = m_Tokens.PeekToken();
            if (next.StartsWith("("))
            {
                token.Append(next);
                m_Tokens.GetToken();
            }
        }
    }
    else if (tok_low.IsSameAs("type") || tok_low.IsSameAs("class"))
    {
        if (next_low.StartsWith("("))
        {
            if (next_low.EndsWith(")"))
            {
                wxString token_s = m_Tokens.GetToken();
                token_s = token_s.Mid(1,token_s.Len()-2).Trim().Trim(false);
                token.Append("(");
                token.Append(token_s);
                token.Append(")");
                found = true;
            }
            else
            {
                //something wrong
                m_Tokens.SkipToOneOfChars(";", true);
            }
        }
        else if (tok_low.IsSameAs("type")  && !next_low.IsSameAs("is"))
        {
            // we found type definition
            HandleType();
        }
    }
    return found;
}


void ParserThreadF::ParseDeclarationsSecondPart(wxString& token, bool& needDefault, TokensArrayF& newTokenArr)
{
    needDefault = true;
    TokenAccessKind taKind = taPublic;
    wxString defT = token;
    wxString dims;
    wxArrayString linesArr;
    m_Tokens.SetDetailedParsing(true);
    wxArrayString lineTok = m_Tokens.GetTokensToEOL(&linesArr);
    m_Tokens.SetDetailedParsing(false);
    int idx = lineTok.Index("::");
    if (idx != wxNOT_FOUND)
    {
        for (int i=0; i<idx; i++)
        {
            if (lineTok.Item(i).IsSameAs(","))
                continue;

            if (!lineTok.Item(i).StartsWith("("))
            {
                defT.Append(", ");
            }
            defT.Append(lineTok.Item(i));

            wxString tokLw = lineTok.Item(i).Lower();
            if (tokLw.IsSameAs("private"))
            {
                taKind = taPrivate;
                needDefault = false;
            }
            else if (tokLw.IsSameAs("protected"))
            {
                taKind = taProtected;
                needDefault = false;
            }
            else if (tokLw.IsSameAs("public"))
            {
                taKind = taPublic;
                needDefault = false;
            }

            if (tokLw.IsSameAs("dimension") && lineTok.Item(i+1).StartsWith("("))
            {
                dims.Append(lineTok.Item(i+1));
            }
        }
    }
    else // "::" not found
    {
        if (lineTok.GetCount() > 0 && lineTok.Item(0).IsSameAs(","))
        {
            // it is unfinished declaration (e.g. "real, pointer, ")
            return;
        }
        idx = -1;
    }

    wxArrayString varNames;
    wxArrayString varArgs;
    wxArrayString varComs;
    wxArrayString varDims;
    size_t lineTokCount = lineTok.GetCount();
    for (size_t i=idx+1; i<lineTokCount; )
    {
        wxString var1= lineTok.Item(i);
        if (var1.IsSameAs(","))
        {
            i++;
            continue;
        }
        wxString arg1;
        wxString dim1;
        while (i+1 < lineTokCount)
        {
            wxString s = lineTok.Item(i+1);
            if ((s.StartsWith("(") && s.EndsWith(")")) || (s.StartsWith("[") && s.EndsWith("]")))
            {
                arg1 << s;
                i++;
            }
            else
                break;
        }
        dim1 << arg1;
        if (i+1 < lineTokCount && (lineTok.Item(i+1).IsSameAs("=>") || lineTok.Item(i+1).IsSameAs("=")
                                         || lineTok.Item(i+1).IsSameAs("*")) )
        {
            i += 1;
            for (; i<lineTokCount; i++)
            {
                if (lineTok.Item(i).IsSameAs(","))
                    break;
                else
                    arg1 << lineTok.Item(i);
            }
            if(i >= lineTokCount)
            {
                i = lineTokCount - 1;
            }
        }
        wxString comStr = linesArr.Item(i).AfterFirst('!');
        if (comStr.StartsWith("<") || comStr.StartsWith(">"))
            comStr = comStr.Mid(1);
        comStr = comStr.Trim(true).Trim(false);

        varNames.Add(var1);
        varArgs.Add(arg1);
        varComs.Add(comStr);
        if (dim1.IsEmpty())
            varDims.Add(dims);
        else
            varDims.Add(dim1);
        i++;
    }
    size_t varNamesCount = varNames.GetCount();
    for (size_t i=0; i<varNamesCount; i++)
    {
        TokenF* tok = DoAddToken(tkVariable, varNames[i], varArgs[i], defT);
        tok->m_DocString = varComs.Item(i);
        tok->m_TokenAccess = taKind;
        tok->AddLineEnd(tok->m_LineStart);
        if (varDims.Item(i).IsEmpty())
        {
            tok->AddPartFirst(token);
        }
        else
        {
            wxString tokStr = token + ", " + varDims.Item(i);
            tok->AddPartFirst(tokStr);
        }
        newTokenArr.Add(tok);
    }
    return;
}


void ParserThreadF::HandleSubmoduleProcedure()
{
    wxString token;
    token = m_Tokens.GetTokenSameFortranLine();

    TokenF* old_parent = m_pLastParent;
    m_pLastParent = DoAddToken(tkProcedure, token);

    GoThroughBody();

    m_pLastParent->AddLineEnd(m_Tokens.GetLineNumber());
    m_pLastParent = old_parent;
}


void ParserThreadF::HandleFunction(TokenKindF kind, TokenAccessKind taKind)
{
    m_ParentDocs.Clear();
    unsigned int ln = m_Tokens.GetLineNumber();
    GetDocBlock(m_ParentDocs, false, ln, true);

    wxString token;
    token = m_Tokens.GetTokenSameFortranLine();

    if (token.empty() && kind == tkProgram)
        token = "unnamed";

    unsigned int defStartLine = m_Tokens.GetLineNumber();
    TokenF* old_parent = m_pLastParent;
    wxString args = m_Tokens.PeekTokenSameFortranLine();

    if (args.empty() || !args(0,1).Matches("("))
        args = "()";
    else
        args = m_Tokens.GetTokenSameFortranLine();
    m_pLastParent = DoAddToken(kind, token, args, defStartLine);
    m_pLastParent->m_TokenAccess = taKind;
    if (m_ParentDocs.HasBrief() || m_ParentDocs.HasDescription())
        m_pLastParent->m_DocString << m_ParentDocs.GetBrief() + m_Briefend + m_ParentDocs.GetDescription();

    if (kind == tkFunction)
    {
        wxString funkLine = m_Tokens.GetLineFortran();
        wxString funkLineLow = funkLine.Lower();
        int i_fun = funkLineLow.Find("function");
        if (i_fun != wxNOT_FOUND)
        {
            m_pLastParent->AddPartFirst(funkLine.Mid(0,i_fun).Trim().Trim(false));
            wxString secPart = funkLine.Mid(i_fun+8);
            i_fun = secPart.Find(')');
            if (i_fun != wxNOT_FOUND && (int)secPart.Len() > i_fun+1)
            {
                wxString lastPart = secPart.Mid(i_fun+1).Trim().Trim(false);
                m_pLastParent->AddPartLast(lastPart);
                wxString lastPartLow = lastPart.Lower();
                i_fun = lastPartLow.Find("result");
                if (i_fun != wxNOT_FOUND)
                {
                    wxString el = lastPartLow.Mid(i_fun+6);
                    int is = el.Find('(');
                    int ie = el.Find(')');
                    if (is != wxNOT_FOUND && ie != wxNOT_FOUND && ie > is)
                        m_pLastParent->AddResultVariable(el.Mid(is+1,ie-is-1).Trim().Trim(false));
                }
            }
        }
    }
    GoThroughBody();
    m_pLastParent->AddLineEnd(m_Tokens.GetLineNumber());
    AddParamDocs(m_pLastParent, m_ParentDocs);

    if (m_pLastParent->m_DocString.IsEmpty())
    {
        ln = m_pLastParent->m_LineStart + m_pLastParent->m_DefinitionLength - 1;
        DocBlock doc;
        GetDocBlock(doc, true, ln, true); // look bellow the declaration for the non-doxyblocks documentation.
        if (doc.HasBrief() || doc.HasDescription())
            m_pLastParent->m_DocString << doc.GetBrief() + m_Briefend + doc.GetDescription();
    }

    m_pLastParent = old_parent;
}


void ParserThreadF::HandleBlockConstruct()
{
    unsigned int defStartLine = m_Tokens.GetLineNumber();
    TokenF* old_parent = m_pLastParent;
    m_NumberOfBlockConstruct += 1;
    wxString name = "%%tkBlockConstruct";
    name << wxString::Format("%.3d", m_NumberOfBlockConstruct);
    m_pLastParent = DoAddToken(tkBlockConstruct, name, wxEmptyString, defStartLine);

    GoThroughBody();
    m_pLastParent->AddLineEnd(m_Tokens.GetLineNumber());
    m_pLastParent = old_parent;
}


void ParserThreadF::HandleAssociateConstruct()
{
    TokenF* old_parent = m_pLastParent;
    wxString args = m_Tokens.PeekTokenSameFortranLine();

    if (args.empty() || !args(0,1).Matches("("))
        args = "()";
    else
        args = m_Tokens.GetTokenSameFortranLine();
    m_pLastParent = DoAddToken(tkAssociateConstruct, wxEmptyString, args, wxEmptyString);

    GoThroughBody();
    m_pLastParent->AddLineEnd(m_Tokens.GetLineNumber());
    m_pLastParent = old_parent;
}


void ParserThreadF::HandleSelectTypeConstruct()
{
    TokenF* old_parent = m_pLastParent;
    wxString args = m_Tokens.PeekTokenSameFortranLine();
    if (args.empty() || !args(0,1).Matches("("))
        args = "()";
    else
        args = m_Tokens.GetTokenSameFortranLine();

    while (1)
    {
        wxString token = m_Tokens.GetToken();
        if (token.IsEmpty())
            break;

        wxString tok_low = token.Lower();
        wxString next    = m_Tokens.PeekToken();
        wxString nex_low = next.Lower();

        if ( (tok_low.Matches("end") && nex_low.Matches("select")) || tok_low.Matches("endselect") )
        {
            m_Tokens.SkipToOneOfChars(";", true);
            break;
        }
        else if ( (tok_low.Matches("type") && nex_low.Matches("is")) ||
                  (tok_low.Matches("class") && nex_low.Matches("is")) )
        {
            wxString defstr = tok_low;
            if (m_Tokens.GetToken().IsEmpty())
                break;
            next = m_Tokens.PeekToken();
            nex_low = next.Lower();
            if (nex_low.StartsWith("("))
                defstr << nex_low;

            m_pLastParent = DoAddToken(tkSelectTypeChild, wxEmptyString, args, defstr);
            GoThroughBody();
            m_pLastParent->AddLineEnd(m_Tokens.GetLineNumber());
            m_pLastParent = old_parent;
        }
        else if (tok_low.Matches("class") && nex_low.Matches("default"))
        {
            m_pLastParent = DoAddToken(tkSelectTypeDefault, wxEmptyString, args, wxEmptyString);
            GoThroughBody();
            m_pLastParent->AddLineEnd(m_Tokens.GetLineNumber());
            m_pLastParent = old_parent;
        }
        else if (tok_low.Matches("include"))
        {
            HandleInclude();
        }
        else if (tok_low.GetChar(0) == '#')
        {
            HandlePPDirective(token);
        }
    }
    m_pLastParent = old_parent;
}


void ParserThreadF::HandleSelectCaseConstruct()
{
    // we are not interesting in SelectCase, but we need to catch EndSelect
    GoThroughBody();
    m_Tokens.GetToken();
    m_Tokens.SkipToOneOfChars(";", true);
}


void ParserThreadF::HandleInterface(TokenAccessKind taKind)
{
    TokenF* tokTmp = 0;
    bool isGeneric;
    HandleInterface(taKind, tokTmp, isGeneric);
}

void ParserThreadF::HandleInterface(TokenAccessKind taKind, TokenF* &tokNew, bool &isGeneric)
{
    isGeneric = false;
    TokenF* old_parent = m_pLastParent;
    unsigned int defStartLine = m_Tokens.GetLineNumber();
    wxArrayString curLineArr = m_Tokens.GetTokensToEOL();
    wxString name;
    TokenKindF tokKin;
    if (curLineArr.GetCount() > 0)
    {
        wxString low = curLineArr.Item(0).Lower();
        if (low.IsSameAs("operator"))
        {
            name.Append("%%");
            name.Append(curLineArr.Item(0));
            for (unsigned int i=1; i<curLineArr.GetCount(); i++)
            {
                name.Append(" ");
                name.Append(curLineArr.Item(i));
            }
            m_InterfaceOperator += 1;
            if (m_InterfaceOperator > 1)
            {
                name << " #" << m_InterfaceOperator;
            }
        }
        else if (low.IsSameAs("assignment"))
        {
            name.Append("%%");
            name.Append(curLineArr.Item(0));
            for (unsigned int i=1; i<curLineArr.GetCount(); i++)
            {
                name.Append(" ");
                name.Append(curLineArr.Item(i));
            }
            m_InterfaceAssignment += 1;
            if (m_InterfaceAssignment > 1)
            {
                name << " #" << m_InterfaceAssignment;
            }
        }
        else if (low.IsSameAs("read"))
        {
            name.Append("%%");
            name.Append(curLineArr.Item(0));
            for (unsigned int i=1; i<curLineArr.GetCount(); i++)
            {
                name.Append(" ");
                name.Append(curLineArr.Item(i));
            }
            m_InterfaceRead += 1;
            if (m_InterfaceRead > 1)
            {
                name << " #" << m_InterfaceRead;
            }
        }
        else if (low.IsSameAs("write"))
        {
            name.Append("%%");
            name.Append(curLineArr.Item(0));
            for (unsigned int i=1; i<curLineArr.GetCount(); i++)
            {
                name.Append(" ");
                name.Append(curLineArr.Item(i));
            }
            m_InterfaceWrite += 1;
            if (m_InterfaceWrite > 1)
            {
                name << " #" << m_InterfaceWrite;
            }
        }
        else
        {
            // generic procedure name
            name.Append(curLineArr.Item(0));
            for (unsigned int i=1; i<curLineArr.GetCount(); i++)
            {
                name.Append(" ");
                name.Append(curLineArr.Item(i));
            }
            isGeneric = true;
        }
        tokKin = tkInterface;
    }
    else
    {
        tokKin = tkInterfaceExplicit;
    }

    m_pLastParent = DoAddToken(tokKin, name, wxEmptyString, defStartLine);
    m_pLastParent->m_TokenAccess = taKind;
    tokNew = m_pLastParent;

    GoThroughBody();

    if (tokKin == tkInterfaceExplicit)
    {
        TokensArrayF* toks = &m_pLastParent->m_Children;
        if (toks)
        {
            for (size_t i=0; i<toks->GetCount(); i++)
            {
                toks->Item(i)->m_TokenAccess = taKind;
            }
        }
    }

    m_pLastParent->AddLineEnd(m_Tokens.GetLineNumber());

    if (isGeneric)
    {
        // Parse documentation
        DocBlock docs;
        GetDocBlock(docs, false, m_pLastParent->m_LineStart, true);
        if (docs.HasBrief() || docs.HasDescription())
            m_pLastParent->m_DocString << docs.GetBrief() + m_Briefend + docs.GetDescription();
        else
        {
            docs.Clear();
            GetDocBlock(docs, true, m_pLastParent->m_LineStart, true); // look bellow the declaration for the non-doxyblocks documentation.
            if (docs.HasBrief() || docs.HasDescription())
                m_pLastParent->m_DocString << docs.GetBrief() + m_Briefend + docs.GetDescription();
        }
    }

    m_pLastParent = old_parent;
}

void ParserThreadF::HandleBlockData()
{
    TokenF* old_parent = m_pLastParent;
    wxString token = m_Tokens.GetTokenSameLine();
    if (token.IsEmpty())
        m_pLastParent = DoAddToken(tkBlockData, "BD_unnamed");
    else
        m_pLastParent = DoAddToken(tkBlockData, token);

    while (1)
    {
        token = m_Tokens.GetToken();
        if (token.IsEmpty())
            break;
        wxString tok_low = token.Lower();

        wxString next = m_Tokens.PeekToken();
        wxString nex_low = next.Lower();
        if ( ((m_Tokens.GetLineNumber() == m_Tokens.GetPeekedLineNumber()) && IsEnd(tok_low, nex_low)) ||
                ((m_Tokens.GetLineNumber() != m_Tokens.GetPeekedLineNumber()) && IsEnd(tok_low, "")) )
        {
            m_Tokens.SkipToOneOfChars(";", true);
            break;
        }
        else if (tok_low.Matches("include"))
        {
            HandleInclude();
        }
        else if (tok_low.GetChar(0) == '#')
        {
            HandlePPDirective(token);
        }
    }
    m_pLastParent->AddLineEnd(m_Tokens.GetLineNumber());
    m_pLastParent = old_parent;
}

void ParserThreadF::HandleInclude()
{
    wxString token = m_Tokens.GetTokenSameFortranLine();

    if (token.IsEmpty())
        return; // something wrong
    else if ((token.StartsWith("\'") || token.StartsWith("\"") || token.StartsWith("<")) &&
             (token.EndsWith("\'") || token.EndsWith("\"")  || token.EndsWith(">")))
    {
        token = token.Mid(1,token.Len()-2).Trim().Trim(false);
        DoAddToken(tkInclude, token);
        m_IncludeList.Add(token);
    }
    else if (token.IsSameAs("<"))
    {
        // Handle #include <filename.fpp>
        token = m_Tokens.GetTokenSameLine();
        if (m_Tokens.PeekTokenSameFortranLine().IsSameAs("."))
        {
            wxString point = m_Tokens.GetToken();
            token.Append(point + m_Tokens.GetTokenSameLine());
        }
        DoAddToken(tkInclude, token);
        m_IncludeList.Add(token);
        m_Tokens.SkipToEOL();
    }
}

void ParserThreadF::HandlePPDirective(wxString& token)
{
    m_Tokens.SkipToEOL();  // Just skip to the end of line. If we have # here, it means that
                           // "interpret preprocessor directives" is disabled in the settings.

//    if (token.Matches(_T("#define")))
//        HandlePPDefine();
//    else if (token.Matches(_T("#undefine")))
//        HandlePPUndefine();
//    else if (token.Matches(_T("#if")) || token.Matches(_T("#ifdef")) || token.Matches(_T("#ifndef")))
//        HandlePPIfdef(token);
//    else if (token.Matches(_T("#endif")) || token.Matches(_T("#else")) || token.Matches(_T("#elif")))
//        HandlePPIfdef(token);
//    else if (token.Matches(_T("#include")))
//        HandleInclude();
//    else
//        m_Tokens.SkipToEOL();
}

void ParserThreadF::HandlePPDefine()
{
    // Handle #define ABC
    // More sophisticated cases isn't interpreted
    wxString token = m_Tokens.GetTokenSameLine();

    if (token.IsEmpty())
        return; // something wrong

    TokenF* newToken = new TokenF;
    newToken->m_TokenKind = tkMacroDefine;
    newToken->m_Filename = m_Tokens.GetFilename();
    newToken->m_DisplayName = token;
    newToken->m_LineStart = m_Tokens.GetLineNumber();
    newToken->m_LineEnd = 0;

    m_pPPDefineTokens->Add(newToken);
    m_Tokens.SkipToEOL();
}

void ParserThreadF::HandlePPUndefine()
{
    // Handle #undefine ABC or #undef ABC
    wxString token = m_Tokens.GetTokenSameLine();

    if (token.IsEmpty())
        return; // something wrong

    bool found = false;
    for (size_t i=0; i<m_pPPDefineTokens->size(); ++i)
    {
        if (m_pPPDefineTokens->Item(i)->m_DisplayName.IsSameAs(token))
        {
            m_pPPDefineTokens->Item(i)->m_LineEnd = m_Tokens.GetLineNumber();
            found = true;
            break;
        }
    }
    m_Tokens.SkipToEOL();

    if (!found)
    {
        size_t nDef = m_ProjPPDefineTokens.size();
        for (size_t i=0; i<nDef; ++i)
        {
            if (m_ProjPPDefineTokens[i].IsSameAs(token))
            {
                m_ProjPPDefineTokens[i] = "+++---"; // Just write something. It should work as erase.
                break;
            }
        }
    }
}

void ParserThreadF::HandlePPIfdef(wxString& ifToken)
{
    // Handle #ifdef construct in the simplest cases.
    if (ifToken.IsSameAs("#ifdef") || ifToken.IsSameAs("#ifndef"))
    {
        m_inIfdef += 1;
        wxString token = m_Tokens.GetTokenSameLine();
        if (token.IsEmpty())
            return;

        bool hasDef = HasDefine(token, m_Tokens.GetLineNumber());
        if ((ifToken.IsSameAs("#ifdef") && hasDef) || (ifToken.IsSameAs("#ifndef") && !hasDef))
        {
            // Will be interpreted until correcponding #elif, #else or #endif
        }
        else
        {
            // Skip to the corresponding #elif #else or #endif
            m_Tokens.SkipToEOL();
            wxString lastTok;
            SkipPPIfdef(lastTok);
            if (lastTok.empty() || lastTok.IsSameAs("#endif"))
                m_inIfdef -= 1;
            else if (lastTok.IsSameAs("#elif"))
                HandlePPIfdef(lastTok);
        }
    }
    else if (ifToken.IsSameAs("#if") || ifToken.IsSameAs("#elif"))
    {
        // More clever interpreter is required.
        // For now take as if defined in case of #if
        if (ifToken.IsSameAs("#if"))
        {
            m_inIfdef += 1;
        }
        else
        {
            // Skip to the corresponding #endif
            wxString lastTok;
            while (true)
            {
                m_Tokens.SkipToEOL();
                SkipPPIfdef(lastTok);
                if (lastTok.empty() || lastTok.IsSameAs("#endif"))
                    break;
            }
            m_inIfdef -= 1;
        }
    }
    else if (ifToken.IsSameAs("#else"))
    {
        // Skip to the corresponding #endif
        m_Tokens.SkipToEOL();
        wxString lastTok;
        SkipPPIfdef(lastTok);
        // here should be lastTok==#endif
        m_inIfdef -= 1;
    }
    else // #endif
        m_inIfdef -= 1;

    m_Tokens.SkipToEOL();
}

bool ParserThreadF::HasDefine(const wxString& token, unsigned int lnum)
{
    size_t nDef = m_pPPDefineTokens->size();
    for (size_t i=0; i<nDef; ++i)
    {
        if (m_pPPDefineTokens->Item(i)->m_DisplayName.IsSameAs(token) &&
            (m_pPPDefineTokens->Item(i)->m_LineEnd == 0 ||
             m_pPPDefineTokens->Item(i)->m_LineEnd > lnum))
        {
            return true;
        }
    }
    return false;
}

bool ParserThreadF::HasProjPPDefineTokens(const wxString& token)
{
    size_t nDef = m_ProjPPDefineTokens.size();
    for (size_t i=0; i<nDef; ++i)
    {
        if (m_ProjPPDefineTokens[i].IsSameAs(token))
        {
            return true;
        }
    }
    return false;
}

void ParserThreadF::SkipPPIfdef(wxString& tokenAtEnd)
{
    // Skip to the next corresponding #elif, #else or #endif
    tokenAtEnd.clear();
    int start_inIfdef = m_inIfdef;
    while (true)
    {
        wxString token = m_Tokens.GetToken();
        if (token.IsEmpty())
            break;

        if (token.StartsWith("#"))
        {
            if (token.IsSameAs("#ifdef") || token.IsSameAs("#ifndef"))
            {
                m_inIfdef += 1;
                m_Tokens.SkipToEOL();
            }
            else if (m_inIfdef > start_inIfdef && token.IsSameAs("#endif"))
            {
                m_inIfdef -= 1;
                m_Tokens.SkipToEOL();
            }
            else if (token.IsSameAs("#define"))
                continue; //HandlePPDefine();
            else if (token.IsSameAs("#undefine") || token.IsSameAs("#undef"))
                continue; //HandlePPUndefine();
            else if (m_inIfdef == start_inIfdef)
            {
                tokenAtEnd = token;
                break;
            }
        }
        else
            m_Tokens.SkipToEOL();
    }
    m_Tokens.SkipToEOL();
}

void ParserThreadF::HandleAccessList(TokenAccessKind taKind, bool& changeDefault, int& countAccess, wxArrayString& nameList)
{
    changeDefault = false;
    wxString curLine = m_Tokens.GetLineFortran().Lower().Trim(false);
    int ipp = wxNOT_FOUND;
    if (taKind == taPrivate)
        ipp = curLine.Find("private");
    else if (taKind == taPublic)
        ipp = curLine.Find("public");
    else if (taKind == taProtected)
        ipp = curLine.Find("protected");

    if (ipp == wxNOT_FOUND)
        return; // something is wrong
    else if (ipp != 0)
        return; // here private (public) is used as an attribute.

    unsigned int defStartLine = m_Tokens.GetLineNumber();
    wxArrayString curLineArr = m_Tokens.GetTokensToEOL();
    if (curLineArr.GetCount() == 0)
    {
        changeDefault = true;
        return;
    }
    countAccess++;
    wxString name;
    name = "AccessList";
    if (countAccess > 1)
        name << " " << countAccess;

    TokenF* token;
    token = DoAddToken(tkAccessList, name, wxEmptyString, defStartLine);
    token->AddLineEnd(m_Tokens.GetLineNumber());
    token->m_TokenAccess = taKind;

    nameList.Add(token->m_Name);
    size_t i=0;
    if (curLineArr.Item(0).IsSameAs("::"))
        i=1;
    for (; i<curLineArr.GetCount(); i++)
    {
        nameList.Add(curLineArr.Item(i).Lower());
    }
}

void ParserThreadF::GoThroughBody()
{
    wxString tok_low;

    while (1)
    {
        unsigned int ln_tokold = m_Tokens.GetLineNumber();

        wxString token = m_Tokens.GetToken();
        if (token.IsEmpty())
            break;
        else if (token.Matches("::"))
        {
            m_Tokens.SkipToOneOfChars(";", true);
            continue;
        }
        tok_low = token.Lower();
        unsigned int tok_ln = m_Tokens.GetLineNumber();

        wxString next = m_Tokens.PeekToken();
        wxString nex_low = next.Lower();
        unsigned int nex_ln = m_Tokens.GetPeekedLineNumber();
        if (tok_ln != nex_ln)
            nex_low = wxEmptyString;

        if (IsEnd(tok_low, nex_low))
        {
            m_Tokens.SkipToOneOfChars(";", true);
            break;
        }
        else if ( (tok_low.Matches("end") && nex_low.Matches("select")) || tok_low.Matches("endselect") ||
                  (tok_low.Matches("type") && nex_low.Matches("is")) ||
                  (tok_low.Matches("class") && nex_low.Matches("is")) ||
                  (tok_low.Matches("class") && nex_low.Matches("default")) )
        {
            m_Tokens.UngetToken();
            break;
        }
        else if (tok_low.Matches("type") && !nex_low(0,1).Matches("(") && !nex_low.Matches("is")
                 && ln_tokold != tok_ln)
        {
            HandleType();
        }
        else if (tok_low.Matches("subroutine"))
        {
            HandleFunction(tkSubroutine);
        }
        else if (tok_low.Matches("function"))
        {
            HandleFunction(tkFunction);
        }
        else if (tok_low.Matches("use"))
        {
            HandleUse();
        }
        else if (tok_low.Matches("interface") && ln_tokold != tok_ln)
        {
            HandleInterface();
        }
        else if (tok_low.Matches("abstract") && nex_low.Matches("interface"))
        {
            token = m_Tokens.GetToken();
            HandleInterface();
        }
        else if (tok_low.Matches("include"))
        {
            HandleInclude();
        }
        else if (tok_low.GetChar(0) == '#')
        {
            HandlePPDirective(token);
        }
        else if (tok_low.Matches("procedure") && m_pLastParent->m_TokenKind == tkInterface)
        {
            HandleProcedureList();
        }
        else if (tok_low.Matches("module") && (nex_low.Matches("subroutine") || nex_low.Matches("function")))
        {
            token = m_Tokens.GetToken();
            tok_low = token.Lower();
            if (tok_low.Matches("subroutine"))
            {
                HandleFunction(tkSubroutine);
            }
            else if (tok_low.Matches("function"))
            {
                HandleFunction(tkFunction);
            }
        }
        else if (tok_low.Matches("block"))
        {
            if (nex_low.Matches("data"))
            {
                token = m_Tokens.GetToken();
                tok_low = token.Lower();
                next = m_Tokens.PeekToken();
                nex_low = next.Lower();
                HandleBlockData();
            }
            else if (tok_ln != nex_ln || nex_low.Matches(";"))
            {
                HandleBlockConstruct();
            }
        }
        else if (tok_low.Matches("blockdata"))
        {
            HandleBlockData();
        }
        else if (tok_low.Matches("associate"))
        {
            HandleAssociateConstruct();
        }
        else if (tok_low.Matches("select") && nex_low.Matches("type"))
        {
            m_Tokens.GetToken();
            HandleSelectTypeConstruct();
        }
        else if (tok_low.Matches("selecttype"))
        {
            HandleSelectTypeConstruct();
        }
        else if ((tok_low.Matches("select") && nex_low.Matches("case")) ||
                 tok_low.Matches("selectcase"))
        {
            HandleSelectCaseConstruct();
        }
        else if ( tok_low.Matches("select") && nex_low.Matches("rank") )
        {
            HandleSelectCaseConstruct();
        }
        else if (tok_low.Matches("procedure") && nex_low(0,1).Matches("("))
        {
            ParseTypeBoundProcedures(token, true, false);
        }
        else if (tok_low.Matches("!bindto"))
        {
            HandleBindTo();
        }
        else if (tok_ln == nex_ln)
        {
            bool needDefault = true;
            bool hasFunctionInLine = false;
            TokensArrayF tokTmpArr;
            if (ln_tokold != tok_ln) // declaration can start only from a new line
            {
                CheckParseOneDeclaration(token, tok_low, next, nex_low, needDefault, tokTmpArr, hasFunctionInLine);
            }
            if (tokTmpArr.size() == 0 && m_pLastParent && !hasFunctionInLine)
            {
                CheckParseCallProcedure(token, tok_low, next);
            }
        }
    }
}

void ParserThreadF::HandleProcedureList()
{
    unsigned int lineNum = m_Tokens.GetLineNumber();
    wxArrayString curLineArr = m_Tokens.GetTokensToEOL();

    for (unsigned int i=0; i<curLineArr.GetCount(); i++)
    {
        if (curLineArr.Item(i).IsSameAs("::"))
            continue;
        DoAddToken(tkOther, curLineArr.Item(i), wxEmptyString, lineNum);
    }
}

void ParserThreadF::ParseTypeBoundProcedures(const wxString& firstWord, bool breakAtEOL, bool passIn)
{
    TokenAccessKind defAccKind = taPublic;
    bool firstTime = true;
    int nOperator = 0;
    int nAssignment = 0;
    while (1)
    {
        bool pass = passIn;
        TokenAccessKind tokAccK = defAccKind;
        wxString firstTokenLw;
        if (firstTime && !firstWord.IsEmpty())
        {
            firstTokenLw = firstWord.Lower();
            firstTime = false;
        }
        else
            firstTokenLw = m_Tokens.GetToken().Lower();

        if (firstTokenLw.IsEmpty())
            break;
        wxString defString = firstTokenLw;
        unsigned int lineNum = m_Tokens.GetLineNumber();
        wxArrayString linesArr;
        wxArrayString curLineArr = m_Tokens.GetTokensToEOL(&linesArr);
        bool isGen = firstTokenLw.IsSameAs("generic");
        bool isProc = firstTokenLw.IsSameAs("procedure");
        if (curLineArr.Count() > 0 && (isProc || isGen) ) // &&
            // !curLineArr.Item(0).StartsWith("(") ) // not interface-name
        {
            wxString interfaceName;
            if (isProc)
            {
                wxString strInter = curLineArr.Item(0);
                int idx_a = strInter.Find(')',true);
                int idx_b = strInter.Find('(');
                if (idx_a != wxNOT_FOUND && idx_b != wxNOT_FOUND && idx_a > idx_b+1)
                {
                    interfaceName = strInter.Mid(idx_b+1,idx_a-idx_b-1);
                    defString << strInter;
                }
            }
            wxString passArg;
            int idx = curLineArr.Index("::");
            int startList;
            if (idx != wxNOT_FOUND)
            {
                int startIdx = interfaceName.empty() ? 0 : 1;
                for (int i=startIdx; i<idx; i++)
                {
                    defString << ", " << curLineArr.Item(i).Lower();
                }

                for (int i=0; i<idx; i++)
                {
                    wxString tok = curLineArr.Item(i).Lower();
                    if (tok.IsSameAs("nopass"))
                    {
                        pass = false;
                    }
                    else if (tok.IsSameAs("pass"))
                    {
                        if (i < idx-1)
                        {
                            wxString strArg = curLineArr.Item(i+1);
                            int idx_a = strArg.Find(')',true);
                            int idx_b = strArg.Find('(');
                            if (idx_a != wxNOT_FOUND && idx_b != wxNOT_FOUND && idx_a > idx_b+1)
                            {
                                passArg = strArg.Mid(idx_b+1,idx_a-idx_b-1);
                            }
                        }
                    }
                    else if (tok.IsSameAs("private"))
                    {
                        tokAccK = taPrivate;
                    }
                    else if (tok.IsSameAs("public"))
                    {
                        tokAccK = taPublic;
                    }
                }
                startList = idx + 1;
            }
            else
            {
                startList = 0;
            }
            int countArr = curLineArr.GetCount();
            int ic=startList;
            if (!isGen)
            {
                while (ic < countArr)
                {
                    // Read docs
                    wxString comStr = linesArr.Item(ic).AfterFirst('!');
                    if (!comStr.IsEmpty())
                    {
                        if (comStr.StartsWith("<") || comStr.StartsWith(">"))
                            comStr = comStr.Mid(1);
                        comStr = comStr.Trim(true).Trim(false);
                    }

                    wxString bindName = curLineArr.Item(ic);
                    wxString procName;
                    if (ic+2 < countArr)
                    {
                        if (curLineArr.Item(ic+1).IsSameAs("=>"))
                        {
                            ic += 2;
                            procName = curLineArr.Item(ic);
                            if (ic+1 < countArr && curLineArr.Item(ic+1).StartsWith("("))
                                ic++;
                        }
                    }
                    ic++;
                    TokenF* token = DoAddToken(tkProcedure, bindName.Lower(), wxEmptyString, lineNum);
                    token->m_DisplayName = bindName;
                    token->m_Pass = pass;
                    token->m_Args = passArg;
                    if (interfaceName.IsEmpty())
                    {
                        token->m_PartLast = procName.Lower();
                        token->m_IsAbstract = false;
                    }
                    else
                    {
                        token->m_PartLast = interfaceName.Lower();
                        token->m_IsAbstract = true;
                    }
                    token->AddLineEnd(m_Tokens.GetLineNumber());
                    token->m_TokenAccess = tokAccK;
                    token->m_TypeDefinition = defString;
                    token->m_DocString = comStr;
                }
            }
            else //isGen
            {
                while (ic < countArr-2)
                {
                    wxString curNam;
                    wxString curNamLw = curLineArr.Item(ic).Lower();
                    if (curNamLw.IsSameAs("operator") || curNamLw.IsSameAs("assignment"))
                    {
                        curNam.Append("%%");
                        curNam.Append(curLineArr.Item(ic));
                        if (curLineArr.Item(ic+1).StartsWith("("))
                        {
                            ic++;
                            curNam.Append(curLineArr.Item(ic));
                        }
                        if (curNamLw.IsSameAs("operator"))
                        {
                            nOperator += 1;
                            curNam << " #" << nOperator;
                        }
                        else
                        {
                            nAssignment += 1;
                            curNam << " #" << nAssignment;
                        }
                        TokenF* token = DoAddToken(tkInterface, curNam, wxEmptyString, lineNum);
                        token->AddLineEnd(m_Tokens.GetLineNumber());
                    }
                    else if (curNamLw.IsSameAs("read") || curNamLw.IsSameAs("write"))
                    {
                        curNam.Append("%%");
                        curNam.Append(curLineArr.Item(ic));
                        if (curLineArr.Item(ic+1).StartsWith("("))
                        {
                            ic++;
                            curNam.Append(curLineArr.Item(ic));
                        }
                        TokenF* token = DoAddToken(tkInterface, curNam, wxEmptyString, lineNum);
                        token->AddLineEnd(m_Tokens.GetLineNumber());
                    }
                    else if (curLineArr.Item(ic+1).IsSameAs("=>"))
                    {
                        wxString bindName = curLineArr.Item(ic);
                        TokenF* token = DoAddToken(tkInterface, bindName.Lower(), wxEmptyString, lineNum);
                        token->m_DisplayName = bindName;
                        ic += 2;
                        wxString specNames;
                        for (; ic < countArr; ic++)
                        {
                            specNames << curLineArr.Item(ic) << " ";
                        }
                        token->m_PartLast = specNames.Trim();
                        token->AddLineEnd(m_Tokens.GetLineNumber());
                        token->m_TokenAccess = tokAccK;
                    }
                    ic++;
                }
            }
        }
        else if ( (curLineArr.Count() == 0 && IsEnd(firstTokenLw, wxEmptyString)) ||
                  (curLineArr.Count() >= 1 && IsEnd(firstTokenLw,curLineArr.Item(0).Lower())) )
        {
            m_Tokens.SkipToOneOfChars(";", true);
            break;
        }
        else if ( firstTokenLw.IsSameAs("private") && curLineArr.Count() == 0 )
        {
            defAccKind = taPrivate;
        }
        else if ( firstTokenLw.IsSameAs("final") && curLineArr.Count() > 0 )
        {
            int idx = curLineArr.Index("::");
            int startIdx = (idx == wxNOT_FOUND) ? 0 : idx+1;
            for (size_t i=startIdx; i<curLineArr.Count(); i++)
            {
                wxString name = curLineArr.Item(i);
                TokenF* token = DoAddToken(tkProcedureFinal, name.Lower(), wxEmptyString, lineNum);
                token->m_DisplayName = name;
                token->AddLineEnd(m_Tokens.GetLineNumber());
            }
        }

        if (breakAtEOL)
            break;
    }
}

bool ParserThreadF::IsEnd(wxString tok_low, wxString nex_low)
{
    bool isend = false;
    if ( tok_low.StartsWith("end") &&
            ( (tok_low.Matches("end") && (nex_low.empty() || m_KnownEndSecPart.count(nex_low))) ||
              tok_low.Matches("endsubroutine") ||
              tok_low.Matches("endfunction") ||
              tok_low.Matches("endmodule") ||
              tok_low.Matches("endsubmodule") ||
              tok_low.Matches("endtype") ||
              tok_low.Matches("endinterface") ||
              tok_low.Matches("endprogram") ||
              tok_low.Matches("endblock") ||
              tok_low.Matches("endblockdata") ||
              tok_low.Matches("endassociate") ||
              tok_low.Matches("endprocedure") )
       )
    {
        isend = true;
    }
    return isend;
}

void ParserThreadF::MakeArrayStringLower(wxArrayString &arr, wxArrayString &arrLw)
{
    for(size_t i=0; i<arr.Count(); i++)
    {
        arrLw.Add(arr.Item(i).Lower());
    }
}

void ParserThreadF::SetTokenAccess(ModuleTokenF* modToken, TokenF* token, TokenAccessKind defAKind)
{
    if (modToken->HasNameInPrivateList(token->m_Name))
    {
        token->m_TokenAccess = taPrivate;
    }
    else if (modToken->HasNameInPublicList(token->m_Name))
    {
        if (token->m_TokenAccess != taProtected)
            token->m_TokenAccess = taPublic;
    }
    else
    {
        token->m_TokenAccess = defAKind;
    }
}


void ParserThreadF::SplitAssociateConstruct(const wxString& argLine, std::map<wxString,wxString>& assocMap)
{
    wxString args = argLine;
    int pos = args.Find(')',true);
    if (pos != wxNOT_FOUND)
        args.Remove(pos);
    args = args.AfterFirst('(');

    int cnt = 0;
    int sta = 0;
    for(size_t i=0; i<args.Len(); i++)
    {
        if (args.GetChar(i) == '(')
            cnt++;
        else if (args.GetChar(i) == ')')
            cnt--;

        if ( (args.GetChar(i) == ',' && cnt == 0) ||
                (i == args.Len()-1) )
        {
            wxString block;
            if (args.GetChar(i) == ',')
                block = args.Mid(sta, i-sta);
            else
                block = args.Mid(sta, i-sta+1);
            sta = i + 1;
            int sind = block.Find("=>");
            if (sind != wxNOT_FOUND)
            {
                wxString assocName = block.Mid(0,sind).Trim(true).Trim(false);
                wxString sourceExpr = block.Mid(sind+2).Trim(true).Trim(false);
                assocMap.insert(std::pair<wxString,wxString>(assocName, sourceExpr));
            }
            else
            {
                wxString assocName = block.Trim(true).Trim(false);
                assocMap.insert(std::pair<wxString,wxString>(assocName, assocName));
            }
        }
    }
}

void ParserThreadF::GetDocBlock(DocBlock &docs, bool lookDown, unsigned int ln, bool takeSimpleDoc)
{
    bool isSimpleDoc = false;
    bool hasDoc = false;
    wxArrayString docLines;
    unsigned int loopStart;
    if (lookDown)
        loopStart = ln+1;
    else
        loopStart = ln-1;
    unsigned int ii = loopStart;
    unsigned int nLines = m_Tokens.GetLineCount();

    while (ii >= 1 && ii <= nLines)
    {
        wxString line = m_Tokens.GetLine(ii).Trim(false);

        if (ii == loopStart && !line.StartsWith("!"))
            isSimpleDoc = false;
        else if (ii == loopStart && line.StartsWith("!"))
        {
            if (!lookDown && (line.StartsWith("!!") || line.StartsWith("!<") || line.StartsWith("!>")))
                isSimpleDoc = false;
            else
            {
                isSimpleDoc = true;
                if (!takeSimpleDoc)
                    break;
            }
        }

        if (line.StartsWith("!"))
        {
            if (line.StartsWith("!!") || line.StartsWith("!<") || line.StartsWith("!>"))
            {
                docLines.Add(line.Mid(2).Trim(true).Trim(false));
            }
            else if (isSimpleDoc)
            {
                docLines.Add(line.Mid(1).Trim(true).Trim(false));
            }
            else
                break;

            hasDoc = true;
        }
        else if (hasDoc && line.IsEmpty())
            break;
        else if (!line.IsEmpty())
            break;

        if (lookDown)
            ii++;
        else
            ii--;
    }

    if (isSimpleDoc && docLines.GetCount()>0)
    {
        // Not Doxygen comments
        // Take first 3 nonempty lines and use it as a doc-string
        wxString docLine;
        int isimp = 0;
        if (lookDown)
            loopStart = 0;
        else
            loopStart = docLines.GetCount()-1;
        int j = loopStart;

        while ((lookDown && j < (int) docLines.GetCount()) || (!lookDown && j >= 0))
        {
            size_t sidx = docLines[j].find_first_not_of("! \t");
            if (sidx != wxString::npos)
            {
                size_t sidx2 = docLines[j].find_first_not_of(docLines[j].at(sidx)); // an attempt to avoid e.g. !********
                if (sidx2 != wxString::npos)
                {
                    //if (isimp == 3) // take 3 lines at most
                    if (docLine.size() > 400) // limit number of symbols
                    {
                        docLine << "...";
                        break;
                    }
                    if (sidx2 > sidx+2)
                        docLine << " " + docLines[j].Mid(sidx2);
                    else
                        docLine << " " + docLines[j].Mid(sidx);

                    isimp += 1;
                }
                else if (!docLine.IsEmpty())
                    break;
            }
            else if (!docLine.IsEmpty())
                break;

            if (lookDown)
                j++;
            else
                j--;
        }
        if (!docLine.IsEmpty())
            docs.AddDescription(docLine.Trim(false));

    }
    else if (docLines.GetCount()>0)
    {
        // Doxygen docs
        wxString description;
        wxString brief;
        wxArrayString paramNames;
        wxArrayString paramDescr;
        bool inbrief = false;
        bool indescription = false;
        for (int i=docLines.GetCount()-1; i>=0; i--)
        {
            bool iscommand = false;

            if (docLines[i].IsEmpty())
            {
                inbrief = false;
                indescription = false;
                continue;
            }
            else if (docLines[i].StartsWith("\\") || docLines[i].StartsWith("@"))
            {
                inbrief = false;
                indescription = false;
                iscommand = true;
            }

            bool isbrief = false;
            bool isparam = false;
            if (iscommand)
            {
                size_t sidx = docLines[i].find("brief",1);
                if (sidx == 1)
                    isbrief = true;

                if (!isbrief)
                {
                    sidx = docLines[i].find("param",1);
                    if (sidx == 1)
                        isparam = true;
                }
            }

            if (isbrief)
            {
                brief = docLines[i].Mid(6).Trim(false);
                inbrief = true;
                indescription = false;
            }
            else if (inbrief)
            {
                wxString repNoStr = TrimRepetitives(docLines[i]);
                if (!repNoStr.IsEmpty())
                    brief << " " << repNoStr;
            }
            else if (isparam)
            {
                wxString pline = docLines[i].Mid(6).Trim(false);
                size_t sidx = pline.find_first_of(" \t");
                if (sidx != wxString::npos)
                {
                    paramDescr.Add(pline.Mid(sidx+1).Trim(false));
                    paramNames.Add(pline.Mid(0,sidx));
                }
                indescription = false;
            }
            else if (!iscommand && description.empty() && paramNames.Count() == 0)
            {
                indescription = true;
            }

            if (indescription)
            {
                wxString repNoStr = TrimRepetitives(docLines[i]);
                if (!repNoStr.IsEmpty())
                    description << repNoStr + " ";
            }

        }

        if (!description.IsEmpty())
            docs.AddDescription(description.Trim());
        if (!brief.IsEmpty())
            docs.AddBrief(brief);
        for (size_t i=0; i<paramDescr.size(); i++)
        {
            docs.AddParam(paramNames[i], paramDescr[i]);
        }
    }
}

wxString ParserThreadF::TrimRepetitives(wxString& inStr)
{
    // an attempt to avoid e.g. !********
    wxString outStr;
    size_t sidx = inStr.find_first_not_of("! \t");
    if (sidx != wxString::npos)
    {
        size_t sidx2 = inStr.find_first_not_of(inStr.at(sidx),sidx);
        if (sidx2 != wxString::npos)
        {
            if (sidx2 > sidx+2)
                outStr = inStr.Mid(sidx2);
            else
                outStr = inStr.Mid(sidx);
        }
    }
    return outStr;
}

wxString ParserThreadF::GetDocLine(unsigned int ln)
{
    wxString line = m_Tokens.GetLine(ln);
    line = line.AfterFirst('!');
    if (line.StartsWith("<") || line.StartsWith(">"))
        line = line.substr(1);
    return line.Trim(true).Trim(false);
}

void ParserThreadF::AddParamDocs(TokenF* pParToken, DocBlock &docs)
{
    int npar = docs.GetParamCount();
    if (npar == 0)
        return;
    int nadd = 0;
    TokensArrayF* tokArr = &pParToken->m_Children;
    for (size_t j=0; j<tokArr->GetCount(); j++)
    {
        if (tokArr->Item(j)->m_TokenKind == tkVariable && tokArr->Item(j)->m_DocString.IsEmpty())
        {
            wxString descr = docs.GetValue(tokArr->Item(j)->m_Name);
            if (!descr.IsEmpty())
            {
                tokArr->Item(j)->m_DocString = descr;
                nadd++;
                if (nadd == npar)
                    break;
            }
        }
    }
}

void ParserThreadF::HandleBindTo()
{
    wxString line = m_Tokens.GetCurrentLine().Trim(true).Trim(false);
    line = line.Mid(7).Trim(false).Lower();
    DoAddToken(tkBindTo, wxEmptyString, line.BeforeFirst('!').Trim());
    m_Tokens.SkipToEOL();
}

void ParserThreadF::CheckParseCallProcedure(wxString& token, wxString& tok_low, wxString& next)
{
    if ( tok_low.IsSameAs("call") )
    {
        wxArrayString args_arr;
        token = m_Tokens.GetTokenSameFortranLine();
        while (true)
        {
            wxString nextTok = m_Tokens.PeekTokenSameFortranLine();
            if (nextTok.IsSameAs("%"))
            {
                token << m_Tokens.GetTokenSameFortranLine();
                token << m_Tokens.GetTokenSameFortranLine();
            }
            else if (nextTok.StartsWith("(") && nextTok.EndsWith(")"))
            {
                args_arr.Add(m_Tokens.GetTokenSameFortranLine());
            }
            else
                break;
        }
        if (token != wxEmptyString)
        {
            DoAddToken(tkCallSubroutine, token);
            for (size_t i=0; i<args_arr.size(); ++i)
            {
                token = args_arr.Item(i);
                if (token.StartsWith("(") && token.EndsWith(")"))
                {
                    // we have argument list. Find function-array calls.
                    wxString args = token.Mid(1, token.length()-2);
                    TakeFunctionsCallsFromString(args);
                }
            }
        }
    }
    else if (next.StartsWith("("))
    {
        wxString curLine = m_Tokens.GetLineFortran();
        TakeFunctionsCallsFromString(curLine);
        m_Tokens.SkipToOneOfChars(";", true);
    }
}

void ParserThreadF::TakeFunctionsCallsFromString(const wxString& strIn)
{
    wxString str = strIn;
    size_t idx_start = 0;
    for (int i=0; i<20; i++)
    {
        size_t idxR = str.find(')', idx_start);
        size_t idx = str.find('(', idx_start);
        if (idx == wxString::npos)
        {
            break;
        }
        else if (idxR != wxString::npos && idxR < idx)
        {
            idx_start = idxR + 1;
            continue;
        }
        else if (idx > 0)
        {
            int idxStart;
            wxString funName;
            int idxEnd = idx-1;
            while (true)
            {
                wxString wordTmp;
                GetWordBefore(str, idxEnd, wordTmp, idxStart);
                if (wordTmp.IsEmpty())
                {
                    break;
                }
                else if (wordTmp.StartsWith("(") && wordTmp.EndsWith(")"))
                {
                    idxEnd = idxStart - 1;
                }
                else
                {
                    int idxCur = idxStart-1;
                    for (; idxCur>=0; idxCur--)
                    {
                        if (!isspace(str.GetChar(idxCur)))
                            break;
                    }

                    if (idxCur >= 0 && str.GetChar(idxCur) == '%')
                    {
                        funName = "%" + wordTmp + funName;
                        idxEnd = idxCur-1;
                    }
                    else
                    {
                        funName = wordTmp + funName;
                        break;
                    }
                }
            }

            if (!funName.empty() && !isdigit(funName.GetChar(0)))
            {
                DoAddToken(tkCallFunction, funName);
            }
            else
                break; // something is wrong
        }
        idx_start = idx+1;
    }
}

void ParserThreadF::GetWordBefore(const wxString& str, int idxEnd, wxString& funName, int& idxStart)
{
    funName = wxEmptyString;
    for (; idxEnd>=0; idxEnd--)
    {
        if (!isspace(str.GetChar(idxEnd)))
            break;
    }

    if (idxEnd < 0)
        return;

    int lev = 0;
    for (idxStart=idxEnd; idxStart>=0; idxStart--)
    {
        if (str.GetChar(idxStart) == ')')
        {
            lev+=1;
        }
        else if (str.GetChar(idxStart) == '(')
        {
            if (lev == 0)
                break;
            else if (lev == 1)
            {
                idxStart-=1;
                break;
            }
            else
                lev-=1;
        }
        else if (!isalnum(str.GetChar(idxStart)) && str.GetChar(idxStart) != '_' && str.GetChar(idxStart) != '$')
            break;
    }
    idxStart += 1;
    funName = str.Mid(idxStart, idxEnd-idxStart+1);
}

wxString ParserThreadF::GetAdditionalIncludeFile(wxString filename)
{
    wxFileName fname(filename);
    wxString filename_ux = fname.GetFullPath(wxPATH_UNIX);
    try
    {
        if (m_pAIncludeFiles && m_pAIncludeFiles->count(filename_ux) == 1)
            return m_pAIncludeFiles->at(filename_ux);
    }
    catch (const std::out_of_range& oor)
    {
        // 'out_of_range' can be thrown if map is modified in the main thread.
        // Do nothing.
    }

    return wxEmptyString;
}

std::vector<wxString> ParserThreadF::GetParsedFileNames()
{
    return m_Tokens.GetParsedFileNames();
}

TokenizerPP::SkippedLinesStruct* ParserThreadF::GetSkippedLines(const wxString& fileName)
{
    return m_Tokens.GetSkippedLines(fileName);
}

