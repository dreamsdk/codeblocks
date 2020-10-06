
#include "calltree.h"

#ifndef CB_PRECOMP
    #include <manager.h>
#endif

#include "tokenf.h"
#include "submoduletokenf.h"
#include "calledbydict.h"

CallTree::CallTree(FortranProject* forproj)
{
    m_pCallTreeView = new CallTreeView(Manager::Get()->GetAppWindow(), forproj);

    m_FortranIntrinsicModules.insert(_T("iso_c_binding"));
    m_FortranIntrinsicModules.insert(_T("iso_fortran_env"));
    m_FortranIntrinsicModules.insert(_T("ieee_exceptions"));
    m_FortranIntrinsicModules.insert(_T("ieee_arithmetic"));
    m_FortranIntrinsicModules.insert(_T("ieee_features"));
    m_FortranIntrinsicModules.insert(_T("omp_lib"));

    CodeBlocksDockEvent evt(cbEVT_ADD_DOCK_WINDOW);
    evt.name = _T("FCallTree");
    evt.title = _("Fortran Call/Called-By Tree");
    evt.pWindow = m_pCallTreeView;
    evt.dockSide = CodeBlocksDockEvent::dsFloating;
    evt.desiredSize.Set(200, 250);
    evt.floatingSize.Set(200, 250);
    evt.minimumSize.Set(150, 150);
    Manager::Get()->ProcessEvent(evt);
}

CallTree::~CallTree()
{
    if (m_pCallTreeView)
    {
        CodeBlocksDockEvent evt(cbEVT_REMOVE_DOCK_WINDOW);
        evt.pWindow = m_pCallTreeView;
        Manager::Get()->ProcessEvent(evt);
        m_pCallTreeView->Destroy();
        m_pCallTreeView = nullptr;
    }
}

void CallTree::BuildCallTree(cbEditor* ed, const wxString& NameUnderCursor, ParserF* pParser, std::set< wxString>& keywordSet, bool showCallTree)
{
    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* pRoot = tokensTmp.GetTokens();

    pParser->FindMatchTokensForJump(ed, true, true, *pRoot);

    int tokenKindMask = tkFunction | tkSubroutine | tkProgram | tkInterface | tkInterfaceExplicit | tkProcedure
                        | tkModule | tkSubmodule;

    if (pRoot->GetCount() > 1)
    {
        // Remove not-procedures
        for (size_t i=0; i < pRoot->GetCount(); )
        {
            if (!(pRoot->Item(i)->m_TokenKind & tokenKindMask))
            {
                pRoot->Item(i)->Clear();
                delete pRoot->Item(i);
                pRoot->RemoveAt(i);
                if (pRoot->GetCount() == 1)
                    break;
            }
            else
                i++;
        }
    }

    if (pRoot->GetCount() == 1 && !(pRoot->Item(0)->m_TokenKind & tokenKindMask))
    {
        wxString msg = _T("\"") + NameUnderCursor + _("\" is not a procedure or a module.");
        cbMessageBox(msg, _("Error"), wxICON_ERROR);
        return;
    }
    else if (pRoot->GetCount() == 0)
    {
        wxString msg = _("Procedure \"") + NameUnderCursor + _("\" was not found.");
        cbMessageBox(msg, _("Not found"), wxICON_WARNING);
        return;
    }

    bool isVis = IsWindowReallyShown((wxWindow*)m_pCallTreeView);
    if (!isVis)
    {
        CodeBlocksDockEvent evt(cbEVT_SHOW_DOCK_WINDOW);
        evt.pWindow = (wxWindow*) m_pCallTreeView;
        Manager::Get()->ProcessEvent(evt);
    }

    int explIntIdx = -1;
    for (size_t i=0; i<pRoot->size(); i++)
    {
        if (pRoot->Item(i)->m_ParentTokenKind == tkInterfaceExplicit)
        {
            explIntIdx = i;
            break;
        }
    }

    CalledByDict cByDict;
    if (!showCallTree)
        pParser->BuildCalledByDict(cByDict);

    TokensArrayClass tokAllTmp;
    TokensArrayF* tokAll = tokAllTmp.GetTokens();

    for (size_t i=0; i<pRoot->size(); i++)
    {
        if (explIntIdx != -1 && explIntIdx != int(i))
            continue; // take only explicitInterface if such exist

        TokenFlat* tf = pRoot->Item(i);
        CallTreeToken* tok = new CallTreeToken(tf);
        tokAll->Add(tok);

        if (showCallTree)
        {
            // Call tree
            if (explIntIdx != -1 && explIntIdx == int(i))
            {
                // it is explicitInterface
                ManageInterfaceExplicit(pParser, tf, tok, keywordSet);
            }
            else if (tf->m_TokenKind == tkProcedure && tf->m_ParentTokenKind == tkType)
            {
                // it is type-bound procedure
                ManageTBProceduresForCallTree(pParser, tf, tok, keywordSet);
            }
            else if (tf->m_TokenKind == tkModule || tf->m_TokenKind == tkSubmodule)
            {
                FindUsedModules(pParser, tok);
            }
            else
            {
                FindCalledTokens(pParser, tok, keywordSet);
            }
        }
        else
        {
            // Called By tree
            FindCallingTokens(pParser, tok, cByDict);
        }
    }

    if (showCallTree)
        m_pCallTreeView->ShowCallTree(tokAll);
    else
        m_pCallTreeView->ShowCalledByTree(tokAll);

}

void CallTree::FindUsedModules(ParserF* pParser, CallTreeToken* token)
{
    if (token->m_TokenKind == tkSubmodule)
    {
        TokenFlat ctF(token);
        TokenF* tokSub = pParser->FindToken(ctF);
        if (tokSub && tokSub->m_TokenKind == tkSubmodule)
        {
            SubmoduleTokenF* submod = static_cast<SubmoduleTokenF*>(tokSub);
            wxString parentModName = submod->m_AncestorModuleName;

            TokensArrayFlatClass tokensMod;
            TokensArrayFlat* resultMod = tokensMod.GetTokens();
            int noChildrenOf = tkInterface | tkFunction | tkSubroutine | tkProgram | tkModule | tkSubmodule;
            pParser->FindMatchTokensDeclared(parentModName, *resultMod, tkModule | tkSubmodule, false, noChildrenOf, true);

            if (resultMod->size() == 0)
            {
                // Ancestor module | submodule was not found
                TokenFlat modFT(ctF);
                modFT.m_Name = parentModName;
                modFT.m_DisplayName = parentModName;
                modFT.m_TokenKind = tkModule;

                if (!HasCallChildToken(token, &modFT))
                {
                    CallTreeToken* tok2  = new CallTreeToken(&modFT, token);
                    tok2->m_CallFilename = modFT.m_Filename;
                    tok2->m_CallLine     = modFT.m_LineStart;
                    tok2->m_TokenKind    = tkCallSubroutine; // to get "unknown" icon, which shows that the ancestor was not found

                    token->AddChild(tok2);
                }
            }
            else
            {
                TokenFlat* tf2 = resultMod->Item(0); // take just first result
                if (!HasChildToken(token, tf2) && !HasInHerarchy(token, tf2))
                {
                    CallTreeToken* tok2 = new CallTreeToken(tf2, token);
                    tok2->m_CallFilename = ctF.m_Filename;
                    tok2->m_CallLine     = ctF.m_LineStart;
                    token->AddChild(tok2);

                    FindUsedModules(pParser, tok2);
                }
            }
        }
    }

    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* callChildren = tokensTmp.GetTokens();

    int callFilter = tkUse | tkInclude ;
    TokenFlat tf = TokenFlat(token);
    pParser->GetChildren(&tf, callFilter, *callChildren, 8);  // here levelMax should be more than enough
    size_t ncChild = callChildren->size();
    for (size_t j=0; j<ncChild; j++)
    {
        TokenFlat* oneCall = callChildren->Item(j);
        if (oneCall->m_TokenKind == tkUse && m_FortranIntrinsicModules.count(oneCall->m_Name) != 0)
            continue;
        TokensArrayFlatClass tokClTmp;
        TokensArrayFlat* resToks = tokClTmp.GetTokens();
        FindTokenFromCall(pParser, &tf, oneCall, resToks);

        if (resToks->size() == 0)
        {
            if (!HasCallChildToken(token, oneCall))
            {
                CallTreeToken* tok2 = new CallTreeToken(oneCall, token);
                tok2->m_CallFilename = oneCall->m_Filename;
                tok2->m_CallLine     = oneCall->m_LineStart;
                tok2->m_TokenKind    = tkCallSubroutine; // just to get "unknown" icon

                token->AddChild(tok2);
            }
        }
        else
        {
            for (size_t k=0; k<resToks->size(); k++)
            {
                TokenFlat* tf2 = resToks->Item(k);
                if (!HasChildToken(token, tf2) && !HasInHerarchy(token, tf2))
                {
                    CallTreeToken* tok2 = new CallTreeToken(tf2, token);
                    tok2->m_CallFilename = oneCall->m_Filename;
                    tok2->m_CallLine     = oneCall->m_LineStart;
                    token->AddChild(tok2);

                    FindUsedModules(pParser, tok2);
                    break; // take just first suitable result
                }
            }
        }
    }
}

void CallTree::FindCalledTokens(ParserF* pParser, CallTreeToken* token, std::set< wxString>& keywordSet)
{
    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* callChildren = tokensTmp.GetTokens();

    int callFilter;
    if (token->m_TokenKind == tkInterface)
        callFilter = tkOther;
    else
        callFilter = tkCallSubroutine | tkCallFunction;

    TokenFlat tf = TokenFlat(token);
    pParser->GetChildren(&tf, callFilter, *callChildren);
    size_t ncChild = callChildren->size();

    for (size_t j=0; j<ncChild; j++)
    {
        TokenFlat* oneCall = callChildren->Item(j);
        if (keywordSet.count(oneCall->m_Name) != 0)
            continue;
        TokensArrayFlatClass tokClTmp;
        TokensArrayFlat* resToks = tokClTmp.GetTokens();
        FindTokenFromCall(pParser, &tf, oneCall, resToks);

        if (resToks->size() == 0)
        {
            if (!HasCallChildToken(token, oneCall))
            {
                CallTreeToken* tok2 = new CallTreeToken(oneCall, token);
                tok2->m_CallFilename = oneCall->m_Filename;
                tok2->m_CallLine     = oneCall->m_LineStart;

                token->AddChild(tok2);
            }
        }
        else
        {
            TokenFlat* tokType = NULL;
            for (size_t k=0; k<resToks->size(); k++)
            {
                TokenFlat* tf2 = resToks->Item(k);
                if (tf2->m_TokenKind == tkType)
                {
                    tokType = tf2;
                }
                else if ((tf2->m_TokenKind != tkVariable) && !HasChildToken(token, tf2) && !HasInHerarchy(token, tf2))
                {
                    CallTreeToken* tok2 = new CallTreeToken(tf2, token);
                    tok2->m_CallFilename = oneCall->m_Filename;
                    tok2->m_CallLine     = oneCall->m_LineStart;

                    token->AddChild(tok2);

                    if (tf2->m_ParentTokenKind == tkInterfaceExplicit)
                    {
                        ManageInterfaceExplicit(pParser, tf2, tok2, keywordSet);
                    }
                    else if (tf2->m_TokenKind == tkProcedure && tf2->m_ParentTokenKind == tkType)
                    {
                        // it is type-bound procedure
                        ManageTBProceduresForCallTree(pParser, tf2, tok2, keywordSet);
                    }
                    else
                    {
                        FindCalledTokens(pParser, tok2, keywordSet);
                    }
                    tokType = NULL;
                    break; // take just first suitable result
                }
            }

            if (tokType && !HasChildToken(token, tokType) && !HasInHerarchy(token, tokType))
            {
                CallTreeToken* tok2 = new CallTreeToken(tokType, token);
                tok2->m_CallFilename = oneCall->m_Filename;
                tok2->m_CallLine     = oneCall->m_LineStart;

                token->AddChild(tok2);
            }
        }
    }
}


void CallTree::FindTokenFromCall(ParserF* pParser, TokenFlat* parentTok, TokenFlat* oneCall, TokensArrayFlat* result)
{
    int tokenKindMask = tkFunction | tkSubroutine | tkInterface | tkInterfaceExplicit | tkVariable | tkType;

    if (oneCall->m_Name.Find('%') != wxNOT_FOUND && parentTok)
    {
        // call of type-bound procedure
        pParser->FindMatchTypeComponents(*parentTok, oneCall->m_Name, *result);
    }
    else
    {
        wxString findName;
        if (oneCall->m_TokenKind == tkProcedure && !oneCall->m_PartLast.IsEmpty())
            findName = oneCall->m_PartLast;
        else
            findName = oneCall->m_Name;
        pParser->FindUseAssociatedTokens(true, oneCall, findName, false, *result, tokenKindMask, false);
    }
    if (oneCall->m_ParentTokenKind == tkInterfaceExplicit)
        pParser->FindImplementedProcInMySubmodules(oneCall, oneCall->m_Name, *result);
    if (result->GetCount() > 0)
        return;

    // Try to find global procedures
    int noChildrenOf = tkInterface | tkFunction | tkSubroutine | tkProgram | tkModule | tkSubmodule;
    tokenKindMask = tokenKindMask | tkModule;
    pParser->FindMatchTokensDeclared(oneCall->m_Name, *result, tokenKindMask, false, noChildrenOf, false, true);
}

bool CallTree::HasChildToken(TokenF* tokParent, TokenF* tok)
{
    TokensArrayF* tokArr = &tokParent->m_Children;
    for (size_t i=0; i<tokArr->size(); i++)
    {
        TokenF* tokItem = tokArr->Item(i);
        if (tokItem->m_TokenKind == tok->m_TokenKind &&
            tokItem->m_Name == tok->m_Name &&
            tokItem->m_Filename == tok->m_Filename &&
            tokItem->m_LineStart == tok->m_LineStart &&
            tokItem->m_LineEnd == tok->m_LineEnd &&
            tokItem->m_TokenAccess == tok->m_TokenAccess)
        {
            return true;
        }
    }
    return false;
}

bool CallTree::HasCallChildToken(TokenF* tokParent, TokenFlat* tok)
{
    TokensArrayF* tokArr = &tokParent->m_Children;
    for (size_t i=0; i<tokArr->size(); i++)
    {
        TokenF* tokItem = tokArr->Item(i);
        if (tokItem->m_Name == tok->m_Name)
        {
            return true;
        }
    }
    return false;
}

bool CallTree::HasInHerarchy(TokenF* tokParent, TokenF* tok)
{
    // Used to avoid recursive calls
    TokenF* tokIn = tokParent;
    while(tokIn)
    {
        if (tokIn->m_TokenKind == tok->m_TokenKind &&
            tokIn->m_Name == tok->m_Name &&
            tokIn->m_Filename == tok->m_Filename &&
            tokIn->m_LineStart == tok->m_LineStart &&
            tokIn->m_LineEnd == tok->m_LineEnd &&
            tokIn->m_TokenAccess == tok->m_TokenAccess)
        {
            return true;
        }
        tokIn = tokIn->m_pParent;
    }
    return false;
}

void CallTree::ManageInterfaceExplicit(ParserF* pParser, TokenFlat* origFT, CallTreeToken* token, std::set<wxString>& keywordSet)
{
    // Try to find global procedures
    TokensArrayFlatClass tokGlobTmp;
    TokensArrayFlat* resGlobOrSumb = tokGlobTmp.GetTokens();
    int tokenKindMask = tkFunction | tkSubroutine;
    int noChildrenOf = tkInterface | tkInterfaceExplicit | tkFunction | tkSubroutine | tkProgram | tkModule | tkSubmodule;
    pParser->FindMatchTokensDeclared(origFT->m_Name, *resGlobOrSumb, tokenKindMask, false, noChildrenOf, false, true);

    if (resGlobOrSumb->size() == 0)
    {
        // Try to find implementation between submodule procedures
        pParser->FindImplementedProcInMySubmodules(origFT, origFT->m_Name, *resGlobOrSumb);
    }

    for (size_t l=0; l<resGlobOrSumb->size(); l++)
    {
        TokenFlat* tfGlob = resGlobOrSumb->Item(l);
        if (!HasChildToken(token, tfGlob))
        {
            CallTreeToken* tg = new CallTreeToken(tfGlob, token);
            tg->m_CallFilename = token->m_Filename;
            tg->m_CallLine     = token->m_LineStart;

            token->AddChild(tg);

            FindCalledTokens(pParser, tg, keywordSet);
        }
    }
}

void CallTree::FindCallingTokens(ParserF* pParser, CallTreeToken* token, CalledByDict& cByDict)
{
    std::list<TokenF*>* tokList = cByDict.GetCallingTokens(token->m_Name);
    if (!tokList)
        return;

    for (std::list<TokenF*>::iterator li=tokList->begin(); li != tokList->end(); ++li)
    {
        TokenF* pCTok = *li;
        TokenFlat oneCall(pCTok);
        if (oneCall.m_TokenKind == tkSubmodule)
        {
            oneCall.m_Name = oneCall.m_Name.BeforeLast(':');
        }
        TokensArrayFlatClass tokClTmp;
        TokensArrayFlat* resToks = tokClTmp.GetTokens();
        TokenFlat parTokF = TokenFlat(pCTok->m_pParent);
        FindTokenFromCall(pParser, &parTokF, &oneCall, resToks);

        for (size_t k=0; k<resToks->size(); k++)
        {
            TokenFlat* tf2 = resToks->Item(k);
            if (tf2->m_TokenKind == token->m_TokenKind &&
                tf2->m_Name == token->m_Name &&
                tf2->m_Filename == token->m_Filename &&
                tf2->m_LineStart == token->m_LineStart)
            {
                TokenF* parTok = NULL;
                if (pCTok->m_TokenKind == tkSubmodule)
                {
                    parTok = pCTok;
                }
                else if (pCTok->m_pParent)
                {
                    if (pCTok->m_pParent->m_TokenKind == tkInterfaceExplicit)
                        parTok = pCTok;
                    else if (pCTok->m_pParent->m_TokenKind == tkType)
                        parTok = pCTok;
                    else if (pCTok->m_pParent->m_TokenKind == tkAssociateConstruct)
                    {
                        parTok = pCTok->m_pParent;
                        while (parTok)
                        {
                            if (parTok->m_TokenKind == tkAssociateConstruct)
                            {
                                parTok = parTok->m_pParent;
                            }
                            else
                                break;
                        }
                    }
                    else
                        parTok = pCTok->m_pParent;
                }

                if (parTok && !HasChildToken(token, parTok) && !HasInHerarchy(token, parTok))
                {
                    CallTreeToken* tok2 = new CallTreeToken(parTok, token);
                    tok2->m_CallFilename = pCTok->m_Filename;
                    tok2->m_CallLine     = pCTok->m_LineStart;

                    token->AddChild(tok2);

                    FindCallingTokens(pParser, tok2, cByDict);
                }

                break; // take only first suitable item
            }
        }
    }
}

void CallTree::ManageTBProceduresForCallTree(ParserF* pParser, TokenFlat* origFT, CallTreeToken* token, std::set<wxString>& keywordSet)
{
    TokensArrayFlatClass resultTmp;
    TokensArrayFlat* result = resultTmp.GetTokens();
    FindTokenFromCall(pParser, NULL, origFT, result);

    for (size_t l=0; l<result->size(); l++)
    {
        TokenFlat* tf = result->Item(l);
        if (!HasChildToken(token, tf))
        {
            CallTreeToken* tg = new CallTreeToken(tf, token);
            tg->m_CallFilename = token->m_Filename;
            tg->m_CallLine     = token->m_LineStart;

            token->AddChild(tg);

            FindCalledTokens(pParser, tg, keywordSet);
        }
    }
}
