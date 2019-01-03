#include "calltree.h"
#include "tokenf.h"
#include "calledbydict.h"
#include <manager.h>



CallTree::CallTree(FortranProject* forproj)
{
    m_pCallTreeView = new CallTreeView(Manager::Get()->GetAppWindow(), forproj);
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

    int tokenKindMask = tkFunction | tkSubroutine | tkProgram | tkInterface | tkInterfaceExplicit | tkProcedure;
    if (pRoot->GetCount() > 1)
    {
        // Remove non-procedures
        for (size_t i=0; i < pRoot->GetCount(); i++)
        {
            if (!(pRoot->Item(i)->m_TokenKind & tokenKindMask))
            {
                pRoot->Item(i)->Clear();
                delete pRoot->Item(i);
                pRoot->RemoveAt(i);
                if (pRoot->GetCount() == 1)
                    break;
            }
        }
    }

    if (pRoot->GetCount() == 1 && !(pRoot->Item(0)->m_TokenKind & tokenKindMask))
    {
        wxString msg = _T("\"") + NameUnderCursor + _("\" is not a procedure.");
        cbMessageBox(msg, _("Error"), wxICON_ERROR);
        return;
    }
    else if (pRoot->GetCount() == 0)
    {
        wxString msg = _("No procedure with name \"") + NameUnderCursor + _("\" was found.");
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
        TokenF* tok = new TokenF();
        tok->m_TokenKind   = tf->m_TokenKind;
        tok->m_DisplayName = tf->m_DisplayName;
        tok->m_Name        = tf->m_Name;
        tok->m_Filename    = tf->m_Filename;
        tok->m_LineStart   = tf->m_LineStart;
        tok->m_LineEnd     = tf->m_LineEnd;
        tok->m_TokenAccess = tf->m_TokenAccess;
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


void CallTree::FindCalledTokens(ParserF* pParser, TokenF* token, std::set< wxString>& keywordSet)
{
    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* callChildren = tokensTmp.GetTokens();

    int callFilter;
    if (token->m_TokenKind == tkInterface)
        callFilter = tkOther;
    else
        callFilter = tkCallSubroutine | tkCallFunction;

    TokenFlat tf = TokenFlat(token);
    pParser->GetChildren(&tf, callFilter, *callChildren); // why don't take 'token' children directly?

    for (size_t j=0; j<callChildren->size(); j++)
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
                TokenF* tok2 = new TokenF();
                tok2->m_TokenKind   = oneCall->m_TokenKind;
                tok2->m_DisplayName = oneCall->m_DisplayName;
                tok2->m_Name        = oneCall->m_Name;
                tok2->m_Filename    = oneCall->m_Filename;
                tok2->m_LineStart   = oneCall->m_LineStart;
                tok2->m_LineEnd     = oneCall->m_LineEnd;
                tok2->m_TokenAccess = oneCall->m_TokenAccess;
                tok2->m_pParent     = token;
                token->AddChild(tok2);
            }
        }
        else
        {
            for (size_t k=0; k<resToks->size(); k++)
            {
                TokenFlat* tf2 = resToks->Item(k);
                if ((tf2->m_TokenKind != tkVariable) && !HasChildToken(token, tf2) && !HasInHerarchy(token, tf2))
                {
                    TokenF* tok2 = new TokenF();
                    tok2->m_TokenKind   = tf2->m_TokenKind;
                    tok2->m_DisplayName = tf2->m_DisplayName;
                    tok2->m_Name        = tf2->m_Name;
                    tok2->m_Filename    = tf2->m_Filename;
                    tok2->m_LineStart   = tf2->m_LineStart;
                    tok2->m_LineEnd     = tf2->m_LineEnd;
                    tok2->m_TokenAccess = tf2->m_TokenAccess;
                    tok2->m_pParent     = token;
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
                    break; // take just first suitable result
                }
            }
        }
    }
}


void CallTree::FindTokenFromCall(ParserF* pParser, TokenFlat* parentTok, TokenFlat* oneCall, TokensArrayFlat* result)
{
    int tokenKindMask = tkFunction | tkSubroutine | tkInterface | tkInterfaceExplicit | tkVariable;

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

void CallTree::ManageInterfaceExplicit(ParserF* pParser, TokenFlat* origFT, TokenF* token, std::set<wxString>& keywordSet)
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
            TokenF* tg = new TokenF();
            tg->m_TokenKind   = tfGlob->m_TokenKind;
            tg->m_DisplayName = tfGlob->m_DisplayName;
            tg->m_Name        = tfGlob->m_Name;
            tg->m_Filename    = tfGlob->m_Filename;
            tg->m_LineStart   = tfGlob->m_LineStart;
            tg->m_LineEnd     = tfGlob->m_LineEnd;
            tg->m_TokenAccess = tfGlob->m_TokenAccess;
            tg->m_pParent     = token;
            token->AddChild(tg);

            FindCalledTokens(pParser, tg, keywordSet);
        }
    }
}

void CallTree::FindCallingTokens(ParserF* pParser, TokenF* token, CalledByDict& cByDict)
{
    std::list<TokenF*>* tokList = cByDict.GetCallingTokens(token->m_Name);
    if (!tokList)
        return;

    for (std::list<TokenF*>::iterator li=tokList->begin(); li != tokList->end(); ++li)
    {
        TokenF* pCTok = *li;
        TokenFlat oneCall(pCTok);

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
                TokenF* parTok;
                if (pCTok->m_pParent->m_TokenKind == tkInterfaceExplicit)
                    parTok = pCTok;
                else if (pCTok->m_pParent->m_TokenKind == tkType)
                    parTok = pCTok;
                else
                    parTok = pCTok->m_pParent;

                if (!HasChildToken(token, parTok) && !HasInHerarchy(token, parTok))
                {
                    TokenF* tok2 = new TokenF();
                    tok2->m_TokenKind   = parTok->m_TokenKind;
                    tok2->m_DisplayName = parTok->m_DisplayName;
                    tok2->m_Name        = parTok->m_Name;
                    tok2->m_Filename    = parTok->m_Filename;
                    tok2->m_LineStart   = parTok->m_LineStart;
                    tok2->m_LineEnd     = parTok->m_LineEnd;
                    tok2->m_TokenAccess = parTok->m_TokenAccess;
                    tok2->m_pParent     = token;
                    token->AddChild(tok2);

                    FindCallingTokens(pParser, tok2, cByDict);
                }

                break;
            }
        }
    }
}

void CallTree::ManageTBProceduresForCallTree(ParserF* pParser, TokenFlat* origFT, TokenF* token, std::set<wxString>& keywordSet)
{
    TokensArrayFlatClass resultTmp;
    TokensArrayFlat* result = resultTmp.GetTokens();
    FindTokenFromCall(pParser, NULL, origFT, result);

    for (size_t l=0; l<result->size(); l++)
    {
        TokenFlat* tf = result->Item(l);
        if (!HasChildToken(token, tf))
        {
            TokenF* tg = new TokenF();
            tg->m_TokenKind   = tf->m_TokenKind;
            tg->m_DisplayName = tf->m_DisplayName;
            tg->m_Name        = tf->m_Name;
            tg->m_Filename    = tf->m_Filename;
            tg->m_LineStart   = tf->m_LineStart;
            tg->m_LineEnd     = tf->m_LineEnd;
            tg->m_TokenAccess = tf->m_TokenAccess;
            tg->m_pParent     = token;
            token->AddChild(tg);

            FindCalledTokens(pParser, tg, keywordSet);
        }
    }
}
