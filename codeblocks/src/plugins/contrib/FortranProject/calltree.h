#ifndef CALLTREE_H
#define CALLTREE_H

#include "calltreeview.h"
#include "parserf.h"

class CallTree
{
    public:
        CallTree(FortranProject* forproj);
        virtual ~CallTree();

        CallTreeView* GetCallTreeView(){return m_pCallTreeView;};
        void BuildCallTree(cbEditor* ed, const wxString& NameUnderCursor, ParserF* pParser, std::set< wxString>& keywordSet, bool showCallTree);

    protected:

    private:
        void FindCalledTokens(ParserF* pParser, TokenF* token, std::set< wxString>& keywordSet);
        void FindTokenFromCall(ParserF* pParser, TokenFlat* parentTok, TokenFlat* oneCall, TokensArrayFlat* result);
        bool HasChildToken(TokenF* tokParent, TokenF* tok);
        bool HasCallChildToken(TokenF* tokParent, TokenFlat* tok);
        bool HasInHerarchy(TokenF* tokParent, TokenF* tok);
        void ManageInterfaceExplicit(ParserF* pParser, TokenFlat* origFT, TokenF* token, std::set<wxString>& keywordSet);
        void FindCallingTokens(ParserF* pParser, TokenF* token, CalledByDict& cByDict);
        void ManageTBProceduresForCallTree(ParserF* pParser, TokenFlat* origFT, TokenF* token, std::set<wxString>& keywordSet);

        CallTreeView* m_pCallTreeView;
};

#endif // CALLTREE_H
