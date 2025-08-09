#ifndef CALLTREE_H
#define CALLTREE_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/stopwatch.h>
    #include <wx/progdlg.h>
#endif
#include <set>

#include "calltreeview.h"
#include "parserf.h"

struct TokenTreeMapKey
{
    int m_LineStart;
    wxString m_Name;
    wxString m_Filename;

    bool operator< (const TokenTreeMapKey& rhs)  const {
        if (m_LineStart < rhs.m_LineStart)
        {
            return true;
        }
        else if (m_LineStart > rhs.m_LineStart)
        {
            return false;
        }
        else
        {
            // m_LineStart == rhs.m_LineStart
            if (m_Name.Cmp(rhs.m_Name) < 0)
            {
                return true;
            }
            else if (m_Name.Cmp(rhs.m_Name) > 0)
            {
                return false;
            }
            else
            {
                // m_Name.Cmp(rhs.m_Name) == 0
                if (m_Filename.Cmp(rhs.m_Filename) < 0)
                {
                    return true;
                }
                else
                {
                    return false; // lhs >= rhs
                }
            }
        }
        return false; // this should not happen.
    }
};


class CallTree
{
    public:
        CallTree(FortranProject* forproj);
        virtual ~CallTree();

        CallTreeView* GetCallTreeView(){return m_pCallTreeView;};
        void BuildCallTree(cbEditor* ed, const wxString& NameUnderCursor, ParserF* pParser, std::set< wxString>& keywordSet, bool showCallTree);

    protected:

    private:
        void ClearTokensArray(TokensArrayF* tokens);
        void FindUsedModules(ParserF* pParser, CallTreeToken* token);
        void FindCalledTokens(ParserF* pParser, CallTreeToken* token, std::set<wxString>& keywordSet);
        void FindTokenFromCall(ParserF* pParser, TokenFlat* parentTok, TokenFlat* oneCall, TokensArrayFlat* result);
        bool HasChildToken(TokenF* tokParent, TokenF* tok);
        bool HasCallChildToken(TokenF* tokParent, TokenFlat* tok);
        bool HasInHierarchy(TokenF* tokParent, TokenF* tok);
        bool FindInTree(CallTreeToken* findTok);
        void ManageInterfaceExplicit(ParserF* pParser, TokenFlat* origFT, CallTreeToken* token, std::set<wxString>& keywordSet);
        void FindCallingTokens(ParserF* pParser, CallTreeToken* token, CalledByDict& cByDict);
        void ManageTBProceduresForCallTree(ParserF* pParser, TokenFlat* origFT, CallTreeToken* token, std::set<wxString>& keywordSet);

        CallTreeView* m_pCallTreeView;

        std::set<wxString> m_FortranIntrinsicModules;

        std::map<TokenTreeMapKey, CallTreeToken*> m_CallTreeTokenMap;

        wxStopWatch m_StopWatch;
        long m_TimeOld;
        wxProgressDialog* m_pProgressDlg;
        bool m_Cancelled;
        int m_CallDepth;
        int m_CallDepthMax;
};

#endif // CALLTREE_H
