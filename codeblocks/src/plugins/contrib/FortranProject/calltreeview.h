#ifndef CALLTREEVIEW_H
#define CALLTREEVIEW_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/panel.h>
    #include <wx/treectrl.h>

    #include <cbthreadpool.h>
#endif

#include "tokenf.h"
#include "fpimagelist.h"
#include "jumptracker.h"

class FortranProject;

class CallTreeToken : public TokenF
{
    public:
        CallTreeToken(TokenFlat* tf, CallTreeToken* parent=NULL);
        CallTreeToken(TokenF* tf, CallTreeToken* parent=NULL);
        virtual ~CallTreeToken() {};

        wxString m_CallFilename;
        unsigned int m_CallLine;
        bool wereChildrenConnnected;
};

class CTVData : public wxTreeItemData
{
    public:
        CTVData(TokenF* token);

        wxString m_DefFilename;
        unsigned int m_DefLineStart;
        TokenKindF m_DefTokenKind;

        wxString m_CallFilename;
        unsigned int m_CallLine;
};

class CallTreeView : public wxPanel
{
    public:
        CallTreeView(wxWindow* parentWindow, FortranProject* forproj);
        virtual ~CallTreeView();
        void ShowCallTree(TokensArrayF* tokArr);
        void ShowCalledByTree(TokensArrayF* tokArr);

    protected:

    private:
        void ShowCallTreeChildren(TokensArrayF* tokArr, wxTreeItemId& parent, int callLevel);
        wxTreeItemId InsertTreeItem(wxTreeItemId& parent, const wxString& displayName, int imageIdx, wxTreeItemData* tidata);
        void OnTreeDoubleClick(wxTreeEvent& event);
        void GoToLine(wxString& filename, unsigned int line);
        void ShowMenu(wxTreeItemId id, const wxPoint& pt, bool isFirstLevelItem);
        void OnTreeItemRightClick(wxTreeEvent& event);
        void OnRefreshTree(wxCommandEvent& event);
        void OnChangeSort(wxCommandEvent& event);
        void OnGoToProcedure(wxCommandEvent& event);
        void OnGoToCall(wxCommandEvent& event);
        void RereadOptions();
        void UpdateView();

        wxTreeCtrl* m_pTree;
        FPImageList* m_pImgList;
        FortranProject* m_pFortranProject;
        bool m_IsCallTree;
        bool m_SortAlphabetically;

    DECLARE_EVENT_TABLE()
};

#endif // CALLTREEVIEW_H
