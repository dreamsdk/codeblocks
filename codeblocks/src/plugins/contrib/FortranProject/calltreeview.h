#ifndef CALLTREEVIEW_H
#define CALLTREEVIEW_H

#include <wx/panel.h>
#include <wx/treectrl.h>
#include "tokenf.h"
#include "fpimagelist.h"
#include "jumptracker.h"

class FortranProject;

class CTVData : public wxTreeItemData
{
    public:
        CTVData(TokenF* token);

        wxString m_Filename;
        unsigned int m_LineStart;
        TokenKindF m_TokenKind;
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
        void ShowMenu(wxTreeItemId id, const wxPoint& pt);
        void OnTreeItemRightClick(wxTreeEvent& event);
        void OnRefreshTree(wxCommandEvent& event);
        void OnChangeSort(wxCommandEvent& event);
        void RereadOptions();
        void UpdateView();

        wxTreeCtrl* m_pTree;
        FPImageList m_ImgList;
        FortranProject* m_pFortranProject;
        bool m_IsCallTree;
        bool m_SortAlphabetically;

    DECLARE_EVENT_TABLE()
};

#endif // CALLTREEVIEW_H
