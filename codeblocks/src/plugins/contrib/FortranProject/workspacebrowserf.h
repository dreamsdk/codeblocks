/*
 * This file is licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef WORKSPACEBROWSERF_H
#define WORKSPACEBROWSERF_H

#include <settings.h> // SDK
#include <cbplugin.h>
#include <manager.h>
#include <wx/panel.h>
#include "workspacebrowserbuilder.h"

class NativeParserF;
class wxComboBox;
class wxTreeCtrl;
class wxTextCtrl;
class cbProject;

class WorkspaceBrowserF : public wxPanel
{
    public:
        // class constructor
        WorkspaceBrowserF(wxWindow* parent, NativeParserF* np, ParserF* par);
        // class destructor
        ~WorkspaceBrowserF();
        BrowserDisplayFilter GetBrowserDisplayFilter() { return m_BrowserOptions.displayFilter; }
        void UpdateSash();
        void UpdateView();
        wxImageList* GetImageList();
        int GetTokenKindImageIdx(TokenF* token);
        void MarkSymbol(wxString filename, int line);
        void RereadOptions();
        void DeleteAllItems();
        void OnMenuEditPaste(wxCommandEvent& event);
        wxString GetActiveFilename() { return m_ActiveFilename; };
        void SetActiveProject(cbProject* prj);

    private:
        friend class myTextCtrl;
        void OnTreeItemDoubleClick(wxTreeEvent& event);
        void OnTreeItemRightClick(wxTreeEvent& event);
        void OnJumpTo(wxCommandEvent& event);
        void OnRefreshTree(wxCommandEvent& event);
        void OnForceReparse(wxCommandEvent& event);
        void OnViewScope(wxCommandEvent& event);
        void JumpToToken(TokenF* token);
        void OnChangeSort(wxCommandEvent& event);
        void OnChangeMode(wxCommandEvent& event);

        void OnMakeVisible(wxCommandEvent& event);
        void OnSearch(wxCommandEvent& event);
        size_t FindMatchTokens(wxString search, TokensArrayF& result);

        void ShowMenu(wxTreeCtrl* tree, wxTreeItemId id, const wxPoint& pt);

        void BuildTree();

        void OnTreeItemSelected(wxTreeEvent& event);
        void OnTreeItemExpanding(wxTreeEvent& event);
        void OnTreeItemCollapsing(wxTreeEvent& event);

        NativeParserF* m_NativeParser;
        wxTreeCtrl* m_TreeTop;
        wxTreeCtrl* m_TreeBottom;
        wxComboBox* m_Search;
        wxTreeCtrl* m_TreeForPopupMenu;
        ParserF* m_pParser;
        wxTreeItemId m_RootNode;

        // filtering
        wxString m_ActiveFilename;
        cbProject* m_pActiveProject;

        WorkspaceBrowserBuilder* m_pBrowserBuilder;

        BrowserOptions m_BrowserOptions;

        DECLARE_EVENT_TABLE()
};

#endif // WORKSPACEBROWSERF_H

