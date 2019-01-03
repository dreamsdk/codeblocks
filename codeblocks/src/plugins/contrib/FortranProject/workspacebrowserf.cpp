/*
 * This file is licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 */

#include <sdk.h>
#include "workspacebrowserf.h" // class's header file
#include "nativeparserf.h"
#include "fortranproject.h"
#include <wx/intl.h>
#include <wx/treectrl.h>
#include <wx/listctrl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/choice.h>
#include <wx/menu.h>
#include <wx/splitter.h>
#include <wx/button.h>
#include <wx/utils.h> // wxBusyCursor
#include <wx/xrc/xmlres.h>
#include <wx/tipwin.h>
#include <wx/tokenzr.h>
#include <manager.h>
#include <configmanager.h>
#include <pluginmanager.h>
#include <editormanager.h>
#include <projectmanager.h>
#include <cbeditor.h>
#include <globals.h>
#include "cbstyledtextctrl.h"

#ifndef CB_PRECOMP
#include "cbproject.h"
#endif


int idMenuJumpToImplementation = wxNewId();
int idMenuRefreshTree = wxNewId();
int idMenuForceReparse = wxNewId();
int idMenuDoNotSort = wxNewId();
int idMenuSortAlphabetically = wxNewId();
int idMenuBottomTree = wxNewId();

BEGIN_EVENT_TABLE(WorkspaceBrowserF, wxPanel)
    // m_TreeBottom
    EVT_TREE_ITEM_ACTIVATED(XRCID("treeMembersWBF"), WorkspaceBrowserF::OnTreeItemDoubleClick)
    EVT_TREE_ITEM_RIGHT_CLICK(XRCID("treeMembersWBF"), WorkspaceBrowserF::OnTreeItemRightClick)
    // m_TreeTop
    EVT_TREE_ITEM_ACTIVATED(XRCID("treeAllWBF"), WorkspaceBrowserF::OnTreeItemDoubleClick)
    EVT_TREE_ITEM_RIGHT_CLICK(XRCID("treeAllWBF"), WorkspaceBrowserF::OnTreeItemRightClick)
    EVT_TREE_SEL_CHANGED(XRCID("treeAllWBF"), WorkspaceBrowserF::OnTreeItemSelected)
    EVT_TREE_ITEM_EXPANDING(XRCID("treeAllWBF"), WorkspaceBrowserF::OnTreeItemExpanding)
    EVT_TREE_ITEM_COLLAPSING(XRCID("treeAllWBF"), WorkspaceBrowserF::OnTreeItemCollapsing)

    EVT_TEXT_ENTER(XRCID("cmbSearchWBF"), WorkspaceBrowserF::OnSearch)
    EVT_COMBOBOX(XRCID("cmbSearchWBF"), WorkspaceBrowserF::OnSearch)

    EVT_BUTTON(XRCID("btnHome"), WorkspaceBrowserF::OnMakeVisible)

    EVT_MENU(idMenuJumpToImplementation, WorkspaceBrowserF::OnJumpTo)
    EVT_MENU(idMenuRefreshTree, WorkspaceBrowserF::OnRefreshTree)
    EVT_MENU(idMenuForceReparse, WorkspaceBrowserF::OnForceReparse)
    EVT_CHOICE(XRCID("cmbViewWBF"), WorkspaceBrowserF::OnViewScope)
    EVT_BUTTON(XRCID("btnSearch"), WorkspaceBrowserF::OnSearch)

    EVT_MENU(idMenuDoNotSort, WorkspaceBrowserF::OnChangeSort)
    EVT_MENU(idMenuSortAlphabetically, WorkspaceBrowserF::OnChangeSort)
    EVT_MENU(idMenuBottomTree, WorkspaceBrowserF::OnChangeMode)
END_EVENT_TABLE()

// class constructor
WorkspaceBrowserF::WorkspaceBrowserF(wxWindow* parent, NativeParserF* np, ParserF* par)
    : m_NativeParser(np),
      m_TreeForPopupMenu(0),
      m_pParser(par),
      m_pActiveProject(0),
      m_pBrowserBuilder(0)
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));

    m_BrowserOptions.visibleBottomTree = cfg->ReadBool(_T("/visible_bottom_tree"), true);
    m_BrowserOptions.sortAlphabetically = cfg->ReadBool(_T("/browser_sort_alphabetically"), true);
    m_BrowserOptions.showLocalVariables = cfg->ReadBool(_T("/browser_show_local_variables"), true);
    m_BrowserOptions.showIncludeSeparately = cfg->ReadBool(_T("/browser_show_include_files_separately"), true);

    wxXmlResource::Get()->LoadPanel(this, parent, _T("pnlWBF"));
    m_Search = XRCCTRL(*this, "cmbSearchWBF", wxComboBox);

    if (platform::windows)
        m_Search->SetWindowStyle(wxTE_PROCESS_ENTER); // it's a must on windows to catch EVT_TEXT_ENTER

    m_TreeTop = XRCCTRL(*this, "treeAllWBF", wxTreeCtrl);
    m_TreeBottom = XRCCTRL(*this, "treeMembersWBF", wxTreeCtrl);

    int filter = cfg->ReadInt(_T("/browser_display_filter"), bdfWorkspace);
    XRCCTRL(*this, "cmbViewWBF", wxChoice)->SetSelection(filter);
    m_BrowserOptions.displayFilter = (BrowserDisplayFilter)filter;

    // if the classbrowser is put under the control of a wxFlatNotebook,
    // somehow the main panel is like "invisible" :/
    // so we force the correct color for the panel here...
    XRCCTRL(*this, "WBFMainPanel", wxPanel)->SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
}

// class destructor
WorkspaceBrowserF::~WorkspaceBrowserF()
{
    int pos = XRCCTRL(*this, "splitterWinWBF", wxSplitterWindow)->GetSashPosition();
    Manager::Get()->GetConfigManager(_T("fortran_project"))->Write(_T("/splitter_pos"), pos);
    int filter = XRCCTRL(*this, "cmbViewWBF", wxChoice)->GetSelection();
    Manager::Get()->GetConfigManager(_T("fortran_project"))->Write(_T("/browser_display_filter"), filter);

    if (m_pBrowserBuilder)
    {
        delete m_pBrowserBuilder;
    }
}

void WorkspaceBrowserF::UpdateSash()
{
    int pos = Manager::Get()->GetConfigManager(_T("fortran_project"))->ReadInt(_T("/splitter_pos"), 250);
    XRCCTRL(*this, "splitterWinWBF", wxSplitterWindow)->SetSashPosition(pos, false);
}

void WorkspaceBrowserF::UpdateView()
{
    m_pActiveProject = 0;
    m_ActiveFilename.Clear();
    if (Manager::IsAppShuttingDown())
    {
        return;
    }
    if (m_pParser)
    {
        m_pActiveProject = Manager::Get()->GetProjectManager()->GetActiveProject();
        cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
        if (ed)
        {
            m_ActiveFilename = ed->GetFilename();
        }
        if (m_pActiveProject)
            BuildTree();
        else if (m_pBrowserBuilder)
        {
            m_pBrowserBuilder->DeleteTopRootChildren();
        }
    }
    else
        m_TreeTop->DeleteAllItems();

}

void WorkspaceBrowserF::ShowMenu(wxTreeCtrl* tree, wxTreeItemId id, const wxPoint& pt)
{
// NOTE: local variables are tricky! If you build two local menus
// and attach menu B to menu A, on function exit both menu A and menu B
// will be destroyed. But when destroying menu A, menu B will be destroyed
// again. Its already-freed memory will be accessed, generating a segfault.

// A safer approach is to make all menus heap-based, and delete the topmost
// on exit.

    m_TreeForPopupMenu = tree;
    if ( !id.IsOk() )
        return;

#if wxUSE_MENUS
    wxString caption;
    wxMenu *menu=new wxMenu(wxEmptyString);

    TreeDataF* ctd = (TreeDataF*)tree->GetItemData(id);
    if (ctd)
    {
        if (ctd->m_SpecialFolder==sfToken)
        {
            menu->Append(idMenuJumpToImplementation, _("Jump to &implementation"));
        }
    }

    if (tree == m_TreeTop)
    {
        // only in top tree
        if (menu->GetMenuItemCount() != 0)
            menu->AppendSeparator();

        menu->Append(idMenuRefreshTree, _("&Refresh tree"));

        if (id == m_TreeTop->GetRootItem())
        {
            menu->Append(idMenuForceReparse, _("Re-&parse now"));
        }
    }
    menu->AppendSeparator();
    menu->AppendCheckItem(idMenuDoNotSort, _("Do not sort"));
    menu->Check(idMenuDoNotSort, !m_BrowserOptions.sortAlphabetically);
    menu->AppendCheckItem(idMenuSortAlphabetically, _("Sort alphabetically"));
    menu->Check(idMenuSortAlphabetically, m_BrowserOptions.sortAlphabetically);

    menu->AppendSeparator();
    menu->AppendCheckItem(idMenuBottomTree, _("Display bottom tree"));
    menu->Check(idMenuBottomTree, m_BrowserOptions.visibleBottomTree);


    if (menu->GetMenuItemCount() != 0)
        PopupMenu(menu);
    delete menu; // Prevents memory leak
#endif // wxUSE_MENUS
}

// events

void WorkspaceBrowserF::OnTreeItemRightClick(wxTreeEvent& event)
{
    wxTreeCtrl* tree = (wxTreeCtrl*)event.GetEventObject();
    tree->SelectItem(event.GetItem());
    ShowMenu(tree, event.GetItem(), event.GetPoint());// + tree->GetPosition());
}

void WorkspaceBrowserF::JumpToToken(TokenF* pToken)
{
    if (pToken)
    {
        LineAddress jumpStart;
        LineAddress jumpFinish;
        if(cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor())
        {
            cbStyledTextCtrl* control = ed->GetControl();
            int curLine = control->LineFromPosition(control->GetCurrentPos());
            jumpStart.Init(ed->GetFilename(), curLine, false);
        }
        EditorManager* edMan = Manager::Get()->GetEditorManager();
        if (cbEditor* ed = edMan->Open(pToken->m_Filename))
        {
            ed->GotoLine(pToken->m_LineStart - 1);
            wxFocusEvent ev(wxEVT_SET_FOCUS);
            ev.SetWindow(this);
#if wxCHECK_VERSION(3, 0, 0)
            ed->GetControl()->GetEventHandler()->AddPendingEvent(ev);
#else
            ed->GetControl()->AddPendingEvent(ev);
#endif

            // Track jump history
            cbStyledTextCtrl* control = ed->GetControl();
            int curLine = control->LineFromPosition(control->GetCurrentPos());
            jumpFinish.Init(ed->GetFilename(), curLine, true);

            m_NativeParser->GetJumpTracker()->TakeJump(jumpStart, jumpFinish);
            m_NativeParser->GetFortranProject()->CheckEnableToolbar();
        }
        else
        {
            cbMessageBox(wxString::Format(_("Declaration not found: %s"), pToken->m_DisplayName.c_str()), _("Warning"), wxICON_WARNING);
        }
    }
}

void WorkspaceBrowserF::OnJumpTo(wxCommandEvent& event)
{
    wxTreeCtrl* tree = m_TreeForPopupMenu;
    wxTreeItemId id = tree->GetSelection();
    TreeDataF* ctd = (TreeDataF*)tree->GetItemData(id);
    if (ctd && ctd->m_pToken)
    {
        JumpToToken(ctd->m_pToken);
    }
}

void WorkspaceBrowserF::OnTreeItemDoubleClick(wxTreeEvent& event)
{
    wxTreeCtrl* tree = (wxTreeCtrl*)event.GetEventObject();
    wxTreeItemId id = event.GetItem();
    TreeDataF* ctd = (TreeDataF*)tree->GetItemData(id);
    if (ctd && ctd->m_pToken)
    {
        JumpToToken(ctd->m_pToken);
    }
}

void WorkspaceBrowserF::OnRefreshTree(wxCommandEvent& event)
{
    UpdateView();
}

void WorkspaceBrowserF::OnForceReparse(wxCommandEvent& event)
{
    if (m_NativeParser)
    {
        switch (m_BrowserOptions.displayFilter)
        {
        case bdfWorkspace:
        {
            m_NativeParser->ForceReparseWorkspace();
            break;
        }
        case bdfProject:
        {
// NOTE (darius#1#): Force reparse workspace, just because currently only the workspace parsing is running on a secondary thread.
            //m_NativeParser->ReparseProject(m_pActiveProject);
            m_NativeParser->ForceReparseWorkspace();
            UpdateView();
            break;
        }
        case bdfFile:
        {
            m_NativeParser->ReparseFile(m_ActiveFilename);
            UpdateView();
            break;
        }
        }
    }
}

void WorkspaceBrowserF::OnViewScope(wxCommandEvent& event)
{
    m_BrowserOptions.displayFilter = (BrowserDisplayFilter)event.GetSelection();
    UpdateView();
}

void WorkspaceBrowserF::OnChangeSort(wxCommandEvent& event)
{
    if (event.GetId() == idMenuDoNotSort)
        m_BrowserOptions.sortAlphabetically = !event.IsChecked();
    else if (event.GetId() == idMenuSortAlphabetically)
        m_BrowserOptions.sortAlphabetically = event.IsChecked();
    UpdateView();

    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));
    cfg->Write(_T("/browser_sort_alphabetically"),m_BrowserOptions.sortAlphabetically);
}

void WorkspaceBrowserF::OnChangeMode(wxCommandEvent& event)
{
    if (event.GetId() == idMenuBottomTree)
    {
        m_BrowserOptions.visibleBottomTree = event.IsChecked();

        ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));
        cfg->Write(_T("/visible_bottom_tree"), m_BrowserOptions.visibleBottomTree);
    }
    UpdateView();
}

size_t WorkspaceBrowserF::FindMatchTokens(wxString search, TokensArrayF& result)
{
    size_t count=0;
    switch (m_BrowserOptions.displayFilter)
    {
    case bdfFile:
    {
        count = m_pParser->FindMatchTokens(m_ActiveFilename, search, result);
        break;
    }
    case bdfProject:
    {
        for (FilesList::iterator it = m_pActiveProject->GetFilesList().begin(); it != m_pActiveProject->GetFilesList().end(); ++it)
        {
            ProjectFile* pf = *it;
            count = m_pParser->FindMatchTokens(pf->file.GetFullPath(), search, result);
        }
        break;
    }
    case bdfWorkspace:
    {
        ProjectsArray* projects = Manager::Get()->GetProjectManager()->GetProjects();
        for (size_t i=0; i < projects->GetCount(); ++i)
        {
            cbProject* project = projects->Item(i);
            for (FilesList::iterator it = project->GetFilesList().begin(); it != project->GetFilesList().end(); ++it)
            {
                ProjectFile* pf = *it;
                count = m_pParser->FindMatchTokens(pf->file.GetFullPath(), search, result);
            }
        }
        break;
    }
    }
    return count;
}

void WorkspaceBrowserF::OnSearch(wxCommandEvent& event)
{
    wxString search = m_Search->GetValue();
    if (search.IsEmpty())
        return;

    TokenF* token = 0;
    TokensArrayF result;
    size_t count = FindMatchTokens(search, result);

    size_t j=0;
    while (j < count)
    {
        if (result.Item(j)->m_TokenKind == tkVariable)
        {
            result.RemoveAt(j);
            count--;
        }
        else
        {
            j++;
        }
    }

    if (count == 0)
    {
        cbMessageBox(_("No matches were found: ") + search, _("Search failed"));
        return;
    }
    else if (count == 1)
    {
        token = *result.begin();
    }
    else if (count > 1)
    {
        wxArrayString selections;
        for (size_t i=0; i<count; ++i)
        {
            wxString inf = result.Item(i)->m_DisplayName;;
            wxFileName fn = wxFileName(result.Item(i)->m_Filename);
            inf << _T("::") << result.Item(i)->GetTokenKindString() << _T(", ") << fn.GetFullName() << _T(" : ");
            inf << wxString::Format(_T("%d"), int(result.Item(i)->m_LineStart));
            selections.Add(inf);
        }
        int sel = wxGetSingleChoiceIndex(_("Please make a selection:"), _("Multiple matches"), selections);
        if (sel == -1)
            return;
        token = result.Item(sel);
    }

    // store the search in the combobox
    if (m_Search->FindString(token->m_DisplayName) == wxNOT_FOUND)
        m_Search->Append(token->m_DisplayName);

    JumpToToken(token);
    m_pBrowserBuilder->SelectItem(token);
}

void WorkspaceBrowserF::BuildTree()
{
    if (Manager::IsAppShuttingDown())
        return;

    // create the WorkspaceBrowserBuilder if needed
    if (!m_pBrowserBuilder)
    {
        m_pBrowserBuilder = new WorkspaceBrowserBuilder(m_pParser, m_TreeTop, m_TreeBottom);
    }

    wxSplitterWindow* splitter = XRCCTRL(*this, "splitterWinWBF", wxSplitterWindow);
    if (m_BrowserOptions.visibleBottomTree)
    {
        splitter->SplitHorizontally(m_TreeTop, m_TreeBottom);
        m_TreeBottom->Show(true);
    }
    else
    {
        splitter->Unsplit();
        m_TreeBottom->Show(false);
    }

    // build tree
    m_pBrowserBuilder->Init(
        m_ActiveFilename,
        m_pActiveProject,
        m_BrowserOptions);
} // end of BuildTree

void WorkspaceBrowserF::OnTreeItemSelected(wxTreeEvent& event)
{
    if (Manager::IsAppShuttingDown())
        return;

    if (m_pBrowserBuilder)
    {
        if (!m_pBrowserBuilder->SelectNode(event.GetItem()))
            return;
    }
    event.Allow();

    EditorManager* edMan = Manager::Get()->GetEditorManager();
    if (!edMan)
        return;
    cbEditor* ed = edMan->GetBuiltinActiveEditor();
    if (!ed)
        return;
    cbStyledTextCtrl* control = ed->GetControl();
    int currentLine = control->GetCurrentLine() + 1;
    wxString activeFilename = ed->GetFilename();
    if (activeFilename.IsEmpty())
        return;
    MarkSymbol(UnixFilename(activeFilename), currentLine);
}

void WorkspaceBrowserF::OnTreeItemExpanding(wxTreeEvent& event)
{
    if (Manager::IsAppShuttingDown())
        return;

    m_pBrowserBuilder->ExpandTopNode(event.GetItem());
}

void WorkspaceBrowserF::OnTreeItemCollapsing(wxTreeEvent& event)
{
    m_pBrowserBuilder->CollapsTopNode(event.GetItem());
    //event.Allow();
}

wxImageList* WorkspaceBrowserF::GetImageList()
{
    if (m_pBrowserBuilder)
        return m_pBrowserBuilder->GetImageList();
    return 0;
}

int WorkspaceBrowserF::GetTokenKindImageIdx(TokenF* token)
{
    if (m_pBrowserBuilder)
        return m_pBrowserBuilder->GetTokenKindImageIdx(token);
    return 0;
}

void WorkspaceBrowserF::MarkSymbol(wxString filename, int line)
{
    if (m_pBrowserBuilder)
    {
        m_pBrowserBuilder->MarkSymbol(filename, line);
    }
}

void WorkspaceBrowserF::RereadOptions()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));
    if (cfg->ReadBool(_("/use_symbols_browser"), true))
    {
        m_BrowserOptions.visibleBottomTree = cfg->ReadBool(_("/visible_bottom_tree"), true);
        m_BrowserOptions.sortAlphabetically = cfg->ReadBool(_("/browser_sort_alphabetically"), true);
        m_BrowserOptions.showLocalVariables = cfg->ReadBool(_T("/browser_show_local_variables"), true);
        m_BrowserOptions.showIncludeSeparately = cfg->ReadBool(_T("/browser_show_include_files_separately"), true);
        UpdateView();
    }
}

void WorkspaceBrowserF::DeleteAllItems()
{
    m_TreeTop->DeleteAllItems();
    m_TreeBottom->DeleteAllItems();
}

void WorkspaceBrowserF::OnMakeVisible(wxCommandEvent& event)
{
    if (m_pBrowserBuilder)
    {
        m_pBrowserBuilder->MakeVisibleCurrent();
    }
}

void WorkspaceBrowserF::OnMenuEditPaste(wxCommandEvent& event)
{
    wxWindow* pFocused = wxWindow::FindFocus();
    if (!pFocused)
    {
        event.Skip();
        return;
    }

    if (pFocused == m_Search)
        m_Search->Paste();
    else
        event.Skip();

    return;
}

void WorkspaceBrowserF::SetActiveProject(cbProject* prj)
{
    m_pActiveProject = prj;
    if (m_pBrowserBuilder)
    {
        m_pBrowserBuilder->SetActiveProject(prj);
    }
}


