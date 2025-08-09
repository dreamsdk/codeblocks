/*
 * This file is licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 */

#include "workspacebrowserf.h" // class's header file

#include <sdk.h>
#ifndef CB_PRECOMP
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
    #include <wx/tipwin.h>
    #include <wx/tokenzr.h>
    #include <wx/combobox.h>
    #include <wx/settings.h>
    #include <wx/choicdlg.h>

    #include <cbproject.h>
    #include <cbeditor.h>
    #include <configmanager.h>
    #include <editormanager.h>
    #include <globals.h>
    #include <manager.h>
    #include <pluginmanager.h>
    #include <projectmanager.h>
#endif

#include <cbstyledtextctrl.h>

#include "nativeparserf.h"
#include "fortranproject.h"

namespace {
    int idMenuJumpToImplementation = wxNewId();
    int idMenuRefreshTree = wxNewId();
    int idMenuForceReparse = wxNewId();
    int idMenuDoNotSort = wxNewId();
    int idMenuSortAlphabetically = wxNewId();
    int idMenuTopTree = wxNewId();
    int idMenuBottomTree = wxNewId();
    int idCmbSearch = wxNewId();
    int idBtnHome = wxNewId();
    int idCmbView = wxNewId();
}

BEGIN_EVENT_TABLE(WorkspaceBrowserF, wxPanel)
    // m_TreeBottom
    EVT_TREE_ITEM_ACTIVATED(idMenuBottomTree, WorkspaceBrowserF::OnTreeItemDoubleClick)
    EVT_TREE_ITEM_RIGHT_CLICK(idMenuBottomTree, WorkspaceBrowserF::OnTreeItemRightClick)
    // m_TreeTop
    EVT_TREE_ITEM_ACTIVATED(idMenuTopTree, WorkspaceBrowserF::OnTreeItemDoubleClick)
    EVT_TREE_ITEM_RIGHT_CLICK(idMenuTopTree, WorkspaceBrowserF::OnTreeItemRightClick)
    EVT_TREE_SEL_CHANGED(idMenuTopTree, WorkspaceBrowserF::OnTreeItemSelected)
    EVT_TREE_ITEM_EXPANDING(idMenuTopTree, WorkspaceBrowserF::OnTreeItemExpanding)
    EVT_TREE_ITEM_COLLAPSING(idMenuTopTree, WorkspaceBrowserF::OnTreeItemCollapsing)

    EVT_TEXT_ENTER(idCmbSearch, WorkspaceBrowserF::OnSearch)
    EVT_COMBOBOX(idCmbSearch, WorkspaceBrowserF::OnSearch)

    EVT_BUTTON(idBtnHome, WorkspaceBrowserF::OnMakeVisible)

    EVT_MENU(idMenuJumpToImplementation, WorkspaceBrowserF::OnJumpTo)
    EVT_MENU(idMenuRefreshTree, WorkspaceBrowserF::OnRefreshTree)
    EVT_MENU(idMenuForceReparse, WorkspaceBrowserF::OnForceReparse)
    EVT_CHOICE(idCmbView, WorkspaceBrowserF::OnViewScope)

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
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");

    m_BrowserOptions.visibleBottomTree = cfg->ReadBool("/visible_bottom_tree", true);
    m_BrowserOptions.sortAlphabetically = cfg->ReadBool("/browser_sort_alphabetically", true);
    m_BrowserOptions.showLocalVariables = cfg->ReadBool("/browser_show_local_variables", true);
    m_BrowserOptions.showIncludeSeparately = cfg->ReadBool("/browser_show_include_files_separately", true);

    Create(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, "WorkspaceBrowserF");
    CreateControlsWBF();

    if (platform::windows)
        m_Search->SetWindowStyle(wxTE_PROCESS_ENTER); // it's a must on windows to catch EVT_TEXT_ENTER

    int filter = cfg->ReadInt("/browser_display_filter", bdfWorkspace);
    m_CmbViewWBF->SetSelection(filter);
    m_BrowserOptions.displayFilter = (BrowserDisplayFilter)filter;

    // if the classbrowser is put under the control of a wxFlatNotebook,
    // somehow the main panel is like "invisible" :/
    // so we force the correct color for the panel here...
    m_WBFMainPanel->SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
}


void WorkspaceBrowserF::CreateControlsWBF()
{
    wxString prefix(ConfigManager::GetDataFolder()+"/FortranProject.zip#zip:images/");
#if wxCHECK_VERSION(3, 1, 6)
    prefix << "svg/";
    wxBitmapBundle bmp_makevisible = cbLoadBitmapBundleFromSVG(prefix + "fprojectmakevisible.svg", wxSize(16, 16));
#else
    const int imageSize = Manager::Get()->GetImageSize(Manager::UIComponent::Main);
    prefix << wxString::Format("%dx%d/", imageSize, imageSize);
    wxBitmap bmp_makevisible = cbLoadBitmap(prefix + "fprojectmakevisible.png");
#endif

    wxBoxSizer* BoxSizer1;
    wxBoxSizer* BoxSizer2;
    wxBoxSizer* BoxSizer3;
    wxBoxSizer* BoxSizer4;
    wxFlexGridSizer* FlexGridSizer1;
    wxStaticText* StaticText1;
    wxStaticText* StaticText2;

    BoxSizer1 = new wxBoxSizer(wxVERTICAL);
    m_WBFMainPanel = new wxPanel(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, "WBFMainPanel");
    BoxSizer2 = new wxBoxSizer(wxVERTICAL);
    FlexGridSizer1 = new wxFlexGridSizer(2, 2, 2, 2);
    FlexGridSizer1->AddGrowableCol(1);
    StaticText1 = new wxStaticText(m_WBFMainPanel, wxID_ANY, _("View:"), wxDefaultPosition, wxDefaultSize, 0, "ID_STATICTEXT1");
    FlexGridSizer1->Add(StaticText1, 1, wxALL|wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL, 5);
    BoxSizer3 = new wxBoxSizer(wxHORIZONTAL);
    m_CmbViewWBF = new wxChoice(m_WBFMainPanel, idCmbView, wxDefaultPosition, wxDefaultSize, 0, 0, 0, wxDefaultValidator, "cmbViewWBF");
    m_CmbViewWBF->Append(_("Current file\'s symbols"));
    m_CmbViewWBF->Append(_("Active project\'s symbols"));
    m_CmbViewWBF->SetSelection( m_CmbViewWBF->Append(_("All local symbols (workspace)")) );
    BoxSizer3->Add(m_CmbViewWBF, 1, wxEXPAND, 0);
    m_BtnHome = new wxBitmapButton(m_WBFMainPanel, idBtnHome, bmp_makevisible, wxDefaultPosition, wxDefaultSize, wxBU_AUTODRAW, wxDefaultValidator, "btnHome");
    m_BtnHome->SetDefault();
    BoxSizer3->Add(m_BtnHome, 0, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
    FlexGridSizer1->Add(BoxSizer3, 1, wxALL|wxEXPAND, 0);
    StaticText2 = new wxStaticText(m_WBFMainPanel, wxID_ANY, _("Search:"), wxDefaultPosition, wxDefaultSize, 0, "ID_STATICTEXT2");
    FlexGridSizer1->Add(StaticText2, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
    BoxSizer4 = new wxBoxSizer(wxHORIZONTAL);
    m_Search = new wxComboBox(m_WBFMainPanel, idCmbSearch, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 0, wxCB_DROPDOWN|wxTE_PROCESS_ENTER, wxDefaultValidator, "cmbSearchWBF");
    BoxSizer4->Add(m_Search, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
    FlexGridSizer1->Add(BoxSizer4, 1, wxALL|wxEXPAND, 0);
    BoxSizer2->Add(FlexGridSizer1, 0, wxALL|wxEXPAND, 4);
    m_SplitterWin = new wxSplitterWindow(m_WBFMainPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE, "splitterWinWBF");
    m_SplitterWin->SetSashGravity(0.5);
    m_TreeTop = new wxTreeCtrl(m_SplitterWin, idMenuTopTree, wxDefaultPosition, wxDefaultSize, wxTR_DEFAULT_STYLE, wxDefaultValidator, "treeAllWBF");
    m_TreeBottom = new wxTreeCtrl(m_SplitterWin, idMenuBottomTree, wxDefaultPosition, wxDefaultSize, wxTR_HIDE_ROOT|wxTR_DEFAULT_STYLE, wxDefaultValidator, "treeMembersWBF");
    m_SplitterWin->SplitHorizontally(m_TreeTop, m_TreeBottom);
    BoxSizer2->Add(m_SplitterWin, 1, wxALL|wxEXPAND, 0);
    m_WBFMainPanel->SetSizer(BoxSizer2);
    BoxSizer2->Fit(m_WBFMainPanel);
    BoxSizer2->SetSizeHints(m_WBFMainPanel);
    BoxSizer1->Add(m_WBFMainPanel, 1, wxALL|wxEXPAND, 0);
    SetSizer(BoxSizer1);
    BoxSizer1->Fit(this);
    BoxSizer1->SetSizeHints(this);
}

// class destructor
WorkspaceBrowserF::~WorkspaceBrowserF()
{
    int pos = m_SplitterWin->GetSashPosition();
    Manager::Get()->GetConfigManager("fortran_project")->Write("/splitter_pos", pos);
    int filter = m_CmbViewWBF->GetSelection();
    Manager::Get()->GetConfigManager("fortran_project")->Write("/browser_display_filter", filter);

    if (m_pBrowserBuilder)
    {
        delete m_pBrowserBuilder;
    }
}

void WorkspaceBrowserF::UpdateSash()
{
    int pos = Manager::Get()->GetConfigManager("fortran_project")->ReadInt("/splitter_pos", 250);
    m_SplitterWin->SetSashPosition(pos, false);
}

void WorkspaceBrowserF::UpdateView()
{
    m_pActiveProject = 0;
    m_ActiveFilename.Clear();
    if (Manager::IsAppShuttingDown())
        return;

    if (m_pParser)
    {
        m_pActiveProject = Manager::Get()->GetProjectManager()->GetActiveProject();
        cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
        if (ed)
            m_ActiveFilename = ed->GetFilename();

        if (m_pActiveProject)
            BuildTree();
        else if (m_pBrowserBuilder)
            m_pBrowserBuilder->DeleteTopRootChildren();
    }
    else
        m_TreeTop->DeleteAllItems();

}

void WorkspaceBrowserF::ShowMenu(wxTreeCtrl* tree, wxTreeItemId id, cb_unused const wxPoint& pt)
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
            menu->Append(idMenuJumpToImplementation, _("Go to &implementation"));
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
        wxString filename;
        int gotoLine;
        if (!pToken->m_WasIncluded || pToken->m_TokenKind == tkModule)
        {
            filename = pToken->m_Filename;
            gotoLine = pToken->m_LineStart;
        }
        else
        {
            // if token was included and not a module
            filename = pToken->m_IncludeFilename;
            gotoLine = pToken->m_IncludeLineStart;
        }

        EditorManager* edMan = Manager::Get()->GetEditorManager();
        if (cbEditor* ed = edMan->Open(filename))
        {
            ed->GotoLine(gotoLine - 1);
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
            cbMessageBox(wxString::Format(_("Declaration not found: %s"), pToken->m_DisplayName), _("Warning"), wxICON_WARNING);
        }
    }
}

void WorkspaceBrowserF::OnJumpTo(cb_unused wxCommandEvent& event)
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

void WorkspaceBrowserF::OnRefreshTree(cb_unused wxCommandEvent& event)
{
    UpdateView();
}

void WorkspaceBrowserF::OnForceReparse(cb_unused wxCommandEvent& event)
{
    if (m_NativeParser)
    {
        m_NativeParser->MakeAIncludeFileList();
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
            wxString pFN;
            if (m_pActiveProject)
                pFN = m_pActiveProject->GetFilename();
            m_NativeParser->ReparseFile(pFN, m_ActiveFilename);
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

    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");
    cfg->Write("/browser_sort_alphabetically",m_BrowserOptions.sortAlphabetically);
}

void WorkspaceBrowserF::OnChangeMode(wxCommandEvent& event)
{
    if (event.GetId() == idMenuBottomTree)
    {
        m_BrowserOptions.visibleBottomTree = event.IsChecked();

        ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");
        cfg->Write("/visible_bottom_tree", m_BrowserOptions.visibleBottomTree);
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

void WorkspaceBrowserF::OnSearch(cb_unused wxCommandEvent& event)
{
    wxString search = m_Search->GetValue().Trim(true).Trim(false);
    if (search.IsEmpty())
        return;

    if (search.length() > 100)
    {
        // Very long string. Such names are not allowed in Fortran.
        search = search.Mid(0, 100);
    }
    TokenF* token = 0;
    TokensArrayF result;
    size_t count = FindMatchTokens(search, result);

    size_t j=0;
    while (j < count)
    {
        if ((result.Item(j)->m_TokenKind == tkVariable) ||
            (result.Item(j)->m_TokenKind == tkCallFunction) ||
            (result.Item(j)->m_TokenKind == tkCallSubroutine))
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
            inf << "::" << result.Item(i)->GetTokenKindString() << ", " << fn.GetFullName() << " : ";
            inf << wxString::Format("%d", int(result.Item(i)->m_LineStart));
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

    if (m_BrowserOptions.visibleBottomTree)
    {
        m_SplitterWin->SplitHorizontally(m_TreeTop, m_TreeBottom);
        m_TreeBottom->Show(true);
    }
    else
    {
        m_SplitterWin->Unsplit();
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

void WorkspaceBrowserF::SelectSymbol(wxString filename, int line)
{
    if (m_pBrowserBuilder)
    {
        m_pBrowserBuilder->SelectSymbol(filename, line);
    }
}

void WorkspaceBrowserF::RereadOptions()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");
    if (cfg->ReadBool(_("/use_symbols_browser"), true))
    {
        m_BrowserOptions.visibleBottomTree = cfg->ReadBool("/visible_bottom_tree", true);
        m_BrowserOptions.sortAlphabetically = cfg->ReadBool("/browser_sort_alphabetically", true);
        m_BrowserOptions.showLocalVariables = cfg->ReadBool("/browser_show_local_variables", true);
        m_BrowserOptions.showIncludeSeparately = cfg->ReadBool("/browser_show_include_files_separately", true);
        UpdateView();
    }
}

void WorkspaceBrowserF::DeleteAllItems()
{
    m_TreeTop->DeleteAllItems();
    m_TreeBottom->DeleteAllItems();
}

void WorkspaceBrowserF::OnMakeVisible(cb_unused wxCommandEvent& event)
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


