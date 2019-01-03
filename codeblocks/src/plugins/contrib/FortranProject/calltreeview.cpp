#include "calltreeview.h"
#include "fortranproject.h"
#include "lineaddress.h"
#include "cbeditor.h"
#include <manager.h>
#include <editormanager.h>
#include <logmanager.h>
#include <configmanager.h>

#include <sdk.h>
#include <sdk_events.h>
#include <wx/sizer.h>

CTVData::CTVData(TokenF* token)
{
    if (token)
    {
        m_Filename  = token->m_Filename;
        m_LineStart = token->m_LineStart;
        m_TokenKind = token->m_TokenKind;
    }
}

namespace
{
    int idTree = wxNewId();
    int idMenuRefreshTree = wxNewId();
    int idMenuDoNotSort = wxNewId();
    int idMenuSortAlphabetically = wxNewId();

};

BEGIN_EVENT_TABLE(CallTreeView, wxPanel)
    EVT_TREE_ITEM_ACTIVATED(idTree, CallTreeView::OnTreeDoubleClick)
    EVT_TREE_ITEM_RIGHT_CLICK(idTree, CallTreeView::OnTreeItemRightClick)

    EVT_MENU(idMenuRefreshTree, CallTreeView::OnRefreshTree)
    EVT_MENU(idMenuDoNotSort, CallTreeView::OnChangeSort)
    EVT_MENU(idMenuSortAlphabetically, CallTreeView::OnChangeSort)
END_EVENT_TABLE()

CallTreeView::CallTreeView(wxWindow* parentWindow, FortranProject* forproj)
{

    this->Create(parentWindow);
    this->SetInitialSize(wxSize(200,100));
    this->SetMinSize(wxSize(200,100));

    m_pTree = new wxTreeCtrl(this, idTree, wxDefaultPosition, wxDefaultSize, wxTR_HAS_BUTTONS|wxTR_LINES_AT_ROOT|wxTR_HIDE_ROOT);
    m_pTree->SetImageList(m_ImgList.GetImageList());
    wxBoxSizer* bs = new wxBoxSizer(wxHORIZONTAL);
    bs->Add(m_pTree, 1, wxGROW | wxALL, 0);
    this->SetSizer(bs);

    m_pFortranProject = forproj;
    m_IsCallTree = true;
}

CallTreeView::~CallTreeView()
{
    //dtor
}

void CallTreeView::ShowCallTree(TokensArrayF* tokArr)
{
    RereadOptions();
    m_IsCallTree = true;
    m_pTree->DeleteAllItems();
    wxTreeItemId root = m_pTree->AddRoot(_("Call Tree"));

    ShowCallTreeChildren(tokArr, root, 0);
}

void CallTreeView::ShowCalledByTree(TokensArrayF* tokArr)
{
    RereadOptions();
    m_IsCallTree = false;
    m_pTree->DeleteAllItems();
    wxTreeItemId root = m_pTree->AddRoot(_("Called-By Tree"));

    ShowCallTreeChildren(tokArr, root, 0);
}

void CallTreeView::ShowCallTreeChildren(TokensArrayF* tokArr, wxTreeItemId& parent, int callLevel)
{
    callLevel += 1;
    for (size_t i=0; i<tokArr->size(); i++)
    {
        wxTreeItemData* tidata = new CTVData(tokArr->Item(i));
        int iind;
        if (callLevel == 1)
        {
            // show for first level different image
            if (m_IsCallTree)
            {
                if (tokArr->Item(i)->m_TokenKind == tkFunction)
                    iind = m_ImgList.GetImageIdx("function_call");
                else
                    iind = m_ImgList.GetImageIdx("subroutine_call");
            }
            else // CalledBy tree
            {
                if (tokArr->Item(i)->m_TokenKind == tkFunction)
                    iind = m_ImgList.GetImageIdx("function_calledby");
                else
                    iind = m_ImgList.GetImageIdx("subroutine_calledby");
            }
        }
        else
        {
            iind = m_ImgList.GetTokenKindImageIdx(tokArr->Item(i));
        }
        wxTreeItemId addedId = InsertTreeItem(parent, tokArr->Item(i)->m_DisplayName, iind, tidata);

        ShowCallTreeChildren(&tokArr->Item(i)->m_Children, addedId, callLevel);
        if (callLevel == 1)
            m_pTree->Expand(addedId);
    }
}

wxTreeItemId CallTreeView::InsertTreeItem(wxTreeItemId& parent, const wxString& displayName, int imageIdx, wxTreeItemData* tidata)
{
    if (!m_SortAlphabetically)
        return m_pTree->AppendItem(parent, displayName, imageIdx, -1, tidata);

    wxTreeItemId insertAfter;
    wxTreeItemIdValue cookie;
    wxTreeItemId item = m_pTree->GetFirstChild(parent, cookie);

    while (item.IsOk())
    {
        wxString itemText = m_pTree->GetItemText(item);
        if (displayName.CmpNoCase(itemText) >= 0)
        {
            insertAfter = item;
        }
        else
        {
            break;
        }
        item = m_pTree->GetNextChild(parent, cookie);
    }

    wxTreeItemId addedId;
    if (insertAfter.IsOk())
        addedId = m_pTree->InsertItem(parent, insertAfter, displayName, imageIdx, -1, tidata);
    else
        addedId = m_pTree->InsertItem(parent, 0, displayName, imageIdx, -1, tidata);

    return addedId;
}

void CallTreeView::OnTreeDoubleClick(wxTreeEvent& event)
{
    wxTreeItemId id = event.GetItem();
    if (!id.IsOk())
        return;

    CTVData* ctd = (CTVData*)m_pTree->GetItemData(id);
    if (!ctd)
        return;

    if (ctd->m_Filename == wxEmptyString)
        return;

    TokenFlat token;
    token.m_Filename = ctd->m_Filename;
    token.m_LineStart = ctd->m_LineStart;
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    m_pFortranProject->GotoToken(&token, ed);

    ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (ed)
    {
        wxFocusEvent ev(wxEVT_SET_FOCUS);
        ev.SetWindow(this);
#if wxCHECK_VERSION(3, 0, 0)
        ed->GetControl()->GetEventHandler()->AddPendingEvent(ev);
#else
        ed->GetControl()->AddPendingEvent(ev);
#endif
    }
}

void CallTreeView::ShowMenu(wxTreeItemId id, const wxPoint& pt)
{
    if ( !id.IsOk() )
        return;

#if wxUSE_MENUS
    wxString caption;
    wxMenu *menu=new wxMenu(wxEmptyString);

    menu->Append(idMenuRefreshTree, _("&Refresh tree"));

    menu->AppendCheckItem(idMenuDoNotSort, _("Do not sort"));
    menu->Check(idMenuDoNotSort, !m_SortAlphabetically);
    menu->AppendCheckItem(idMenuSortAlphabetically, _("Sort alphabetically"));
    menu->Check(idMenuSortAlphabetically, m_SortAlphabetically);

    if (menu->GetMenuItemCount() != 0)
        PopupMenu(menu);
    delete menu; // Prevents memory leak
#endif // wxUSE_MENUS
}

void CallTreeView::OnTreeItemRightClick(wxTreeEvent& event)
{
    wxTreeItemId idRoot = m_pTree->GetRootItem();
    if (!idRoot.IsOk())
        return;

    bool isFirstLevelItem = false;
    wxTreeItemIdValue cookie;
    wxTreeItemId item = m_pTree->GetFirstChild(idRoot, cookie);
    while (item.IsOk())
    {
        if (item == event.GetItem())
        {
            isFirstLevelItem = true;
            break;
        }
        item = m_pTree->GetNextChild(idRoot, cookie);
    }

    if (!isFirstLevelItem)
        return;

    m_pTree->SelectItem(event.GetItem());
    ShowMenu(event.GetItem(), event.GetPoint());
}

void CallTreeView::OnRefreshTree(wxCommandEvent& event)
{
    UpdateView();
}

void CallTreeView::OnChangeSort(wxCommandEvent& event)
{
    if (event.GetId() == idMenuDoNotSort)
        m_SortAlphabetically = !event.IsChecked();
    else if (event.GetId() == idMenuSortAlphabetically)
        m_SortAlphabetically = event.IsChecked();

    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));
    cfg->Write(_T("/calltree_sort_alphabetically"), m_SortAlphabetically);

    UpdateView();
}

void CallTreeView::RereadOptions()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));
    m_SortAlphabetically = cfg->ReadBool(_("/calltree_sort_alphabetically"), true);
}

void CallTreeView::UpdateView()
{

    wxTreeItemId id = m_pTree->GetSelection();
    if (!id.IsOk())
        return;

    CTVData* ctd = (CTVData*)m_pTree->GetItemData(id);
    if (!ctd)
        return;

    if (ctd->m_Filename == wxEmptyString)
        return;

    TokenFlat token;
    token.m_Filename = ctd->m_Filename;
    token.m_LineStart = ctd->m_LineStart;
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (!ed)
        return;
    m_pFortranProject->GotoToken(&token, ed);

    cbStyledTextCtrl* control = ed->GetControl();
    const int pos = control->GetCurrentPos();
    int curLine = control->LineFromPosition(pos);
    int lineStart;
    if (curLine == 0)
        lineStart = 0;
    else
        lineStart = control->GetLineEndPosition(curLine-1);
    int lineEnd = control->GetLineEndPosition(curLine);
    int wordStart = control->FindText(lineStart, lineEnd, m_pTree->GetItemText(id), wxSCI_FIND_WHOLEWORD);
    control->GotoPos(wordStart+1);

    m_pFortranProject->ShowCallTree(m_IsCallTree);
}
