#include "calltreeview.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/sizer.h>
    #include <wx/xrc/xmlres.h>
    #include <wx/menu.h>

    #include <sdk_events.h>
    #include <manager.h>
    #include <editormanager.h>
    #include <logmanager.h>
    #include <configmanager.h>
    #include <cbeditor.h>
#endif
#include <cmath>

#include <cbstyledtextctrl.h>

#include "fortranproject.h"
#include "lineaddress.h"

CallTreeToken::CallTreeToken(TokenFlat* tf, CallTreeToken* parent)
{
    m_TokenKind   = tf->m_TokenKind;
    m_DisplayName = tf->m_DisplayName;
    m_Name        = tf->m_Name;
    m_Filename    = tf->m_Filename;
    m_LineStart   = tf->m_LineStart;
    m_LineEnd     = tf->m_LineEnd;
    m_TokenAccess = tf->m_TokenAccess;

    m_pParent     = parent;
    wereChildrenConnnected = false;
}

CallTreeToken::CallTreeToken(TokenF* tf, CallTreeToken* parent)
{
    m_TokenKind   = tf->m_TokenKind;
    m_DisplayName = tf->m_DisplayName;
    m_Name        = tf->m_Name;
    m_Filename    = tf->m_Filename;
    m_LineStart   = tf->m_LineStart;
    m_LineEnd     = tf->m_LineEnd;
    m_TokenAccess = tf->m_TokenAccess;

    m_pParent     = parent;
    wereChildrenConnnected = false;
}

CTVData::CTVData(TokenF* token)
{
    if (token)
    {
        CallTreeToken* cttok = static_cast<CallTreeToken*>(token);
        m_DefFilename  = cttok->m_Filename;
        m_DefLineStart = cttok->m_LineStart;
        m_DefTokenKind = cttok->m_TokenKind;

        m_CallFilename = cttok->m_CallFilename;
        m_CallLine     = cttok->m_CallLine;
    }
}

namespace
{
    int idMenuRefreshTree = wxNewId();
    int idMenuDoNotSort = wxNewId();
    int idMenuSortAlphabetically = wxNewId();
    int idMenuGoToProcedure = wxNewId();
    int idMenuGoToCall = wxNewId();
};

BEGIN_EVENT_TABLE(CallTreeView, wxPanel)
    EVT_TREE_ITEM_ACTIVATED(XRCID("treeCallTreeView"), CallTreeView::OnTreeDoubleClick)
    EVT_TREE_ITEM_RIGHT_CLICK(XRCID("treeCallTreeView"), CallTreeView::OnTreeItemRightClick)

    EVT_MENU(idMenuRefreshTree, CallTreeView::OnRefreshTree)
    EVT_MENU(idMenuDoNotSort, CallTreeView::OnChangeSort)
    EVT_MENU(idMenuSortAlphabetically, CallTreeView::OnChangeSort)
    EVT_MENU(idMenuGoToProcedure, CallTreeView::OnGoToProcedure)
    EVT_MENU(idMenuGoToCall, CallTreeView::OnGoToCall)
END_EVENT_TABLE()

CallTreeView::CallTreeView(wxWindow* parentWindow, FortranProject* forproj)
{
    wxXmlResource::Get()->LoadPanel(this, parentWindow, "pnlCallTreeView");
    m_pTree = XRCCTRL(*this, "treeCallTreeView", wxTreeCtrl);

    int targetHeight = floor(16 * cbGetActualContentScaleFactor(*parentWindow));
    m_pImgList = new FPImageList(targetHeight);
    m_pTree->SetImageList(m_pImgList->GetWxImageList());

    m_pFortranProject = forproj;
    m_IsCallTree = true;
}

CallTreeView::~CallTreeView()
{
    //dtor
    delete m_pImgList;
}

void CallTreeView::ShowCallTree(TokensArrayF* tokArr)
{
    RereadOptions();
    m_IsCallTree = true;
    m_pTree->DeleteAllItems();
    wxTreeItemId root = m_pTree->AddRoot(_("Call tree"));

    ShowCallTreeChildren(tokArr, root, 0);
}

void CallTreeView::ShowCalledByTree(TokensArrayF* tokArr)
{
    RereadOptions();
    m_IsCallTree = false;
    m_pTree->DeleteAllItems();
    wxTreeItemId root = m_pTree->AddRoot(_("Called-By tree"));

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
                    iind = m_pImgList->GetImageIdx("function_call");
                else
                    iind = m_pImgList->GetImageIdx("subroutine_call");
            }
            else // CalledBy tree
            {
                if (tokArr->Item(i)->m_TokenKind == tkFunction)
                    iind = m_pImgList->GetImageIdx("function_calledby");
                else
                    iind = m_pImgList->GetImageIdx("subroutine_calledby");
            }
        }
        else
        {
            iind = m_pImgList->GetTokenKindImageIdx(tokArr->Item(i));
        }
        wxTreeItemId addedId = InsertTreeItem(parent, tokArr->Item(i)->m_DisplayName, iind, tidata);

        ShowCallTreeChildren(&tokArr->Item(i)->m_Children, addedId, callLevel);
        if (callLevel == 1)
            m_pTree->Expand(addedId);
    }
    callLevel -= 1;
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

    if (ctd->m_DefFilename == wxEmptyString)
        return;

    GoToLine(ctd->m_DefFilename, ctd->m_DefLineStart);
}

void CallTreeView::GoToLine(wxString& filename, unsigned int line)
{
    TokenFlat token;
    token.m_Filename = filename;
    token.m_LineStart = line;
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

void CallTreeView::ShowMenu(wxTreeItemId id, const wxPoint& pt, bool isFirstLevelItem)
{
    if ( !id.IsOk() )
        return;

#if wxUSE_MENUS
    wxString caption;
    wxMenu *menu=new wxMenu(wxEmptyString);

    if (isFirstLevelItem)
    {
        menu->Append(idMenuRefreshTree, _("&Refresh tree"));

        menu->AppendCheckItem(idMenuDoNotSort, _("Do not sort"));
        menu->Check(idMenuDoNotSort, !m_SortAlphabetically);
        menu->AppendCheckItem(idMenuSortAlphabetically, _("Sort alphabetically"));
        menu->Check(idMenuSortAlphabetically, m_SortAlphabetically);
    }
    else
    {
        menu->Append(idMenuGoToProcedure, _("Go to &procedure"));
        menu->Append(idMenuGoToCall, _("Go to &caller"));
    }

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

    m_pTree->SelectItem(event.GetItem());
    ShowMenu(event.GetItem(), event.GetPoint(), isFirstLevelItem);
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

    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");
    cfg->Write("/calltree_sort_alphabetically", m_SortAlphabetically);

    UpdateView();
}

void CallTreeView::OnGoToProcedure(wxCommandEvent& event)
{
    wxTreeItemId id = m_pTree->GetSelection();
    if (!id.IsOk())
        return;

    CTVData* ctd = (CTVData*)m_pTree->GetItemData(id);
    if (!ctd)
        return;

    if (ctd->m_DefFilename == wxEmptyString)
        return;

    GoToLine(ctd->m_DefFilename, ctd->m_DefLineStart);
}

void CallTreeView::OnGoToCall(wxCommandEvent& event)
{
    wxTreeItemId id = m_pTree->GetSelection();
    if (!id.IsOk())
        return;

    CTVData* ctd = (CTVData*)m_pTree->GetItemData(id);
    if (!ctd)
        return;

    if (ctd->m_CallFilename == wxEmptyString)
        return;

    GoToLine(ctd->m_CallFilename, ctd->m_CallLine);
}

void CallTreeView::RereadOptions()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");
    m_SortAlphabetically = cfg->ReadBool("/calltree_sort_alphabetically", true);
}

void CallTreeView::UpdateView()
{

    wxTreeItemId id = m_pTree->GetSelection();
    if (!id.IsOk())
        return;

    CTVData* ctd = (CTVData*)m_pTree->GetItemData(id);
    if (!ctd)
        return;

    if (ctd->m_DefFilename == wxEmptyString)
        return;

    TokenFlat token;
    token.m_Filename = ctd->m_DefFilename;
    token.m_LineStart = ctd->m_DefLineStart;
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (!ed)
        return;
    m_pFortranProject->GotoToken(&token, ed);

    ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (!ed)
        return;
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
