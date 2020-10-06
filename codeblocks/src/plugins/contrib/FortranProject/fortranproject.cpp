/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */

#include <sdk.h> // Code::Blocks SDK
#ifndef CB_PRECOMP
    #include <wx/filename.h>
    #include <wx/tokenzr.h>
    #include <wx/gdicmn.h>
    #include <wx/xrc/xmlres.h>
    #include <wx/event.h>
    #include <wx/menu.h>
    #include <wx/toolbar.h>
    #include <wx/choicdlg.h>

    #include <configurationpanel.h>
    #include <manager.h>
    #include <ccmanager.h>
    #include <editorcolourset.h>
    #include <editormanager.h>
    #include <logmanager.h>
    #include <projectmanager.h>
    #include <cbstyledtextctrl.h>
    #include <projectloader_hooks.h>
    #include <editor_hooks.h>
    #include <cbeditor.h>
#endif
#include <vector>

#include "fortranproject.h"
#include "fpoptionsdlg.h"
#include "fpoptionsprjdlg.h"
#include "jumptracker.h"
#include "changecase.h"
#include "tab2space.h"
#include "docblock.h"
#include "formatindent.h"
#include "bindto.h"
#include "ccsmartfilter.h"

// this auto-registers the plugin
namespace
{
    PluginRegistrant<FortranProject> reg(_T("FortranProject"));
}


int idGotoDeclaration      = wxNewId();
int idCodeCompleteTimer    = wxNewId();
int idMenuJump             = wxNewId();
int idMenuGotoDeclaration  = wxNewId();
int idMenuJumpBack         = wxNewId();
int idMenuJumpHome         = wxNewId();
int idMenuJumpForward      = wxNewId();
int idViewSymbolsBrowser   = wxNewId();
int idMenuGenerateMakefile = wxNewId();
int idMenuChangeCase       = wxNewId();
int idMenuTab2Space        = wxNewId();
int idMenuFormatIndent     = wxNewId();
int idMenuBindTo           = wxNewId();
int idReparseEditorTimer   = wxNewId();
int idShowCallTree         = wxNewId();
int idShowCalledByTree     = wxNewId();
int idViewCallTree         = wxNewId();

#ifndef __WXMSW__
int idMenuEditPaste = XRCID("idEditPaste");
#endif

BEGIN_EVENT_TABLE(FortranProject, cbCodeCompletionPlugin)
    EVT_UPDATE_UI(idViewSymbolsBrowser, FortranProject::OnUpdateUI)
    EVT_UPDATE_UI(idViewCallTree, FortranProject::OnUpdateUICallTree)
    EVT_MENU(idMenuGotoDeclaration, FortranProject::OnGotoDeclaration)
    EVT_MENU(idMenuJumpBack, FortranProject::OnJumpBack)
    EVT_MENU(idMenuJumpHome, FortranProject::OnJumpHome)
    EVT_MENU(idMenuJumpForward, FortranProject::OnJumpForward)
    EVT_MENU(idGotoDeclaration, FortranProject::OnGotoDeclaration)
    EVT_MENU(idViewSymbolsBrowser, FortranProject::OnViewWorkspaceBrowser)
    EVT_MENU(idViewCallTree, FortranProject::OnShowCallTreeView)
    EVT_MENU(idMenuGenerateMakefile, FortranProject::OnGenerateMakefile)
    EVT_MENU(idMenuChangeCase, FortranProject::OnChangeCase)
    EVT_MENU(idMenuTab2Space, FortranProject::OnTab2Space)
    EVT_MENU(idMenuFormatIndent, FortranProject::OnFormatIndent)
    EVT_MENU(idMenuBindTo, FortranProject::OnBindTo)
    EVT_MENU(idShowCallTree, FortranProject::OnShowCallTree)
    EVT_MENU(idShowCalledByTree, FortranProject::OnShowCallTree)
#ifndef __WXMSW__
    EVT_MENU(idMenuEditPaste, FortranProject::OnMenuEditPaste)
#endif
    EVT_TIMER(idReparseEditorTimer, FortranProject::OnReparseEditorTimer)
    EVT_TOOL(XRCID("idFortProjBack"), FortranProject::OnJumpBack)
    EVT_TOOL(XRCID("idFortProjHome"), FortranProject::OnJumpHome)
    EVT_TOOL(XRCID("idFortProjForward"), FortranProject::OnJumpForward)
END_EVENT_TABLE()

FortranProject::FortranProject() :
    m_pNativeParser(0),
    m_EditorHookId(0),
    m_TimerCodeCompletion(this, idCodeCompleteTimer),
    m_pCodeCompletionLastEditor(0),
    m_pToolbar(0L),
    m_ShowedCallTip(false),
    m_WasCallTipActive(false),
    m_IsAutoPopup(false),
    m_ActiveCalltipsNest(0),
    m_ActiveCalltipsPosition(-1),
    m_CurrentLine(0),
    m_pFortranLog(0L),
    m_TimerReparseEditor(this, idReparseEditorTimer)
{
    if(!Manager::LoadResource(_T("FortranProject.zip")))
    {
        NotifyMissingFile(_T("FortranProject.zip"));
    }
}

FortranProject::~FortranProject()
{
}


void FortranProject::OnAttach()
{
    m_ViewMenu = 0;
    m_FortranToolsMenu = 0;

    m_pNativeParser = new NativeParserF(this);
    m_pNativeParser->CreateWorkspaceBrowser();
    m_LastPosForCodeCompletion = -1;

    m_pKeywordsParser = new KeywordsParserF();

    m_pCallTree = new CallTree(this);

    RereadOptions();
    LoadFortranKeywordImages();

    // hook to project loading procedure
    ProjectLoaderHooks::HookFunctorBase* fp_hook =
        new ProjectLoaderHooks::HookFunctor<FortranProject>(this, &FortranProject::OnProjectLoadingHook);
    m_ProjectLoadingHookID = ProjectLoaderHooks::RegisterHook(fp_hook);

    // hook to editors
    EditorHooks::HookFunctorBase* myhook = new EditorHooks::HookFunctor<FortranProject>(this, &FortranProject::EditorEventHook);
    m_EditorHookId = EditorHooks::RegisterHook(myhook);

    // register event sinks
    Manager* pm = Manager::Get();

    pm->RegisterEventSink(cbEVT_EDITOR_SAVE, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnEditorSave));
    pm->RegisterEventSink(cbEVT_EDITOR_ACTIVATED, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnEditorActivated));
    pm->RegisterEventSink(cbEVT_EDITOR_DEACTIVATED, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnEditorDeactivated));
    pm->RegisterEventSink(cbEVT_EDITOR_CLOSE, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnEditorClose));

    pm->RegisterEventSink(cbEVT_APP_STARTUP_DONE, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnAppDoneStartup));
    pm->RegisterEventSink(cbEVT_WORKSPACE_CHANGED, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnWorkspaceChanged));
    pm->RegisterEventSink(cbEVT_PROJECT_ACTIVATE, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnProjectActivated));
    pm->RegisterEventSink(cbEVT_PROJECT_CLOSE, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnProjectClosed));
    pm->RegisterEventSink(cbEVT_PROJECT_SAVE, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnProjectSaved));
    pm->RegisterEventSink(cbEVT_PROJECT_FILE_ADDED, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnProjectFileAdded));
    pm->RegisterEventSink(cbEVT_PROJECT_FILE_REMOVED, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnProjectFileRemoved));
    pm->RegisterEventSink(cbEVT_COMPILER_STARTED, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnCompilerStarted));
    pm->RegisterEventSink(cbEVT_CLEAN_PROJECT_STARTED, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnCleanProjectStarted));
    pm->RegisterEventSink(cbEVT_CLEAN_WORKSPACE_STARTED, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnCleanWorkspaceStarted));

    pm->RegisterEventSink(cbEVT_DEBUGGER_STARTED, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnDebuggerStarted));
    pm->RegisterEventSink(cbEVT_DEBUGGER_FINISHED, new cbEventFunctor<FortranProject, CodeBlocksEvent>(this, &FortranProject::OnDebuggerFinished));

    pm->GetCCManager()->RegisterAutoLaunchChars(wxT("%"), this);

    m_IsDebugging = false;
    m_InitDone = true;
}

void FortranProject::OnRelease(bool appShutDown)
{
    // unregister hook
    // 'true' will delete the functor too
    ProjectLoaderHooks::UnregisterHook(m_ProjectLoadingHookID, true);
    EditorHooks::UnregisterHook(m_EditorHookId, true);

    // remove registered event sinks
    Manager::Get()->RemoveAllEventSinksFor(this);

    if (m_pNativeParser)
    {
        delete m_pNativeParser;
    }
    if (m_pKeywordsParser)
    {
        delete m_pKeywordsParser;
    }
    if (m_pCallTree)
    {
        delete m_pCallTree;
    }

    RemoveLogWindow(appShutDown);

    if (m_ViewMenu)
    {
        m_ViewMenu->Delete(idViewSymbolsBrowser);
        m_ViewMenu->Delete(idViewCallTree);
    }

    if (m_FortranToolsMenu)
    {
        m_FortranToolsMenu->Delete(idMenuJump);
        m_FortranToolsMenu->Delete(idMenuGenerateMakefile);
        m_FortranToolsMenu->Delete(idMenuChangeCase);
        m_FortranToolsMenu->Delete(idMenuTab2Space);
        m_FortranToolsMenu->Delete(idMenuFormatIndent);
        m_FortranToolsMenu->Delete(idMenuBindTo);
    }
} // end of OnRelease


void FortranProject::OnUpdateUI(wxUpdateUIEvent& event)
{
    if (m_ViewMenu)
    {
        bool isVis = IsWindowReallyShown((wxWindow*)m_pNativeParser->GetWorkspaceBrowser());
        m_ViewMenu->Check(idViewSymbolsBrowser, isVis);
    }

    event.Skip();
}


void FortranProject::OnUpdateUICallTree(wxUpdateUIEvent& event)
{
    if (m_ViewMenu)
    {
        bool isVis = IsWindowReallyShown((wxWindow*)m_pCallTree->GetCallTreeView());
        m_ViewMenu->Check(idViewCallTree, isVis);
    }

    event.Skip();
}


void FortranProject::OnAppDoneStartup(CodeBlocksEvent& event)
{
    if (IsAttached())
    {
        m_InitDone = false;
        // parse any projects opened through DDE or the command-line
        m_pNativeParser->ForceReparseWorkspace();
        m_InitDone = true;
    }

    if (m_pNativeParser->GetWorkspaceBrowser())
    {
        m_pNativeParser->GetWorkspaceBrowser()->UpdateSash();
    }
    event.Skip();
}

void FortranProject::OnWorkspaceChanged(CodeBlocksEvent& event)
{
    // EVT_WORKSPACE_CHANGED is a powerful event, it's sent after any project
    // has finished loading or closing. It's the *LAST* event to be sent when
    // the workspace has been changed, and it's not sent if the application is
    // shutting down. So it's the ideal time to parse files and update your
    // widgets.
    if (IsAttached() && m_InitDone && !Manager::IsAppShuttingDown())
    {
        m_InitDone = false;
        // Parse the projects
        m_pNativeParser->ForceReparseWorkspace();
        m_InitDone = true;
    }
    event.Skip();
}

void FortranProject::OnProjectActivated(CodeBlocksEvent& event)
{
    // The Class browser shouldn't be updated if we're in the middle of loading/closing
    // a project/workspace, because the class browser would need to be updated again.
    // So we need to update it with the EVT_WORKSPACE_CHANGED event, which gets
    // triggered after everything's finished loading/closing.

    if (!ProjectManager::IsBusy() && IsAttached() && m_InitDone)
    {
        m_pNativeParser->OnProjectActivated(event.GetProject());
    }
    event.Skip();
}

void FortranProject::OnProjectClosed(CodeBlocksEvent& event)
{
    // After this, the Class Browser needs to be updated. It will happen
    // when we receive the next EVT_PROJECT_ACTIVATED event.
//    if (IsAttached() && m_InitDone)
//    {
//        m_pNativeParser->RemoveFromParser(event.GetProject());
//    }

    if (!ProjectManager::IsBusy() && IsAttached() && m_InitDone)
    {
        m_pNativeParser->DelProjectSearchDirs(event.GetProject());
    }

    event.Skip();
}

void FortranProject::OnProjectSaved(CodeBlocksEvent& event)
{
    // Do we need it for Fortran?
    event.Skip();
}

void FortranProject::OnProjectFileAdded(CodeBlocksEvent& event)
{
    if (IsAttached() && m_InitDone)
    {
        cbProject* cbp = event.GetProject();
        if (cbp)
        {
            wxString pfn = cbp->GetFilename();
            m_pNativeParser->AddFileToParser(pfn, event.GetString());
            m_pNativeParser->UpdateWorkspaceBrowser();
        }
    }
    event.Skip();
}

void FortranProject::OnProjectFileRemoved(CodeBlocksEvent& event)
{
    if (IsAttached() && m_InitDone)
    {
        m_pNativeParser->RemoveFileFromParser(event.GetString());
        m_pNativeParser->UpdateWorkspaceBrowser();
    }
    event.Skip();
}

void FortranProject::OnEditorSave(CodeBlocksEvent& event)
{
    if (!ProjectManager::IsBusy() && IsAttached() && m_InitDone)
    {
        EditorBase* eb = event.GetEditor();
        cbEditor* editor = (eb && eb->IsBuiltinEditor()) ? static_cast<cbEditor*>(eb) : 0;
        if (editor)
        {
            wxString projFN;
            ProjectFile* pf = editor->GetProjectFile();
            if (pf)
            {
                cbProject* cbp = pf->GetParentProject();
                projFN = cbp->GetFilename();
            }
            m_pNativeParser->ReparseFile(projFN, editor->GetFilename());
            m_pNativeParser->UpdateWorkspaceBrowser();
        }
    }
    event.Skip();
}

void FortranProject::OnEditorActivated(CodeBlocksEvent& event)
{
    if (!ProjectManager::IsBusy() && IsAttached() && m_InitDone)
    {
        EditorBase* eb = event.GetEditor();
        m_pNativeParser->OnEditorActivated(eb);

        if (m_TimerReparseEditor.IsRunning())
            m_TimerReparseEditor.Stop();
        cbEditor* editor = (eb && eb->IsBuiltinEditor()) ? static_cast<cbEditor*>(eb) : 0;
        if (editor && editor->GetModified())
            m_TimerReparseEditor.Start(1500, wxTIMER_ONE_SHOT);

        FortranSourceForm fsForm;
        if (editor && m_pNativeParser->IsFileFortran(editor->GetShortName(), fsForm))
        {
            cbStyledTextCtrl* control = editor->GetControl();
            m_ConstrHighlighter.ClearHighlighting(control, true);
            m_ConstrHighlighter.DoWork(editor, fsForm);
        }
    }

    event.Skip();
}

void FortranProject::OnEditorDeactivated(CodeBlocksEvent& event)
{
    if (!ProjectManager::IsBusy() && IsAttached() && m_InitDone)
    {
        EditorBase* eb = event.GetEditor();
        cbEditor* editor = (eb && eb->IsBuiltinEditor()) ? static_cast<cbEditor*>(eb) : 0;
        if (editor)
        {
            cbStyledTextCtrl* control = editor->GetControl();
            m_ConstrHighlighter.ClearHighlighting(control);
        }
    }
    event.Skip();
}

void FortranProject::OnEditorClose(CodeBlocksEvent& event)
{
    if (!ProjectManager::IsBusy() && IsAttached() && m_InitDone)
    {
        EditorBase* eb = event.GetEditor();
        m_pNativeParser->OnEditorClose(eb);
    }

    event.Skip();
}

void FortranProject::OnCompilerStarted(CodeBlocksEvent& event)
{
    event.Skip();
    m_pNativeParser->UpdateWorkspaceFilesDependency();
}

void FortranProject::OnCleanProjectStarted(CodeBlocksEvent& event)
{
    event.Skip();
    //Remove all *.mod files from obj folder
    wxString targetName = event.GetBuildTargetName();
    cbProject* pr = event.GetProject();
    if (!pr)
        return;
    if (pr->IsMakefileCustom())
        return;
    ProjectBuildTarget* bTarget = pr->GetBuildTarget(targetName);
    if (bTarget)
    {
        ProjectDependencies::RemoveModFiles(pr, bTarget, m_pNativeParser);
    }
}

void FortranProject::OnCleanWorkspaceStarted(CodeBlocksEvent& event)
{
    event.Skip();
    //Remove all *.mod files from obj folder
    ProjectDependencies::RemoveModFilesWS(m_pNativeParser);
}

void FortranProject::BuildMenu(wxMenuBar* menuBar)
{
    if (!IsAttached())
        return;

    // add the fsymbolsbrowser window in the "View" menu
    int idx = menuBar->FindMenu(_("&View"));
    if (idx != wxNOT_FOUND)
    {
        m_ViewMenu = menuBar->GetMenu(idx);
        wxMenuItemList& items = m_ViewMenu->GetMenuItems();
        bool inserted = false;

        // find the first separator and insert before it
        for (size_t i = 0; i < items.GetCount(); ++i)
        {
            if (items[i]->IsSeparator())
            {
                m_ViewMenu->InsertCheckItem(i, idViewSymbolsBrowser, _("Fortran symbols browser"), _("Toggle displaying the fortran symbols browser"));
                m_ViewMenu->InsertCheckItem(i, idViewCallTree, _("Fortran Call/Called-By tree"), _("Toggle displaying the fortran Call/Called-By tree window"));
                inserted = true;
                break;
            }
        }

        // not found, just append
        if (!inserted)
        {
            m_ViewMenu->AppendCheckItem(idViewSymbolsBrowser, _("Fortran symbols browser"), _("Toggle displaying the fortran symbols browser"));
            m_ViewMenu->AppendCheckItem(idViewCallTree, _("Fortran call tree"), _("Toggle displaying the fortran Call/Called-By tree window"));
        }
    }
    else
        Manager::Get()->GetLogManager()->DebugLog(_T("FortranProject: Could not find View menu!"));


    int pos = menuBar->FindMenu(_("Fortra&n"));
    if (pos == wxNOT_FOUND)
    {
        pos = menuBar->FindMenu(_("&Tools"));
        if (pos != wxNOT_FOUND)
        {
            m_FortranToolsMenu = new wxMenu();
            menuBar->Insert(pos, m_FortranToolsMenu, _("Fortra&n"));
        }
        else
            Manager::Get()->GetLogManager()->DebugLog(_T("FortranProject: Could not find Tools menu!"));
    }
    else
    {
        m_FortranToolsMenu = menuBar->GetMenu(pos);
    }
    if (m_FortranToolsMenu)
    {
        wxMenu* submenuJump = new wxMenu();
        submenuJump->Append(idMenuGotoDeclaration, _("Jump to declaration"));
        const int imageSize = Manager::Get()->GetImageSize(Manager::UIComponent::Menus);
        const int uiScaleFactor = Manager::Get()->GetUIScaleFactor(Manager::UIComponent::Menus);
        wxString prefix = ConfigManager::GetDataFolder() +
                          wxString::Format(_T("/images/fortranproject/%dx%d/"), imageSize, imageSize);

        wxBitmap bmp_back = cbLoadBitmapScaled(prefix + _T("fprojectjumpback.png"), wxBITMAP_TYPE_PNG, uiScaleFactor);
        wxBitmap bmp_home = cbLoadBitmapScaled(prefix + _T("fprojectjumphome.png"), wxBITMAP_TYPE_PNG, uiScaleFactor);
        wxBitmap bmp_forward = cbLoadBitmapScaled(prefix + _T("fprojectjumpforward.png"), wxBITMAP_TYPE_PNG, uiScaleFactor);
        wxMenuItem* itemJumpBack = new wxMenuItem(submenuJump, idMenuJumpBack, _("Jump back"));
        itemJumpBack->SetBitmap(bmp_back);
        wxMenuItem* itemJumpHome = new wxMenuItem(submenuJump, idMenuJumpHome, _("Jump last"));
        itemJumpHome->SetBitmap(bmp_home);
        wxMenuItem* itemJumpForward = new wxMenuItem(submenuJump, idMenuJumpForward, _("Jump forward"));
        itemJumpForward->SetBitmap(bmp_forward);
        submenuJump->Append(itemJumpBack);
        submenuJump->Append(itemJumpHome);
        submenuJump->Append(itemJumpForward);
        submenuJump->Enable(idMenuJumpBack, false);
        submenuJump->Enable(idMenuJumpHome, false);
        submenuJump->Enable(idMenuJumpForward, false);

        m_FortranToolsMenu->Insert(0, idMenuBindTo, _("Bind To..."));
        m_FortranToolsMenu->Insert(0, idMenuFormatIndent, _("Format indent..."));
        m_FortranToolsMenu->Insert(0, idMenuTab2Space, _("Tab2space..."));
        m_FortranToolsMenu->Insert(0, idMenuChangeCase, _("Change case..."));
        m_FortranToolsMenu->Insert(0, idMenuGenerateMakefile, _("Generate Makefile..."));
        m_FortranToolsMenu->Insert(0, idMenuJump, _("Jump"), submenuJump);
    }
}

static int CalcStcFontSize(cbStyledTextCtrl *stc)
{
    wxFont defaultFont = stc->StyleGetFont(wxSCI_STYLE_DEFAULT);
    defaultFont.SetPointSize(defaultFont.GetPointSize() + stc->GetZoom());
    int fontSize;
    stc->GetTextExtent(wxT("A"), nullptr, &fontSize, nullptr, nullptr, &defaultFont);
    return fontSize;
}

// invariant : on return true : NameUnderCursor is NOT empty
static bool EditorHasNameUnderCursor(wxString& NameUnderCursor, bool& isOperator)
{
    if(cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor())
    {
        isOperator = false;
        cbStyledTextCtrl* control = ed->GetControl();
        const int pos = control->GetCurrentPos();

        int ws = control->WordStartPosition(pos, true);
        int we = control->WordEndPosition(pos, true);
        if (ws < we && ws > 0 && control->GetCharAt(ws-1) == '.'
            && we < control->GetLength() && control->GetCharAt(we) == '.')
        {
            // maybe we have user defined operator .name. Take it as one word.
            ws--;
            we++;
            isOperator = true;
        }
        const wxString txt = control->GetTextRange(ws, we);
        if (!txt.IsEmpty())
        {
            NameUnderCursor = txt;
            return true;
        }
        // Check if we at operator
        wxString operatorsTxt = _T("=*/+-<>");
        int opStart = pos;
        for (int i=1; i<3 && pos-i>0; i++)
        {
            wxChar txt1 = control->GetCharAt(pos-i);
            if (operatorsTxt.Contains(txt1))
                opStart = pos-i;
            else
                break;
        }
        int opEnd = pos;
        for (int i=0; i<3 && pos+i<control->GetLength(); i++)
        {
            wxChar txt1 = control->GetCharAt(pos+i);
            if (operatorsTxt.Contains(txt1))
                opEnd = pos+i+1;
            else
                break;
        }
        wxString opStr = control->GetTextRange(opStart, opEnd);
        if (!opStr.IsEmpty())
        {
            NameUnderCursor = opStr;
            isOperator = true;
            return true;
        }

    }
    return false;
} // end of EditorHasNameUnderCursor

void FortranProject::BuildModuleMenu(const ModuleType type, wxMenu* menu, const FileTreeData* data)
{
    if (!menu || !IsAttached() || !m_InitDone)
        return;
    if (type == mtEditorManager)
    {
        cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
        if (!ed || !m_pNativeParser->IsFileFortran(ed->GetFilename()))
            return;

        wxString NameUnderCursor;
        bool isOperator;
        if(EditorHasNameUnderCursor(NameUnderCursor, isOperator))
        {
            wxString msg;
            msg.Printf(_("Jump to '%s'"), NameUnderCursor.c_str());
            menu->Insert(0, idGotoDeclaration, msg);

            menu->Insert(1, wxID_SEPARATOR, wxEmptyString);
            Manager::Get()->GetPluginManager()->RegisterFindMenuItems(true, 2);

            if (!isOperator)
            {
                wxMenu* showsubmenu = new wxMenu();
                showsubmenu->Append(idShowCallTree, _T("Call tree"));
                showsubmenu->Append(idShowCalledByTree, _T("Called-By tree"));
                menu->Insert(1, wxID_ANY, _("Show"), showsubmenu);
                Manager::Get()->GetPluginManager()->RegisterFindMenuItems(true, 1);
            }
        }
    }

}

void FortranProject::OnGotoDeclaration(wxCommandEvent& event)
{
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (!ed)
        return;

    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return;

    wxString NameUnderCursor;
    bool isOperator;
    if(!EditorHasNameUnderCursor(NameUnderCursor, isOperator))
        return;

    // get the matching set
    ParserF* pParser = m_pNativeParser->GetParser();
    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* result = tokensTmp.GetTokens();

    wxString includeFilename = GetIncludeFilename(ed->GetControl());
    if (!includeFilename.IsEmpty())
    {
        // was asked to jump to include file.
        pParser->FindFile(includeFilename, *result);
    }
    else if (isOperator)
    {
        pParser->FindMatchOperatorTokensForJump(NameUnderCursor, *result);
    }
    else
    {
        pParser->FindMatchTokensForJump(ed, m_LogOnlyUseAssoc, false, *result);
        // don't jump to intrinsic module
        size_t ri = 0;
        while (ri<result->GetCount())
        {
            if (result->Item(ri)->m_Filename.EndsWith(UnixFilename(_T("/images/fortranproject/fortran_intrinsic_modules.f90"))))
            {
                result->Item(ri)->Clear();
                delete result->Item(ri);
                result->RemoveAt(ri);
            }
            else
                ri++;
        }
    }

    if (result->GetCount() > 1)
    {
        // Remove result with the current line
        wxString curfname = UnixFilename(ed->GetFilename());
        unsigned int curlineIdx = control->LineFromPosition(control->GetCurrentPos());
        for (size_t ri=0; ri<result->GetCount(); ri++)
        {
            if (result->Item(ri)->m_Filename.IsSameAs(curfname) &&
                result->Item(ri)->m_LineStart == curlineIdx + 1)
            {
                result->Item(ri)->Clear();
                delete result->Item(ri);
                result->RemoveAt(ri);
                break;
            }
        }
    }

    size_t count = std::min(result->GetCount(),m_MaxMatch);

    TokenFlat* pToken = 0;
    // one match
    if (count == 1)
    {
		pToken = result->Item(0);
    }
    // if more than one match, display a selection dialog
    else if (count > 1)
    {
        wxArrayString selections;
        std::vector<int> idxItems;
        for (size_t i=0; i<count; ++i)
        {
            wxFileName fn = wxFileName(result->Item(i)->m_Filename);
            wxString inf;
            if (result->Item(i)->m_TokenKind == tkUse && !result->Item(i)->m_Rename.IsEmpty())
            {
                inf = _T("use :: ") + result->Item(i)->m_DisplayName + _T(", ") + result->Item(i)->m_Rename;
            }
            else
            {
                inf = result->Item(i)->m_DisplayName + _T(" :: ") + result->Item(i)->GetTokenKindString();
            }
            inf += _T(" : ") + fn.GetFullName() + _T(" : ");
            inf += wxString::Format(_T("%d"), int(result->Item(i)->m_LineStart));

            if (selections.Index(inf) == wxNOT_FOUND)
            {
                selections.Add(inf);
                idxItems.push_back(i);
            }
        }
        if (selections.Count() > 1)
        {
            int sel = wxGetSingleChoiceIndex(_("Please make a selection:"), _("Multiple matches"), selections);
            if (sel == -1)
                return;
            pToken = result->Item(idxItems[sel]);
        }
        else
        {
            pToken = result->Item(0);
        }
    }

    if (pToken)
    {
        if (!GotoToken(pToken, ed))
            cbMessageBox(wxString::Format(_("Declaration not found: %s"), NameUnderCursor.c_str()), _("Warning"), wxICON_WARNING);
    }
    else
    {
        cbMessageBox(wxString::Format(_("Not found: %s"), NameUnderCursor.c_str()), _("Warning"), wxICON_WARNING);
    }

} // end of OnGotoDeclaration


bool FortranProject::GotoToken(TokenFlat* pToken, cbEditor* cured)
{
    LineAddress jumpStart;
    LineAddress jumpFinish;
    if(cured)
    {
        cbStyledTextCtrl* control = cured->GetControl();
        int curLine = control->LineFromPosition(control->GetCurrentPos());
        jumpStart.Init(cured->GetFilename(), curLine, false);
    }

    if (cbEditor* newed = Manager::Get()->GetEditorManager()->Open(pToken->m_Filename))
    {
        newed->GotoLine(pToken->m_LineStart - 1);

        // Track jump history
        cbStyledTextCtrl* control = newed->GetControl();
        int curLine = control->LineFromPosition(control->GetCurrentPos());
        jumpFinish.Init(newed->GetFilename(), curLine, true);
        m_pNativeParser->GetJumpTracker()->TakeJump(jumpStart, jumpFinish);
        CheckEnableToolbar();
    }
    else
    {
        return false;
    }
    return true;
} // end of OnGotoToken


void FortranProject::CodeCompletePreprocessor(int tknStart, int tknEnd, cbEditor* ed, std::vector<CCToken>& tokens)
{
    if (!IsAttached() || !m_InitDone)
        return;

    cbStyledTextCtrl* stc = ed->GetControl();
    const wxString text = stc->GetTextRange(tknStart, tknEnd);

    TokenF tp;
    tp.m_TokenKind = tkPreprocessor;
    int iidx = m_pNativeParser->GetTokenKindImageIdx(&tp);

    wxStringVec macros;
    macros.push_back(wxT("define"));
    macros.push_back(wxT("elif"));
    macros.push_back(wxT("elifdef"));
    macros.push_back(wxT("elifndef"));
    macros.push_back(wxT("else"));
    macros.push_back(wxT("endif"));
    macros.push_back(wxT("error"));
    macros.push_back(wxT("if"));
    macros.push_back(wxT("ifdef"));
    macros.push_back(wxT("ifndef"));
    macros.push_back(wxT("include"));
    macros.push_back(wxT("line"));
    macros.push_back(wxT("pragma"));
    macros.push_back(wxT("undef"));
    for (size_t i = 0; i < macros.size(); ++i)
    {
        if (text.IsEmpty() || macros[i][0] == text[0]) // ignore tokens that start with a different letter
            tokens.push_back(CCToken(wxNOT_FOUND, macros[i], iidx));
    }
    stc->ClearRegisteredImages();
    int fontSize = CalcStcFontSize(stc);
    FPImageList fpImList(fontSize);
    wxImageList* ilist = fpImList.GetImageList();
    if (!ilist)
        return;

    stc->RegisterImage(iidx, ilist->GetBitmap(iidx));
}

void FortranProject::DoCodeComplete(int caretPos, cbEditor* ed, std::vector<CCToken>& tokens)
{
    if (!ed)
        return;

    cbStyledTextCtrl* control = ed->GetControl();
    const int pos = control->GetCurrentPos();
    const int lineIndentPos = control->GetLineIndentPosition(control->GetCurrentLine());
    const wxChar lineFirstChar = control->GetCharAt(lineIndentPos);

    int lineCur = control->LineFromPosition(pos);
    int lineStartPos = control->PositionFromLine(lineCur);
    wxString curLine = control->GetTextRange(lineStartPos,pos).Trim(false);

    if (lineFirstChar == _T('!'))
    {
        wxString curLineLw = curLine.Lower();
        if (!curLineLw.StartsWith(_T("!$ ")) && !curLineLw.StartsWith(_T("!$\t")) && !curLineLw.StartsWith(_T("!$omp")) && !curLineLw.StartsWith(_T("!$acc")))
            return;
    }
    else
    {
        if (curLine.Find('!') != wxNOT_FOUND) // we are in comments
            return;
    }

    int style = control->GetStyleAt(control->GetCurrentPos());
    if (style != wxSCI_F_DEFAULT && style != wxSCI_F_WORD && style != wxSCI_F_WORD2 && style != wxSCI_F_WORD3
        && style != wxSCI_F_OPERATOR && style != wxSCI_F_IDENTIFIER && style != wxSCI_F_OPERATOR2
        && style != wxSCI_F_PREPROCESSOR )
        return;

    CodeComplete(caretPos, ed, tokens);
}


std::vector<FortranProject::CCToken> FortranProject::GetAutocompList(bool isAuto, cbEditor* ed, int& tknStart, int& tknEnd)
{
    std::vector<CCToken> tokens;

    if (!IsAttached() || !m_InitDone)
        return tokens;

    if (   !ed
        || !m_pNativeParser->IsFileFortran(ed->GetShortName())
        || !Manager::Get()->GetConfigManager(_T("fortran_project"))->ReadBool(_T("/use_code_completion"), true))
        return tokens;

    cbStyledTextCtrl* stc = ed->GetControl();
    const int style = stc->GetStyleAt(tknEnd);
    const wxChar curChar = stc->GetCharAt(tknEnd - 1);

    if (isAuto && curChar != wxT('%'))
        return tokens;

    const int lineIndentPos = stc->GetLineIndentPosition(stc->GetCurrentLine());
    const wxChar lineFirstChar = stc->GetCharAt(lineIndentPos);

    if (lineFirstChar == wxT('#'))
    {
        const int startPos = stc->WordStartPosition(lineIndentPos + 1, true);
        const int endPos = stc->WordEndPosition(lineIndentPos + 1, true);
        const wxString str = stc->GetTextRange(startPos, endPos);

        if (endPos >= tknEnd && tknEnd > lineIndentPos)
            CodeCompletePreprocessor(tknStart, tknEnd, ed, tokens);
        return tokens;
    }

    if (   stc->IsString(style)
        || stc->IsCharacter(style) )
    {
        return tokens;
    }

    DoCodeComplete(tknEnd, ed, tokens);
    return tokens;
}

void FortranProject::DoAutocomplete(const CCToken& token, cbEditor* ed)
{
    cbStyledTextCtrl* control = ed->GetControl();

    wxString itemText = token.displayName.BeforeFirst(':');
    control->AutoCompCancel();
    int pos = control->GetCurrentPos();
    int start = control->WordStartPosition(pos, true);
    int endPos = control->WordEndPosition(pos, true);
    const wxString& textUnder = control->GetTextRange(start, endPos);
    bool replaceWord = false;
    if (!textUnder.IsEmpty() && (start != pos || (pos != 0 && control->GetCharAt(pos-1) == _T('%'))))
    {
        TokensArrayFlat* ts = m_TokensCCList.GetTokens();
        for (size_t i=0; i < ts->size(); ++i)
        {
            if (ts->Item(i)->m_DisplayName.IsSameAs(textUnder, false))
            {
                replaceWord = true;
                break;
            }
        }
    }
    if (!replaceWord && pos != endPos)
    {
        const wxString& textUnderEnd = control->GetTextRange(pos, endPos);
        if (itemText.EndsWith(textUnderEnd))
        {
            replaceWord = true;
        }
    }
    if (textUnder.IsEmpty() || !textUnder.IsSameAs(itemText))
    {
        if (!replaceWord)
            endPos = pos;
        control->SetTargetStart(start);
        control->SetTargetEnd(endPos);
        control->ReplaceTarget(itemText);
    }
    control->GotoPos(start + itemText.size());

    if (m_WasCallTipActive)
    {
        m_WasCallTipActive = false;
        CodeBlocksEvent evt(cbEVT_SHOW_CALL_TIP);
        Manager::Get()->ProcessEvent(evt);
    }
}


void FortranProject::EditorEventHook(cbEditor* editor, wxScintillaEvent& event)
{
    if (!IsAttached() || !m_InitDone)
    {
        event.Skip();
        return;
    }

    FortranSourceForm fsForm;
    if (!m_pNativeParser->IsFileFortran(editor->GetShortName(), fsForm))
    {
        event.Skip();
        return;
    }

    cbStyledTextCtrl* control = editor->GetControl();
    wxEventType etyp = event.GetEventType();

    if (   (etyp == wxEVT_SCI_CHARADDED)
        && (!control->AutoCompActive()) ) // not already active autocompletion
    {
        wxChar ch = event.GetKey();
        // update calltip highlight while we type
        if (!control->CallTipActive())
            m_ActiveCalltipsNest = 0;

        // start calltip
        if (ch == _T('('))
        {
            if (control->CallTipActive())
                ++m_ActiveCalltipsNest;
        }
        // end calltip
        else if (ch == _T(')'))
        {
            control->CallTipCancel();
            if (m_ActiveCalltipsNest > 0)
            {
                --m_ActiveCalltipsNest;
                CodeBlocksEvent evt(cbEVT_SHOW_CALL_TIP);
                Manager::Get()->ProcessEvent(evt);
            }
        }
    }
    else if (etyp != wxEVT_SCI_CHARADDED && etyp != wxEVT_SCI_MODIFIED)
    {
        if (control->CallTipActive())
        {
            int curPos = control->GetCurrentPos();
            if (m_ActiveCalltipsPosition == -1)
            {
                m_ActiveCalltipsPosition = curPos;
            }
            else if (m_ActiveCalltipsPosition != curPos)
            {
                bool ctipsUpdate = true;
                if (m_ActiveCalltipsPosition+1 == curPos)
                {
                    wxChar prevChar = control->GetCharAt(m_ActiveCalltipsPosition);
                    wxChar curChar  = control->GetCharAt(curPos);
                    if (isalpha(prevChar) || prevChar == '_' || isdigit(prevChar))
                        ctipsUpdate = false;
                    else if (prevChar == ' ' && curChar == ' ')
                        ctipsUpdate = false;
                }
                else if (m_ActiveCalltipsPosition-1 == curPos)
                {
                    wxChar curChar  = control->GetCharAt(curPos);
                    if (isalpha(curChar) || curChar == '_' || isdigit(curChar) || curChar == ' ')
                        ctipsUpdate = false;
                }

                m_ActiveCalltipsPosition = curPos;
                if (ctipsUpdate)
                {
                    CodeBlocksEvent evt(cbEVT_SHOW_CALL_TIP);
                    Manager::Get()->ProcessEvent(evt);
                }
            }
        }
        else
            m_ActiveCalltipsPosition = -1;
    }

    if( control->GetCurrentLine() != m_CurrentLine )
    {
        m_CurrentLine = control->GetCurrentLine();
        m_pNativeParser->MarkCurrentSymbol(false);
    }

    if (etyp == wxEVT_SCI_MODIFIED && !m_TimerReparseEditor.IsRunning())
        m_TimerReparseEditor.Start(1500, wxTIMER_ONE_SHOT);

    if (m_AutoInsertEnabled && etyp == wxEVT_SCI_CHARADDED)
    {
        wxChar ch = event.GetKey();
        if ((ch == _T('\n')) || ( (control->GetEOLMode() == wxSCI_EOL_CR) && (ch == _T('\r')) ))
            m_AutoInsert.MakeAutoInsert(editor);
    }

    m_ConstrHighlighter.DoWork(editor, fsForm);

    // allow others to handle this event
    event.Skip();
}


void FortranProject::OnViewWorkspaceBrowser(wxCommandEvent& event)
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));
    if (!cfg->ReadBool(_T("/use_symbols_browser"), true))
    {
        cbMessageBox(_("The Fortran symbols browser is disabled in FortranProject options.\n"
                        "Please enable it there first..."), _("Information"), wxICON_INFORMATION);
        return;
    }
    CodeBlocksDockEvent evt(event.IsChecked() ? cbEVT_SHOW_DOCK_WINDOW : cbEVT_HIDE_DOCK_WINDOW);
    evt.pWindow = (wxWindow*)m_pNativeParser->GetWorkspaceBrowser();
    Manager::Get()->ProcessEvent(evt);
}


bool FortranProject::BuildToolBar(wxToolBar* toolBar)
{
    //The application is offering its toolbar for your plugin,
    //to add any toolbar items you want...
    //Append any items you need on the toolbar...
    //NotImplemented(_T("FortranProject::BuildToolBar()"));

    //Build toolbar
    if (!IsAttached() || !toolBar)
    {
        return false;
    }
    int imSize = Manager::Get()->GetImageSize(Manager::UIComponent::Toolbars);
    wxString tbSStr;
    if (imSize <= 16)
        tbSStr = _T("_16x16");
    else if (imSize <= 20)
        tbSStr = _T("_20x20");
    else if (imSize <= 24)
        tbSStr = _T("_24x24");
    else if (imSize <= 28)
        tbSStr = _T("_28x28");
    else if (imSize <= 32)
        tbSStr = _T("_32x32");
    else if (imSize <= 40)
        tbSStr = _T("_40x40");
    else if (imSize <= 48)
        tbSStr = _T("_48x48");
    else if (imSize <= 56)
        tbSStr = _T("_56x56");
    else
        tbSStr = _T("_64x64");


    Manager::Get()->AddonToolBar(toolBar,_T("fortran_project_toolbar") + tbSStr);
    toolBar->Realize();
    m_pToolbar = toolBar;
    m_pToolbar->EnableTool(XRCID("idFortProjBack"), false);
    m_pToolbar->EnableTool(XRCID("idFortProjHome"), false);
    m_pToolbar->EnableTool(XRCID("idFortProjForward"), false);
    m_pToolbar->SetInitialSize();

    return true;
}


static int SortCCList(const wxString& first, const wxString& second)
{
    const wxChar* a = first.c_str();
    const wxChar* b = second.c_str();
    while (*a && *b)
    {
        if (*a != *b)
        {
            if      ((*a == _T('?')) && (*b != _T('?')))
                return -1;
            else if ((*a != _T('?')) && (*b == _T('?')))
                return 1;
            else if ((*a == _T('?')) && (*b == _T('?')))
                return 0;

            if      ((*a == _T('_')) && (*b != _T('_')))
                return 1;
            else if ((*a != _T('_')) && (*b == _T('_')))
                return -1;

            wxChar lowerA = wxTolower(*a);
            wxChar lowerB = wxTolower(*b);

            if (lowerA != lowerB)
                return lowerA - lowerB;
        }
        a++;
        b++;
    }
    // Either *a or *b is null
    return *a - *b;
}


void FortranProject::CodeComplete(const int pos, cbEditor* ed, std::vector<CCToken>& tokens)
{
    if (!IsAttached() || !m_InitDone)
        return;

    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));

    ParserF* pParser = m_pNativeParser->GetParser();
    m_TokensCCList.Clear();
    TokensArrayFlat* result = m_TokensCCList.GetTokens();

    cbStyledTextCtrl* control = ed->GetControl();
    const int start = control->WordStartPosition(pos, true);
    wxString NameUnderCursor = control->GetTextRange(start,pos);
    wxString NameUnderCursorLw = NameUnderCursor.Lower();

    CompilerDirective pdir = cdNone;
    int lineCur = control->LineFromPosition(pos);
    int lineStartPos = control->PositionFromLine(lineCur);
    wxString curLine = control->GetTextRange(lineStartPos,pos).Trim(false).Lower();

    if (curLine.StartsWith(_T("!$")))
    {
        if ((NameUnderCursorLw.IsSameAs(_T("omp")) || NameUnderCursorLw.IsSameAs(_T("acc"))) && start >= 2)
        {
            // Check if cursor is not direct after !$omp or !$acc
            wxString word = control->GetTextRange(start-2,pos).Lower();
            if (word.IsSameAs(_T("!$omp")) || word.IsSameAs(_T("!$acc")))
                return;
        }
        if (curLine.StartsWith(_T("!$omp")))
            pdir = cdOpenMP;
        else if (curLine.StartsWith(_T("!$acc")))
            pdir = cdOpenACC;
        else
            pdir = cdOther;
    }

    bool isAfterPercent;
    int tokenKind;
    wxArrayString firstWords;

    if (!pParser->FindMatchTokensForCodeCompletion(m_UseSmartCC, m_LogOnlyUseAssoc, m_LogOnlyPublicNames,
                                                   NameUnderCursor, ed, *result, isAfterPercent, tokenKind, firstWords))
        return;

    if (result->size() <= m_MaxMatch)
    {
        int fontSize = CalcStcFontSize(control);

        FPImageList fpImList(fontSize);
        wxImageList* ilist = fpImList.GetImageList();
        if (!ilist)
            return;
        control->ClearRegisteredImages();
        int ilistImWidth = 0;
        int ilistImHeight = 0;
        ilist->GetSize(0, ilistImWidth, ilistImHeight);

        tokens.reserve(result->size());
        wxArrayString items; items.Alloc(result->size());
        std::set<int> already_registered;
        std::set< wxString, std::less<wxString> > unique_strings; // check against this before inserting a new string in the list
        for (size_t i=0; i<result->GetCount(); ++i)
        {
            TokenFlat* token = result->Item(i);
            if (token->m_Name.StartsWith(_T("%%")) || token->m_Name.IsEmpty())
                continue;

            wxString tmpstr = token->m_Name;
            if (m_LogShowTypeVariables && token->m_TokenKind == tkVariable)
                tmpstr << _T(": ") << token->m_PartFirst; // add type of variable

            // check for unique_strings
            if (unique_strings.find(tmpstr) != unique_strings.end())
                continue;

            unique_strings.insert(tmpstr);
            int iidx = m_pNativeParser->GetTokenKindImageIdx(token);
            if (already_registered.find(iidx) == already_registered.end())
            {
                if (iidx != -1)
                {
                    control->RegisterImage(iidx, ilist->GetBitmap(iidx));
                    already_registered.insert(iidx);
                }
            }
            wxString tmp;
            if (iidx != -1)
            {
                if (m_LogShowTypeVariables && token->m_TokenKind == tkVariable)
                    tmp << token->m_DisplayName << _T(": ") << token->m_PartFirst;
                else
                    tmp << token->m_DisplayName;
            }
            else
                tmp << token->m_DisplayName;

            tokens.push_back(CCToken(i, tmp, token->m_DisplayName, 5, iidx));
        }

        EditorColourSet* theme = ed->GetColourSet();
        if (theme && !isAfterPercent && (pdir == cdNone || pdir == cdOther) )
        {
            int iidx = ilist->GetImageCount();
            control->RegisterImage(iidx, GetFortranKeywordImage(ilistImHeight));
            // theme keywords
            HighlightLanguage lang = theme->GetLanguageForFilename(_T(".")+wxFileName(ed->GetFilename()).GetExt());

            int kwcase = cfg->ReadInt(_T("/keywords_case"), 0);
            for (int i = 0; i <= wxSCI_KEYWORDSET_MAX; ++i)
            {
                if (!m_LexerKeywordsToInclude[i])
                    continue;

                int oldSize = result->size();
                wxString keywords = theme->GetKeywords(lang, i);
                wxStringTokenizer tkz(keywords, _T(" \t\r\n"), wxTOKEN_STRTOK);
                while (tkz.HasMoreTokens())
                {
                    wxString kw = tkz.GetNextToken();

                    if ( (m_UseSmartCC && kw.Lower().StartsWith(NameUnderCursorLw) && m_pKeywordsParser->HasTokenSuitableKind(kw,tokenKind) &&
                          CCSmartFilter::FitsToContext(kw, firstWords))
                         || (!m_UseSmartCC && kw.Lower().StartsWith(NameUnderCursorLw)) )
                    {
                        // check for unique_strings
                        if (unique_strings.find(kw) != unique_strings.end())
                            continue;
                        unique_strings.insert(kw);

                        switch (kwcase)
                        {
                            case 0:
                            {
                                break;
                            }
                            case 1:
                            {
                                kw = kw.MakeUpper();
                                break;
                            }
                            case 2:
                            {
                                kw = kw.Mid(0,1).MakeUpper() + kw.Mid(1).MakeLower();
                                break;
                            }
                            default :
                            {
                                kw = kw.MakeLower();
                                break;
                            }
                        }
                        m_pKeywordsParser->FindTokens(kw, *result);
                        int newSize = result->size();
                        if (newSize > oldSize && (result->Item(newSize-1)->m_TokenKind & tokenKind))
                        {
                            tokens.push_back(CCToken(newSize-1, kw, iidx));
                            oldSize = newSize;
                        }
                        else
                            tokens.push_back(CCToken(wxNOT_FOUND, kw, iidx));
                    }
                }
            }
        }
        else if (pdir == cdOpenMP || pdir == cdOpenACC)
        {
            int iidx = ilist->GetImageCount();
            control->RegisterImage(iidx, GetFortranKeywordImage(ilistImHeight));

            int kwcase = cfg->ReadInt(_T("/keywords_case"), 0);
            const wxArrayString* kwOMP = m_pKeywordsParser->GetKeywords(pdir);
            for (size_t i=0; i<kwOMP->size(); i++)
            {
                wxString kw = kwOMP->Item(i);

                if (kw.Lower().StartsWith(NameUnderCursorLw))
                {
                    switch (kwcase)
                    {
                        case 0:
                        {
                            break;
                        }
                        case 1:
                        {
                            kw = kw.MakeUpper();
                            break;
                        }
                        case 2:
                        {
                            kw = kw.Mid(0,1).MakeUpper() + kw.Mid(1).MakeLower();
                            break;
                        }
                        default :
                        {
                            kw = kw.MakeLower();
                            break;
                        }
                    }
                    tokens.push_back(CCToken(wxNOT_FOUND, kw, iidx));
                }
            }
        }

        if (items.GetCount() == 0)
        {
            return;
        }
        items.Sort(SortCCList);

        // Remove duplicate items
        size_t i=0;
        size_t count=items.Count() - 1;
        while (i < count)
        {
            if (items.Item(i)==items.Item(i+1))
            {
                items.RemoveAt(i);
                count--;
            }
            else
                i++;
        }

        if (control->CallTipActive())
        {
            m_WasCallTipActive = true;
        }

        control->AutoCompSetIgnoreCase(true);
        control->AutoCompSetCancelAtStart(true);
        control->AutoCompSetFillUps(wxEmptyString);
        control->AutoCompSetAutoHide(true);
        control->AutoCompSetDropRestOfWord(m_IsAutoPopup ? false : true);
        ed->GetControl()->AutoCompSetSeparator('\n');
        ed->GetControl()->AutoCompSetMaxWidth(80);
        ed->GetControl()->AutoCompSetMaxHeight(16);
        wxString final = GetStringFromArray(items, _T("\n"));
        final.Trim();

        control->AutoCompShow(pos - start, final);

        return;
    }
    else if (!control->CallTipActive())
    {
        wxString msg = _("Too many results.\n"
                             "Please edit results' limit in code-completion options,\n"
                             "or type at least one more character to narrow the scope down.");
        control->CallTipShow(control->GetCurrentPos(), msg);
        return;
    }

    return;
}

FortranProject::CCProviderStatus FortranProject::GetProviderStatusFor(cbEditor* ed)
{
    if (ed && m_pNativeParser->IsFileFortran(ed->GetShortName()))
        return ccpsActive;
    return ccpsInactive;
}

std::vector<FortranProject::CCCallTip> FortranProject::GetCallTips(int pos, int style, cbEditor* ed, int& argsPos)
{
    argsPos = wxSCI_INVALID_POSITION;
    std::vector<FortranProject::CCCallTip> tips;
    if (!IsAttached() || !m_InitDone || !ed)
        return tips;

    int hlStart = wxSCI_INVALID_POSITION;
    int hlEnd   = wxSCI_INVALID_POSITION;
    int commas; // how many commas has the user typed so far?
    int commasPos; // how many commas until current position?
    wxArrayString callTips;
    wxArrayInt idxFuncSub;
    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* result = tokensTmp.GetTokens();
    TokenFlat* token = NULL;
    bool isAfterPercent = false;

    wxString lastName;
    wxString argNameUnderCursor;
    m_pNativeParser->CollectInformationForCallTip(commas, commasPos, argNameUnderCursor, lastName, isAfterPercent, argsPos, result);

    if (isAfterPercent && result->GetCount() > 0)
    {
        if (result->Item(0)->m_TokenKind == tkProcedure)
        {
            m_pNativeParser->GetCallTipsForTypeBoundProc(result, callTips);
            idxFuncSub.Add(1);
        }
        else if (result->Item(0)->m_TokenKind == tkInterface)
            m_pNativeParser->GetCallTipsForGenericTypeBoundProc(result, callTips, idxFuncSub);
        else if (result->Item(0)->m_TokenKind == tkVariable &&
                 Manager::Get()->GetConfigManager(_T("fortran_project"))->ReadBool(_T("/call_tip_arrays"), true))
        {
            wxString callTip;
            m_pNativeParser->GetCallTipsForVariable(result->Item(0), callTip);
            if (!callTip.IsEmpty())
                callTips.Add(callTip);
        }
    }
    else if (!lastName.IsEmpty())
    {
        m_pNativeParser->GetCallTips(lastName, m_LogOnlyUseAssoc, m_LogOnlyPublicNames, callTips, result);

        wxString kwName;
        if (lastName.IsSameAs(_T("open")))
            kwName = _T("__fortran_statement_") + lastName;
        else
            kwName = lastName;
        m_pKeywordsParser->GetCallTips(kwName, callTips, result);
    }

    bool isUnique = true;
    wxString definition;
    for (unsigned int i = 0; i < callTips.GetCount(); ++i)
    {
        if (!callTips[i].IsEmpty()) // non-empty
        {
            if (!definition.IsEmpty())
            {
                isUnique = false;
                break;
            }
            definition << callTips[i];
            token = result->Item(i);

            int nCommas = m_pNativeParser->CountCommas(callTips[i], 1, false);
            int commasDif = commas - nCommas;
            if (commasDif > 0)
            {
                for (int idif=0; idif< commasDif; idif++)
                {
                    definition << _T(", *???*");
                }
                definition << _T(" ");
            }
        }
    }

    if (isUnique && !argNameUnderCursor.IsEmpty())
    {
        // Determine number of commas before argNameUnderCursor
        int argidx = definition.Lower().Find(argNameUnderCursor.Lower());
        if (argidx != wxNOT_FOUND)
        {
            commasPos = m_pNativeParser->CountCommas(definition.Mid(0, argidx), 1, false);
        }
    }

    if (!definition.IsEmpty() && isUnique && token && token->m_TokenKind == tkVariable)
    {
        m_pNativeParser->GetCallTipHighlight(definition, commasPos, hlStart, hlEnd);
    }
    else if (!definition.IsEmpty() && isUnique &&
        (!isAfterPercent || ( isAfterPercent && result->GetCount() >= 2 && (result->Item(0)->m_TokenKind == tkProcedure) )))
    {
        m_pNativeParser->GetCallTipHighlight(definition, commasPos, hlStart, hlEnd);
        if (isAfterPercent)
            token = result->Item(1);

        if (token->m_TokenKind == tkSubroutine || token->m_TokenKind == tkFunction || token->m_TokenKind == tkType)
        {
            wxString argName = definition.Mid(hlStart,hlEnd-hlStart);
            argName = argName.BeforeFirst(_T(','));
            argName = argName.BeforeFirst(_T(')'));
            argName.Replace(_T("["),_T(" "));
            argName.Replace(_T("]"),_T(" "));
            argName.Trim().Trim(false);

            wxString argDecl;
            wxString argDescription;
            bool found = m_pNativeParser->GetParser()->FindTokenDeclaration(*token, argName, argDecl, argDescription);
            if (!found)
                found = m_pKeywordsParser->GetParser()->FindTokenDeclaration(*token, argName, argDecl, argDescription);
            if (found)
            {
                definition << _T('\n') << argDecl;
                if (!argDescription.IsEmpty())
                    definition << _T('\n') << _T("! ") << argDescription;
            }
        }
    }
    else if(!isUnique)
    {
        if (   (isAfterPercent && (callTips.GetCount() != idxFuncSub.GetCount()))
            || (!isAfterPercent && (callTips.GetCount() != result->GetCount())) )
            return tips;

        if (lastName.IsEmpty())
            return tips;

        for (size_t i=0; i < callTips.GetCount(); ++i)
        {
            definition = _T("");
            if (isAfterPercent)
                definition << result->Item(idxFuncSub[i])->m_DisplayName << _T('\n');
            else
                definition << result->Item(i)->m_DisplayName << _T('\n');
            int mStart = definition.length();

            wxString ctdef = callTips.Item(i);
            int nCommas = m_pNativeParser->CountCommas(ctdef, 1, false);
            int commasDif = commas - nCommas;
            if (commasDif > 0)
            {
                for (int j=0; j<commasDif; j++)
                {
                    ctdef << _T(", *???*");
                }
                ctdef << _T(" ");
            }
            definition << ctdef;

            m_pNativeParser->GetCallTipHighlight(ctdef, commasPos, hlStart, hlEnd);
            if (isAfterPercent)
                token = result->Item(idxFuncSub[i]);
            else
                token = result->Item(i);
            if (token->m_TokenKind == tkSubroutine || token->m_TokenKind == tkFunction)
            {
                wxString argName = callTips.Item(i).Mid(hlStart,hlEnd-hlStart);
                argName = argName.BeforeFirst(_T(','));
                argName = argName.BeforeFirst(_T(')'));
                argName.Replace(_T("["),_T(" "));
                argName.Replace(_T("]"),_T(" "));
                argName.Trim().Trim(false);

                wxString argDecl;
                wxString argDescription;
                if (m_pNativeParser->GetParser()->FindTokenDeclaration(*token, argName, argDecl, argDescription))
                {
                    definition << _T('\n') << argDecl;
                    if (!argDescription.IsEmpty())
                        definition << _T('\n') << _T("! ") << argDescription;
                }
            }
            hlStart += mStart;
            hlEnd   += mStart;
            if (!definition.IsEmpty())
                tips.push_back(CCCallTip(definition, hlStart, hlEnd));
        }
    }

    if (isUnique && !definition.IsEmpty())
        tips.push_back(CCCallTip(definition, hlStart, hlEnd));

    if (!tips.empty())
    {
        if (m_LogUseWindow && (!m_WasCallTipInfoLog || !m_LastCallTipName.IsSameAs(lastName)))
            ShowInfoLog(result, isAfterPercent);

        m_ShowedCallTip = true;
        m_LastCallTipName = lastName;
        m_WasCallTipInfoLog = true;
    }

    return tips;
}

std::vector<FortranProject::CCToken> FortranProject::GetTokenAt(int position, cbEditor* ed, bool& allowCallTip)
{
    // Get tokens for tooltip.
    std::vector<CCToken> tokens;
    if (!IsAttached() || !m_InitDone)
        return tokens;

    cbEditor* edLoc = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (ed != edLoc)
        return tokens;

    if (!m_pNativeParser->IsFileFortran(ed->GetShortName()))
        return tokens;

    allowCallTip = false;

    cbStyledTextCtrl* control = ed->GetControl();

    if ((m_ShowedCallTip && control->CallTipActive()) || control->AutoCompActive())
        return tokens;

    const int style = control->GetStyleAt(position);
    if (style != wxSCI_F_DEFAULT && style != wxSCI_F_OPERATOR && style != wxSCI_F_IDENTIFIER
            && style != wxSCI_F_OPERATOR2 && style != wxSCI_F_WORD && style != wxSCI_F_WORD2
            && style != wxSCI_F_WORD3)
        return tokens;

    int endOfWord = control->WordEndPosition(position, true);
    int startOfWord = control->WordStartPosition(position, true);
    wxString nameUnder = control->GetTextRange(startOfWord, endOfWord);
    if (nameUnder.IsEmpty())
        return tokens;

    ParserF* pParser = m_pNativeParser->GetParser();
    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* result = tokensTmp.GetTokens();

    bool isAfterPercent = false;
    pParser->FindMatchTokensForToolTip(nameUnder, endOfWord, ed, m_LogOnlyUseAssoc, m_LogOnlyPublicNames, *result, isAfterPercent);
    if (result->IsEmpty())
        m_pKeywordsParser->FindTokens(nameUnder, *result);

    if (result->size() > 32 || result->size() == 0)
        return tokens;

    bool type_bound = false;
    wxString msg;
    for (size_t i=0; i<result->GetCount(); ++i)
    {
        TokenFlat* token = result->Item(i);
        if (!token->m_Rename.IsEmpty())
        {
            msg << token->m_Rename << _T(" => ") << token->m_DisplayName << _T("\n");
        }
        if (token->m_TokenKind == tkVariable)
        {
            msg << token->m_TypeDefinition << _T(" :: ") << token->m_DisplayName << token->m_Args << _T("\n");
        }
        else if (token->m_TokenKind == tkType)
        {
            msg << _T("type: ") << token->m_DisplayName << _T("\n");
        }
        else if (token->m_TokenKind == tkSubroutine)
        {
            msg << _T("subroutine ") << token->m_DisplayName << token->m_Args << _T("\n");
        }
        else if (token->m_TokenKind == tkFunction)
        {
            if (!token->m_PartFirst.IsEmpty())
            {
                msg << token->m_PartFirst << _T(" ");
            }
            msg << _T("function ") << token->m_DisplayName << token->m_Args << _T("\n");
        }
        else if (token->m_TokenKind == tkProcedure)
        {
            TokenFlat* token2 = 0;
            if (result->GetCount() > i+1)
            {
                i++;
                token2 = result->Item(i);
            }
            pParser->FindTooltipForTypeBoundProc(msg, token, token2);
            type_bound = true;
        }
        else if (isAfterPercent && token->m_TokenKind == tkInterface)
        {
            while (i < result->GetCount())
            {
                token = result->Item(i);
                if (token->m_TokenKind == tkInterface)
                {
                    wxString specNames = token->m_PartLast;
                    specNames.Replace(_T(" "),_T(", "));
                    msg << _T("generic :: ") << token->m_DisplayName << _T(" => ") << specNames << _T("\n");
                }
                i++;
            }
            type_bound = true;
        }
        else
        {
            msg << token->GetTokenKindString() << _T(" ") << token->m_DisplayName << token->m_Args << _T("\n");
        }
        wxString doc = HtmlDoc::GetDocForTooltip(token);
        if (!doc.IsEmpty())
            msg << _T("! ") << doc << _T("\n");
    }
    if (result->GetCount() == 1 && !type_bound)
    {
        if (!result->Item(0)->m_Filename.IsEmpty())
        {
            if (result->Item(0)->m_ParentTokenKind == tkModule)
            {
                msg << result->Item(0)->m_ParentDisplayName << _T(", ");
            }
            msg << result->Item(0)->m_Filename.AfterLast(wxFILE_SEP_PATH) << _T(":") << result->Item(0)->m_LineStart;
        }
        else
            msg.Trim();
    }
    else
        msg.Trim();

    if (!m_IsDebugging)
    {
        tokens.push_back(CCToken(wxNOT_FOUND, msg));
        m_ShowedCallTip = false;
    }

    if (result->GetCount() >= 1 && m_LogUseWindow)
    {
        ShowInfoLog(result, isAfterPercent);
        m_WasCallTipInfoLog = false;
    }

    return tokens;
}

void FortranProject::ShowInfoLog(TokensArrayFlat* result, bool isAfterPercent)
{
    if (!m_LogUseWindow)
        return;
    if (result->GetCount() == 0)
        return;

    wxString logMsg;
    wxString fileNameOld;

    if (!isAfterPercent)
    {
        unsigned int countMax = 20<result->GetCount() ? 20 : result->GetCount();
        for (unsigned int i=0; i < countMax; i++)
        {
            TokenFlat* token = result->Item(i);
            wxString logMsg1;
            bool readFile;
            if (token->m_Filename.IsSameAs(fileNameOld))
            {
                readFile = false;
            }
            else
            {
                readFile = true;
                fileNameOld = token->m_Filename;
            }

            if (!token->m_Rename.IsEmpty())
            {
                logMsg << token->m_Rename << _T(" => ") << token->m_DisplayName << _T("\n");
            }

            if (token->m_TokenKind == tkSubroutine || token->m_TokenKind == tkFunction || token->m_TokenKind == tkType)
            {
                if (m_pNativeParser->GetParser()->FindInfoLog(*token,m_LogComAbove,m_LogComBelow,m_LogDeclar,m_LogComVariab,logMsg1,readFile))
                {
                    logMsg << logMsg1 << _T("\n\n");
                }
            }
            else if (token->m_TokenKind == tkInterface)
            {
                if (m_pNativeParser->GetParser()->GetTokenStr(*token, logMsg1))
                    logMsg << logMsg1 << _T("\n\n");

                fileNameOld.Empty();
            }
        }
        if (!logMsg.IsEmpty())
        {
            if (countMax < result->GetCount())
            {
                logMsg << wxString::Format(_T("!*********** %d more interfaces was not showed *************"),int(result->GetCount())-int(countMax));
            }
            WriteToLog(logMsg);
        }
    }
    else //isAfterPercent
    {
        if (result->Item(0)->m_TokenKind == tkProcedure)
            m_pNativeParser->GetParser()->FindInfoLogForTypeBoundProc(*result,m_LogComAbove,m_LogComBelow,m_LogDeclar,m_LogComVariab,logMsg);
        else if (result->Item(0)->m_TokenKind == tkInterface)
            m_pNativeParser->GetParser()->FindInfoLogForGenericTBProc(*result,m_LogComAbove,m_LogComBelow,m_LogDeclar,m_LogComVariab,logMsg);
        if (!logMsg.IsEmpty())
        {
            WriteToLog(logMsg);
        }
    }
}


cbConfigurationPanel* FortranProject::GetConfigurationPanel(wxWindow* parent)
{
    FPOptionsDlg* dlg = new FPOptionsDlg(parent, m_pNativeParser, this);
    return dlg;
}

cbConfigurationPanel* FortranProject::GetProjectConfigurationPanel(wxWindow* parent, cbProject* project)
{
    if (!m_pNativeParser)
        return nullptr;

    if (m_pNativeParser->HasFortranFiles(project))
    {
        FPOptionsProjectDlg* dlg = new FPOptionsProjectDlg(parent, project, m_pNativeParser);
        return dlg;
    }
    return nullptr;
}

void FortranProject::OnProjectLoadingHook(cbProject* prj, TiXmlElement* elem, bool loading)
{
    if (!prj || !elem)
        return; // ?! Should actually NOT happen...

    if (loading)
    {
        // Project is loaded
        wxArrayString adirs;
        TiXmlElement* node = elem->FirstChildElement("fortran_project");
        if (node)
        {
            for(TiXmlElement* e = node->FirstChildElement("additional_dir"); e != NULL; e = e->NextSiblingElement("additional_dir"))
            {
                adirs.Add(cbC2U(e->Attribute("dir")));
            }
        }
        m_pNativeParser->SetProjectSearchDirs(prj, adirs);
    }
    else
    {
        // Hook called when saving project file

        TiXmlElement* node = elem->FirstChildElement("fortran_project");
        if (!node)
            node = elem->InsertEndChild(TiXmlElement("fortran_project"))->ToElement();
        node->Clear();

        wxArrayString adirs = m_pNativeParser->GetProjectSearchDirs(prj);
        for (size_t i=0; i<adirs.size(); ++i)
        {
            TiXmlElement* e = node->InsertEndChild(TiXmlElement("additional_dir"))->ToElement();
            e->SetAttribute("dir", cbU2C(adirs.Item(i)));
        }
    }
}

void FortranProject::RereadOptions()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));

    m_LexerKeywordsToInclude[0] = cfg->ReadBool(_T("/lexer_keywords_set1"), true);
    m_LexerKeywordsToInclude[1] = cfg->ReadBool(_T("/lexer_keywords_set2"), true);
    m_LexerKeywordsToInclude[2] = cfg->ReadBool(_T("/lexer_keywords_set3"), false);
    m_LexerKeywordsToInclude[3] = cfg->ReadBool(_T("/lexer_keywords_set4"), false);
    m_LexerKeywordsToInclude[4] = cfg->ReadBool(_T("/lexer_keywords_set5"), false);
    m_LexerKeywordsToInclude[5] = cfg->ReadBool(_T("/lexer_keywords_set6"), false);
    m_LexerKeywordsToInclude[6] = cfg->ReadBool(_T("/lexer_keywords_set7"), false);
    m_LexerKeywordsToInclude[7] = cfg->ReadBool(_T("/lexer_keywords_set8"), false);
    m_LexerKeywordsToInclude[8] = cfg->ReadBool(_T("/lexer_keywords_set9"), false);

    m_MaxMatch = cfg->ReadInt(_T("/max_matches"), 1000);
    if (m_MaxMatch < 1)
        m_MaxMatch = 1;

    m_UseSmartCC = cfg->ReadBool(_T("/use_smart_code_completion"), true);
    m_LogOnlyUseAssoc = cfg->ReadBool(_T("/only_use_associated"), true);
    m_LogOnlyPublicNames = !cfg->ReadBool(_T("/show_hidden_entities"), false);
    m_LogShowTypeVariables = cfg->ReadBool(_T("/show_type_variables"), true);

    m_LogUseWindow = cfg->ReadBool(_T("/use_log_window"), true);
    m_LogComAbove = cfg->ReadBool(_T("/include_comments_above"), true);
    m_LogComBelow = cfg->ReadBool(_T("/include_comments_below"), true);
    m_LogDeclar = cfg->ReadBool(_T("/include_declarations_log"), true);
    m_LogComVariab = cfg->ReadBool(_T("/include_log_comments_variable"), true);

    int docsOpt = cfg->ReadInt(_T("/show_docs_window"), 1);
    if (docsOpt == 0)
        m_DocsShowOption = dsoAlways;
    else if (docsOpt == 1)
        m_DocsShowOption = dsoOnly;
    else
        m_DocsShowOption = dsoNot;

    m_AutoInsertEnabled = cfg->ReadBool(_T("/auto_insert_enabled"), true);
    m_AutoInsert.ReadAIOptions();

    if (!m_pFortranLog && m_LogUseWindow)
    {
        CreateLogWindow();
    }
    else if (m_pFortranLog && !m_LogUseWindow)
    {
        RemoveLogWindow(false);
    }

    m_ConstrHighlighter.ReadOptions();
}

void FortranProject::WriteToLog(const wxString& text)
{
    if (m_pFortranLog)
    {
        m_pFortranLog->WriteToInfoWindow(text);
    }
}

void FortranProject::CreateLogWindow()
{
    m_pFortranLog = new FInfoWindow();
}

void FortranProject::RemoveLogWindow(bool appShutDown)
{
    if (appShutDown)
        return;

    if (m_pFortranLog)
    {
        m_pFortranLog->RemoveFromNotebook();
        m_pFortranLog = 0L;
    }
}

void FortranProject::OnJumpBack(wxCommandEvent& event)
{
    JumpTracker* jTr = m_pNativeParser->GetJumpTracker();

    if (!jTr->IsJumpBackEmpty())
    {
        jTr->MakeJumpBack();
        CheckEnableToolbar();
        JumpToLine(jTr->GetHomeAddress());
    }
}

void FortranProject::OnJumpHome(wxCommandEvent& event)
{
    JumpTracker* jTr = m_pNativeParser->GetJumpTracker();

    if (!jTr->IsJumpHomeEmpty())
        JumpToLine(jTr->GetHomeAddress());
}

void FortranProject::OnJumpForward(wxCommandEvent& event)
{
    JumpTracker* jTr = m_pNativeParser->GetJumpTracker();
    if (!jTr->IsJumpForwardEmpty())
    {
        jTr->MakeJumpForward();
        CheckEnableToolbar();
        JumpToLine(jTr->GetHomeAddress());
    }
}

void FortranProject::CheckEnableToolbar()
{
    m_pToolbar->EnableTool(XRCID("idFortProjBack"), !m_pNativeParser->GetJumpTracker()->IsJumpBackEmpty());
    m_pToolbar->EnableTool(XRCID("idFortProjHome"), !m_pNativeParser->GetJumpTracker()->IsJumpHomeEmpty());
    m_pToolbar->EnableTool(XRCID("idFortProjForward"), !m_pNativeParser->GetJumpTracker()->IsJumpForwardEmpty());

    wxMenuItem* pJumpBack = m_FortranToolsMenu->FindItem(idMenuJumpBack);
    pJumpBack->Enable(!m_pNativeParser->GetJumpTracker()->IsJumpBackEmpty());
    wxMenuItem* pJumpHome = m_FortranToolsMenu->FindItem(idMenuJumpHome);
    pJumpHome->Enable(!m_pNativeParser->GetJumpTracker()->IsJumpHomeEmpty());
    wxMenuItem* pJumpForward = m_FortranToolsMenu->FindItem(idMenuJumpForward);
    pJumpForward->Enable(!m_pNativeParser->GetJumpTracker()->IsJumpForwardEmpty());
}

void FortranProject::JumpToLine(const LineAddress& adr)
{
    if (!IsAttached() || Manager::IsAppShuttingDown())
        return;

    EditorManager* edMan = Manager::Get()->GetEditorManager();
    if (cbEditor* ed = edMan->Open(adr.GetFilename()))
    {
        ed->GotoLine(adr.GetLineNumber());
    }
}

void FortranProject::OnDebuggerStarted(CodeBlocksEvent& event)
{
    event.Skip();
    m_IsDebugging = true;
}

void FortranProject::OnDebuggerFinished(CodeBlocksEvent& event)
{
    event.Skip();
    m_IsDebugging = false;
}

void FortranProject::OnGenerateMakefile(wxCommandEvent& event)
{
    m_pNativeParser->GenMakefile();
}

void FortranProject::OnChangeCase(wxCommandEvent& event)
{
    ChangeCase changCaseDlg(Manager::Get()->GetAppWindow());
    changCaseDlg.ShowModal();
}

void FortranProject::OnTab2Space(wxCommandEvent& event)
{
    Tab2Space tab2SpaceDlg(Manager::Get()->GetAppWindow());
    tab2SpaceDlg.ShowModal();
}

void FortranProject::OnBindTo(wxCommandEvent& event)
{
    cbProject* pr = Manager::Get()->GetProjectManager()->GetActiveProject();
    if (pr)
        pr->SaveAllFiles();

    Bindto bindto(Manager::Get()->GetAppWindow(), m_pNativeParser->GetParser());
    bindto.ShowModal();
}

void FortranProject::OnFormatIndent(wxCommandEvent& event)
{
    FormatIndent indent;
    indent.Format();
}

void FortranProject::OnReparseEditorTimer(wxTimerEvent& event)
{
    m_pNativeParser->ReparseCurrentEditor();
}

wxString FortranProject::GetIncludeFilename(cbStyledTextCtrl* control)
{
    if (!control)
        return wxEmptyString;
    wxString strName;
    int style = control->GetStyleAt(control->GetCurrentPos());
    if (style == wxSCI_F_STRING1 || style == wxSCI_F_STRING2 || style == wxSCI_F_PREPROCESSOR)
    {
        wxString curLine = control->GetCurLine().Lower();
        if (!curLine.Trim(false).StartsWith(_T("include")) &&
            !curLine.Trim(false).StartsWith(_T("#include")))
            return wxEmptyString;

        int pos   = control->GetCurrentPos();
        int lineCur = control->LineFromPosition(pos);
        int lineStartPos = control->PositionFromLine(lineCur);
        wxString strBefore = control->GetTextRange(lineStartPos, pos).Lower().Trim(false);
        int idx1 = strBefore.Find('"', true);
        int idx2 = strBefore.Find('\'', true);
        int idx3 = strBefore.Find('<', true);
        if ((idx1 == wxNOT_FOUND && idx2 == wxNOT_FOUND && idx3 == wxNOT_FOUND) ||
            (idx1 != wxNOT_FOUND && idx2 != wxNOT_FOUND) ||
            (idx1 != wxNOT_FOUND && idx3 != wxNOT_FOUND) ||
            (idx2 != wxNOT_FOUND && idx3 != wxNOT_FOUND))
            return wxEmptyString;
        int idx = (idx1 != wxNOT_FOUND) ? idx1 : idx2;
        idx = (idx != wxNOT_FOUND) ? idx : idx3;
        if (strBefore.Mid(0,idx).Trim().Trim(false) != _T("include") &&
            strBefore.Mid(0,idx).Trim().Trim(false) != _T("#include"))
            return wxEmptyString;

        wxChar ch = curLine[idx];
        if (ch == '<')
            ch = '>';
        wxString strLast = curLine.Mid(strBefore.size());
        int idxL = strLast.Find(ch);
        if (idxL == wxNOT_FOUND)
            return wxEmptyString;
        int idxE = strBefore.size() + idxL;
        strName = curLine.Mid(idx+1, idxE-(idx+1)).Trim().Trim(false);
    }
    return strName;
}

void FortranProject::OnMenuEditPaste(wxCommandEvent& event)
{
    // Process clipboard data only if we have the focus
    if (!IsAttached() || !m_InitDone)
    {
        event.Skip();
        return;
    }
    if (m_pNativeParser->GetWorkspaceBrowser())
        m_pNativeParser->GetWorkspaceBrowser()->OnMenuEditPaste(event);
    else
        event.Skip();
}

wxString FortranProject::GetDocumentation(const CCToken& tok)
{
    if (tok.id == wxNOT_FOUND || m_DocsShowOption == dsoNot)
        return wxEmptyString;

    TokensArrayFlat* tokens = m_TokensCCList.GetTokens();
    if (tok.id >= int(tokens->GetCount()))
        return wxEmptyString;

    wxString doc;
    TokenFlat* token = tokens->Item(tok.id);

    bool hasDoc;
    doc = HtmlDoc::GenerateHtmlDoc(token, tok.id, hasDoc);
    if (m_DocsShowOption == dsoOnly && !hasDoc)
        return wxEmptyString;

    return doc;
}

wxString FortranProject::OnDocumentationLink(wxHtmlLinkEvent& event, bool& dismissPopup)
{
    bool isGoto = false;
    long int tokenIdx;
    wxString doc = HtmlDoc::OnDocumentationLink(event, dismissPopup, isGoto, tokenIdx);

    if (isGoto)
    {
        TokensArrayFlat* tokens = m_TokensCCList.GetTokens();
        if (tokenIdx >= long(tokens->GetCount()))
            return wxEmptyString;

        TokenFlat* pToken = tokens->Item(tokenIdx);
        if ( pToken->m_Filename.EndsWith(UnixFilename(_T("/fortranproject/fortran_intrinsic_modules.f90")))
          || pToken->m_Filename.EndsWith(UnixFilename(_T("/fortranproject/fortran_procedures.f90"))) )
        {
            // don't go to fortran_intrinsic_modules.f90
            dismissPopup = false;
            return doc;
        }
        cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
        if (!ed)
            return doc;
        if (!GotoToken(pToken, ed))
            dismissPopup = false;
    }
    return doc;
}

void FortranProject::OnShowCallTreeView(wxCommandEvent& event)
{
    CodeBlocksDockEvent evt(event.IsChecked() ? cbEVT_SHOW_DOCK_WINDOW : cbEVT_HIDE_DOCK_WINDOW);
    evt.pWindow = (wxWindow*)m_pCallTree->GetCallTreeView();
    Manager::Get()->ProcessEvent(evt);
}

void FortranProject::OnShowCallTree(wxCommandEvent& event)
{
    bool showCallTree = false;
    if (event.GetId() == idShowCallTree)
        showCallTree = true;

    ShowCallTree(showCallTree);
}

void FortranProject::ShowCallTree(bool showCallTree)
{
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (!ed)
        return;

    if (!m_pNativeParser->IsFileFortran(ed->GetFilename()))
        return;

    // Make fortran keyword set
    std::set< wxString> keywordSet;
    EditorColourSet* theme = ed->GetColourSet();
    if (!theme)
        return;

    HighlightLanguage lang = theme->GetLanguageForFilename(_T(".")+wxFileName(ed->GetFilename()).GetExt());
    for (int i = 0; i <= wxSCI_KEYWORDSET_MAX; ++i)
    {
        wxString keywords = theme->GetKeywords(lang, i);
        wxStringTokenizer tkz(keywords, _T(" \t\r\n"), wxTOKEN_STRTOK);
        while (tkz.HasMoreTokens())
        {
            wxString kw = tkz.GetNextToken();
            if (keywordSet.find(kw) != keywordSet.end())
                continue;
            keywordSet.insert(kw);
        }
    }

    wxString NameUnderCursor;
    bool isOperator;
    if(!EditorHasNameUnderCursor(NameUnderCursor, isOperator))
        return;
    if (isOperator)
        return;

    ParserF* pParser = m_pNativeParser->GetParser();
    m_pCallTree->BuildCallTree(ed, NameUnderCursor, pParser, keywordSet, showCallTree);
}

void FortranProject::LoadFortranKeywordImages()
{
    wxString prefix = ConfigManager::GetDataFolder() + _T("/images/fortranproject/");
    m_FKImages[16] = cbLoadBitmap(prefix + _T("16x16/fortran_keyword.png"), wxBITMAP_TYPE_PNG);
    m_FKImages[20] = cbLoadBitmap(prefix + _T("20x20/fortran_keyword.png"), wxBITMAP_TYPE_PNG);
    m_FKImages[24] = cbLoadBitmap(prefix + _T("24x24/fortran_keyword.png"), wxBITMAP_TYPE_PNG);
    m_FKImages[28] = cbLoadBitmap(prefix + _T("28x28/fortran_keyword.png"), wxBITMAP_TYPE_PNG);
    m_FKImages[32] = cbLoadBitmap(prefix + _T("32x32/fortran_keyword.png"), wxBITMAP_TYPE_PNG);
    m_FKImages[40] = cbLoadBitmap(prefix + _T("40x40/fortran_keyword.png"), wxBITMAP_TYPE_PNG);
    m_FKImages[48] = cbLoadBitmap(prefix + _T("48x48/fortran_keyword.png"), wxBITMAP_TYPE_PNG);
    m_FKImages[56] = cbLoadBitmap(prefix + _T("56x56/fortran_keyword.png"), wxBITMAP_TYPE_PNG);
    m_FKImages[64] = cbLoadBitmap(prefix + _T("64x64/fortran_keyword.png"), wxBITMAP_TYPE_PNG);
}

wxBitmap FortranProject::GetFortranKeywordImage(int height)
{
    if (height <= 16)
        return m_FKImages[16];
    else if (height <= 20)
        return m_FKImages[20];
    else if (height <= 24)
        return m_FKImages[24];
    else if (height <= 28)
        return m_FKImages[28];
    else if (height <= 32)
        return m_FKImages[32];
    else if (height <= 40)
        return m_FKImages[40];
    else if (height <= 48)
        return m_FKImages[48];
    else if (height <= 56)
        return m_FKImages[56];

    return m_FKImages[64];
}
