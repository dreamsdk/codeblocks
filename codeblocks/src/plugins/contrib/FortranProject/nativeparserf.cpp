/*
 * This file licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 */

#include "nativeparserf.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/regex.h>
    #include <wx/log.h>
    #include <wx/string.h>
    #include <wx/tokenzr.h>
    #include <wx/dir.h>
    #include <wx/wfstream.h>
    #include <wx/stopwatch.h>

    #include <manager.h>
    #include <configmanager.h>
    #include <editormanager.h>
    #include <projectmanager.h>
    #include <pluginmanager.h>
    #include <logmanager.h>
    #include <cbauibook.h>
    #include <cbeditor.h>
    #include <cbproject.h>
    #include <cbexception.h>
    #include <projectloader_hooks.h>
    #include <cbstyledtextctrl.h>
    #include <tinyxml/tinyxml.h>
#endif
#include <cctype>

#include "workspacebrowserf.h"
#include "workspacebrowserbuilder.h"
#include "parserf.h"
#include "makefilegen.h"
#include "bufferparserthread.h"
#include "adddirparserthread.h"

static wxCriticalSection s_CurrentBufferCritSect;


int idWSPThreadEvent          = wxNewId();
int idADirPThreadEvent        = wxNewId();
int idBPThreadEvent           = wxNewId();
int idWorkspaceReparseTimer   = wxNewId();
int idASearchDirsReparseTimer = wxNewId();
BEGIN_EVENT_TABLE(NativeParserF, wxEvtHandler)
    EVT_COMMAND(idWSPThreadEvent, wxEVT_COMMAND_ENTER, NativeParserF::OnUpdateWorkspaceBrowser)
    EVT_COMMAND(idADirPThreadEvent, wxEVT_COMMAND_ENTER, NativeParserF::OnUpdateADirTokens)
    EVT_COMMAND(idBPThreadEvent, wxEVT_COMMAND_ENTER, NativeParserF::OnUpdateCurrentFileTokens)
    EVT_TIMER(idWorkspaceReparseTimer, NativeParserF::OnReparseWorkspaceTimer)
    EVT_TIMER(idASearchDirsReparseTimer, NativeParserF::OnASearchDirsReparseTimer)
END_EVENT_TABLE()

NativeParserF::NativeParserF(FortranProject* forproj)
    : m_pWorkspaceBrowser(0),
      m_WorkspaceBrowserIsFloating(false),
      m_pFortranProject(forproj),
      m_WorkspaceReparseTimer(this, idWorkspaceReparseTimer),
      m_ThreadPool(this, wxNewId(), 2, 2 * 1024 * 1024),
      m_ASearchDirsReparseTimer(this, idASearchDirsReparseTimer)
{
}

NativeParserF::~NativeParserF()
{
    RemoveWorkspaceBrowser();
    ClearParser();
}

void NativeParserF::CreateWorkspaceBrowser()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));
    m_WorkspaceBrowserIsFloating = cfg->ReadBool(_T("/as_floating_window"), false);

    if (cfg->ReadBool(_T("/use_symbols_browser"), true))
    {
        if (!m_pWorkspaceBrowser)
        {
            if (!m_WorkspaceBrowserIsFloating)
            {
                // make this a tab in projectmanager notebook
                cbAuiNotebook* bk = Manager::Get()->GetProjectManager()->GetUI().GetNotebook();
                m_pWorkspaceBrowser = new WorkspaceBrowserF(bk, this, &m_Parser);
                Manager::Get()->GetProjectManager()->GetUI().GetNotebook()->AddPage(m_pWorkspaceBrowser, _("FSymbols"));
            }
            else
            {
                // make this a free floating/docking window
                m_pWorkspaceBrowser = new WorkspaceBrowserF(Manager::Get()->GetAppWindow(), this, &m_Parser);
                CodeBlocksDockEvent evt(cbEVT_ADD_DOCK_WINDOW);

                evt.name = _T("FSymbolsBrowser");
                evt.title = _("FSymbols browser");
                evt.pWindow = m_pWorkspaceBrowser;
                evt.dockSide = CodeBlocksDockEvent::dsRight;
                evt.desiredSize.Set(200, 250);
                evt.floatingSize.Set(200, 250);
                evt.minimumSize.Set(150, 150);
                evt.shown = true;
                evt.hideable = true;
                Manager::Get()->ProcessEvent(evt);
            }
            m_pWorkspaceBrowser->UpdateSash();
        }
    }
}

WorkspaceBrowserF* NativeParserF::GetWorkspaceBrowser()
{
    return m_pWorkspaceBrowser;
}

void NativeParserF::RemoveWorkspaceBrowser()
{
    if (m_pWorkspaceBrowser)
    {
        if (!m_WorkspaceBrowserIsFloating)
        {
            int idx = Manager::Get()->GetProjectManager()->GetUI().GetNotebook()->GetPageIndex(m_pWorkspaceBrowser);
            if (idx != -1)
                Manager::Get()->GetProjectManager()->GetUI().GetNotebook()->RemovePage(idx);
        }
        else
        {
            CodeBlocksDockEvent evt(cbEVT_REMOVE_DOCK_WINDOW);
            evt.pWindow = m_pWorkspaceBrowser;
            Manager::Get()->ProcessEvent(evt);
        }
        m_pWorkspaceBrowser->Destroy();
    }
    m_pWorkspaceBrowser = 0L;
}

void NativeParserF::UpdateWorkspaceBrowser(bool selectCurrentSymbol)
{
    if (m_pWorkspaceBrowser && !Manager::IsAppShuttingDown())
    {
        wxCriticalSectionLocker locker(s_CritSect);

        m_pWorkspaceBrowser->UpdateView();
    }
    MarkCurrentSymbol(selectCurrentSymbol);
}

void NativeParserF::AddParser(cbProject* project)
{
    if (!project)
        return;

    ParseProject(project);
}

void NativeParserF::ClearParser()
{
    m_Parser.Clear();
}

void NativeParserF::RemoveFromParser(cbProject* project)
{
    if (Manager::Get()->GetProjectManager()->GetProjects()->GetCount() == 0)
    {
        m_Parser.Clear();
        UpdateWorkspaceBrowser();

        return;
    }
    if (!project)
        return;

    for (FilesList::iterator it = project->GetFilesList().begin(); it != project->GetFilesList().end(); ++it)
    {
        ProjectFile* pf = *it;
        m_Parser.RemoveFile(pf->file.GetFullPath());
    }
    RemoveProjectFilesDependency(project);
}

bool NativeParserF::IsFileFortran(const wxString& filename)
{
    FortranSourceForm fsForm;
    return IsFileFortran(filename, fsForm);
}

bool NativeParserF::IsFileFortran(const wxString& filename, FortranSourceForm& fsForm)
{
   return m_Parser.IsFileFortran(filename, fsForm);
}

void NativeParserF::AddFileToParser(const wxString& projectFilename, const wxString& filename)
{
    FortranSourceForm fsForm;
    if (IsFileFortran(filename, fsForm))
    {
        m_Parser.Reparse(projectFilename, filename, fsForm);
    }
}

void NativeParserF::RemoveFileFromParser(const wxString& filename)
{
    m_Parser.RemoveFile(filename);
}

void NativeParserF::ParseProject(cbProject* project)
{
    wxArrayString files;
    FortranSourceForm fsForm;
    ArrayOfFortranSourceForm fileForms;
    wxArrayString prFilenameArr;
    wxString prFName = project->GetFilename();

    for (FilesList::iterator it = project->GetFilesList().begin(); it != project->GetFilesList().end(); ++it)
    {
        ProjectFile* pf = *it;

        if (IsFileFortran(pf->relativeFilename, fsForm))
        {
            files.Add(pf->file.GetFullPath());
            fileForms.push_back(fsForm);
            prFilenameArr.Add(prFName);
        }
    }
    if (!files.IsEmpty())
    {
        m_Parser.BatchParse(prFilenameArr, files, fileForms);
    }
}

void NativeParserF::ReparseFile(const wxString& projectFilename, const wxString& filename)
{
    FortranSourceForm fsForm;
    if (IsFileFortran(filename, fsForm))
        m_Parser.Reparse(projectFilename, filename, fsForm);
}

void NativeParserF::ReparseProject(cbProject* project)
{
    wxStopWatch sw;

    if (project && !Manager::IsAppShuttingDown())
    {
        wxString projectFilename = project->GetFilename();
        for (FilesList::iterator it = project->GetFilesList().begin(); it != project->GetFilesList().end(); ++it)
        {
            ProjectFile* pf = *it;
            ReparseFile(projectFilename, pf->file.GetFullPath());
        }
    }

    Manager::Get()->GetLogManager()->DebugLog(F(_T("NativeParserF::ReparseProject: Reparse poject took %d ms."), sw.Time()));
}

void NativeParserF::ForceReparseWorkspace()
{
    if (Manager::IsAppShuttingDown())
        return;

    cbProject* project = Manager::Get()->GetProjectManager()->GetActiveProject();
    if (project && m_pWorkspaceBrowser)
        m_pWorkspaceBrowser->SetActiveProject(project);
    m_WorkspaceReparseTimer.Start(500, wxTIMER_ONE_SHOT);
}

void NativeParserF::OnReparseWorkspaceTimer(wxTimerEvent& event)
{
    if (Manager::IsAppShuttingDown())
        return;

    if (s_WorkspaceParserMutex.TryLock() == wxMUTEX_NO_ERROR)
    {
        MakeWSFileList();
        s_WorkspaceParserMutex.Unlock();

        WorkspaceParserThread* thread = new WorkspaceParserThread(this, idWSPThreadEvent);
        m_ThreadPool.AddTask(thread, true);
    }

    OnASearchDirsReparseTimer(event);
}

void NativeParserF::MakeWSFileList()
{
    FortranSourceForm fsForm;
    m_WSFiles.clear();
    m_WSFileForms.clear();

    ProjectsArray* projects = Manager::Get()->GetProjectManager()->GetProjects();
    for (size_t i = 0; i < projects->GetCount(); ++i)
    {
        cbProject* proj = projects->Item(i);
        wxString prFName = proj->GetFilename();

        for (FilesList::iterator it = proj->GetFilesList().begin(); it != proj->GetFilesList().end(); ++it)
        {
            ProjectFile* pf = *it;

            if (IsFileFortran(pf->relativeFilename, fsForm))
            {
                m_WSFiles.Add(pf->file.GetFullPath());
                m_WSFileForms.push_back(fsForm);
                m_WSFilePFN.push_back(prFName);
            }
        }
    }
}

void NativeParserF::MakeADirFileList()
{
    FortranSourceForm fsForm;

    m_ADirFiles.clear();
    m_ADirFileForms.clear();
    m_ADirFNameToProjMap.clear();

    for (auto it=m_ASearchDirs.begin(); it != m_ASearchDirs.end(); ++it)
    {
        wxArrayString files;
        wxArrayString* pDirs = &it->second;
        for (size_t i=0; i<pDirs->size(); ++i)
        {
            wxDir::GetAllFiles(pDirs->Item(i), &files, wxEmptyString, wxDIR_FILES);
        }

        size_t nfiles = files.size();
        for (size_t i=0; i<nfiles; i++)
        {
            if (IsFileFortran(files.Item(i), fsForm))
            {
                if (m_ADirFNameToProjMap.count(files.Item(i)) == 0)
                {
                    m_ADirFiles.Add(files.Item(i));
                    m_ADirFileForms.push_back(fsForm);

                    wxArrayString prarr;
                    prarr.Add(it->first);
                    m_ADirFNameToProjMap[files.Item(i)] = prarr;
                }
                else
                {
                    wxArrayString* prarr = &m_ADirFNameToProjMap[files.Item(i)];
                    prarr->Add(it->first);
                }
            }
        }
    }
}

wxArrayString* NativeParserF::GetWSFiles()
{
    return &m_WSFiles;
}

ArrayOfFortranSourceForm* NativeParserF::GetWSFileForms()
{
    return &m_WSFileForms;
}

wxArrayString* NativeParserF::GetWSFileProjFilenames()
{
    return &m_WSFilePFN;
}

wxArrayString* NativeParserF::GetADirFiles()
{
    return &m_ADirFiles;
}

ArrayOfFortranSourceForm* NativeParserF::GetADirFileForms()
{
    return &m_ADirFileForms;
}

void NativeParserF::OnUpdateWorkspaceBrowser(wxCommandEvent& /*event*/)
{
    m_Parser.ConnectToNewTokens();
    UpdateWorkspaceBrowser();
}

void NativeParserF::OnUpdateADirTokens(wxCommandEvent& /*event*/)
{
    m_Parser.ConnectToNewADirTokens();
}

void NativeParserF::OnProjectActivated(cbProject* prj)
{
    if (!m_pWorkspaceBrowser)
        return;

    m_pWorkspaceBrowser->SetActiveProject(prj);
    UpdateWorkspaceBrowser();
}

void NativeParserF::OnEditorActivated(EditorBase* editor)
{
    if (!m_pWorkspaceBrowser)
        return;
    cbEditor* ed = editor && editor->IsBuiltinEditor() ? static_cast<cbEditor*>(editor) : 0;
    if (ed)
    {
        wxString filename = ed->GetFilename();
        if (m_pWorkspaceBrowser->GetBrowserDisplayFilter() == bdfFile && !m_pWorkspaceBrowser->GetActiveFilename().IsSameAs(filename))
        {
            UpdateWorkspaceBrowser(true);
        }
    }
}

void NativeParserF::OnEditorClose(EditorBase* editor)
{
    cbEditor* ed = editor && editor->IsBuiltinEditor() ? static_cast<cbEditor*>(editor) : 0;
    if (ed)
    {
        m_Parser.RemoveBuffer(ed->GetFilename());
    }
}

void NativeParserF::UpdateWorkspaceFilesDependency()
{
    ClearWSDependency();
    ProjectsArray* projects = Manager::Get()->GetProjectManager()->GetProjects();

    for (size_t i = 0; i < projects->GetCount(); ++i)
    {
        cbProject* proj = projects->Item(i);
        if (!proj->IsMakefileCustom())
            UpdateProjectFilesDependency(proj);
    }
}

void NativeParserF::UpdateProjectFilesDependency(cbProject* project)
{
    project->SaveAllFiles();

    ProjectFilesArray pfs;
    for (FilesList::iterator it = project->GetFilesList().begin(); it != project->GetFilesList().end(); ++it)
    {
        ProjectFile* pf = *it;
        if (IsFileFortran(pf->relativeFilename))
        {
            pfs.push_back(pf);
        }
    }

    wxString fn = project->GetFilename();
    WSDependencyMap::iterator pos;
    pos = m_WSDependency.find(fn);
    if (pos == m_WSDependency.end())
    {
        pos = m_WSDependency.insert(std::make_pair(fn,new ProjectDependencies(project))).first;
    }
    if (pfs.size() > 0)
    {
        pos->second->MakeProjectFilesDependencies(pfs, m_Parser);
        pos->second->EnsureUpToDateObjs();

        for (size_t i=0; i<pfs.size(); i++)
        {
            wxString fn2 = pfs[i]->file.GetFullPath();
            unsigned short int wt = pos->second->GetFileWeight(fn2);
            pfs[i]->weight = wt;
        }
        if (pos->second->HasInfiniteDependences())
        {
            wxString msg = _T("Warning. FortranProject plugin:\n");
            msg << _T("     'It seems you have a circular dependency in Fortran files. Check your USE or INCLUDE statements.'");
            Manager::Get()->GetLogManager()->Log(msg);
            cbMessageBox(_("It seems you have a circular dependency in Fortran files. Check your USE or INCLUDE statements."),
                         _("Warning"));
        }
    }
}

void NativeParserF::ClearWSDependency()
{
    WSDependencyMap::iterator pos=m_WSDependency.begin();
    while(pos != m_WSDependency.end())
    {
        ProjectDependencies* pd = pos->second;
        pd->Clear();
        delete pd;
        pos++;
    }
    m_WSDependency.clear();
}

void NativeParserF::RemoveProjectFilesDependency(cbProject* project)
{
    if (m_WSDependency.count(project->GetFilename()))
    {
        ProjectDependencies* pd = m_WSDependency[project->GetFilename()];
        pd->Clear();
        delete pd;
    }
}

ParserF* NativeParserF::GetParser()
{
    return &m_Parser;
}

int NativeParserF::GetTokenKindImageIdx(TokenF* token)
{
    if (m_pWorkspaceBrowser)
        return m_pWorkspaceBrowser->GetTokenKindImageIdx(token);
    return 0;
}

// count commas in lineText (nesting parentheses)
int NativeParserF::CountCommas(const wxString& lineText, int start, bool nesting)
{
    int commas = 0;
    int nest   = 0;
    bool inA  = false;
    bool inDA = false;
    while (true)
    {
        wxChar c = lineText.GetChar(start);
        start++;
        if (c == '\0')
            break;
        else if (nesting && (c == '(' || c == '[') && !inA && !inDA)
            ++nest;
        else if (nesting && (c == ')' || c == ']') && !inA && !inDA)
        {
            --nest;
            if (nest < 0)
                break;
        }
        else if (c == '\'' && !inA && !inDA)
            inA = true;
        else if (c == '\'' && inA)
            inA = false;
        else if (c == '"' && !inA && !inDA)
            inDA = true;
        else if (c == '"' && inDA)
            inDA = false;
        else if (c == ',' && nest == 0 && !inA && !inDA)
            ++commas;
    }
    return commas;
}

wxString NativeParserF::GetLastName(const wxString& line)
{
    wxString name;
    wxString tmp = line;
    tmp.Trim();
    if (tmp.IsEmpty())
        return name;
    int cur = tmp.Len() - 1;

    while (cur >= 0)
    {
        wxChar cch = tmp.GetChar(cur);
        if (!isalnum(cch) && (cch != '_'))
        {
            cur++;
            break;
        }
        else
            cur--;
    }
    if (cur < 0)
        cur = 0;
    name = tmp.Mid(cur);

    return name;
}

void NativeParserF::CollectInformationForCallTip(int& commasAll, int& commasUntilPos, wxString& argNameUnderCursor, wxString& lastName,
                                                 bool& isAfterPercent, int& argsPos, TokensArrayFlat* result)
{
    wxString lineText; // string before '('
    CountCommasInEditor(commasAll, commasUntilPos, lastName, lineText, argsPos);
    if (lastName.IsEmpty())
        return;

    lineText.Trim();
    wxString lineTextMinus = lineText.Mid(0,lineText.Len()-lastName.Len());
    wxString beforLast = GetLastName(lineTextMinus);
    if (beforLast.IsSameAs(_T("subroutine"),false) || beforLast.IsSameAs(_T("function"),false))
    {
        lastName = _T("");
        return; // we don't want calltips during procedure declaration
    }

    isAfterPercent = false;
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if(!ed)
        return;

    GetDummyVarName(ed, argNameUnderCursor);

    m_Parser.ChangeLineIfRequired(ed, lineText);

    lineText.Trim();
    TokensArrayFlatClass tokensTemp;
    TokensArrayFlat* resultTemp = tokensTemp.GetTokens();
    if (!m_Parser.FindMatchTypeComponents(ed, lineText, *resultTemp, false, false, isAfterPercent, true))
        return;
    if (resultTemp->GetCount() > 0)
    {
        TokenFlat* token = resultTemp->Item(0); // we take only first added item
        result->Add( new TokenFlat(token) );
        if (token->m_TokenKind == tkProcedure)
        {
            wxString tokName;
            if (!token->m_PartLast.IsEmpty())
                tokName = token->m_PartLast;
            else
                tokName = token->m_Name;

            TokensArrayFlatClass tokensTmp;
            TokensArrayFlat* resultTmp = tokensTmp.GetTokens();
            int kindMask = tkFunction | tkSubroutine;
            int noInChildren = tkInterface | tkFunction | tkSubroutine;
            bool found = m_Parser.FindMatchTokenInSameModule(token, tokName, *resultTmp, kindMask, noInChildren);
            if (!found)
                m_Parser.FindMatchTokensDeclared(tokName, *resultTmp, kindMask, false, noInChildren);
            if (resultTmp->GetCount() > 0)
                result->Add( new TokenFlat(resultTmp->Item(0)) );
        }
        else if (token->m_TokenKind == tkInterface)
        {
            m_Parser.FindGenericTypeBoudComponents(token, *result);
            for (size_t i=1; i<resultTemp->GetCount(); i++)
            {
                if (resultTemp->Item(i)->m_TokenKind == tkInterface)
                {
                    result->Add( new TokenFlat(resultTemp->Item(i)));
                    m_Parser.FindGenericTypeBoudComponents(resultTemp->Item(i), *result);
                }
            }
        }
    }

}


void NativeParserF::CountCommasInEditor(int& commasAll, int& commasUntilPos, wxString& lastName, wxString& lineText, int &pos)
{
    commasAll = 0;
    commasUntilPos = 0;
    lastName = wxEmptyString;
    int end = 0;
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if(!ed)
        return;

    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return;
    int line = control->GetCurrentLine();
    lineText = control->GetLine(line);
    pos = control->PositionFromLine(line);
    end = control->GetCurrentPos() - pos;

    lineText = lineText.BeforeFirst('!');
    if (int(lineText.Len()) < end)
        return; // we are in comments
    //join lines first, if we are in the continuation line
    FortranSourceForm fsForm;
    IsFileFortran(ed->GetShortName(), fsForm);

    if (fsForm == fsfFree)
    {
        int line2 = line - 1;
        while (line2 > 0)
        {
            wxString lineTextPast = control->GetLine(line2).BeforeFirst('!');
            lineTextPast = lineTextPast.Trim();
            if (!lineTextPast.IsEmpty())
            {
                int idx = lineTextPast.Find('&', true);
                if (idx == wxNOT_FOUND)
                {
                    break;
                }
                else
                {
                    lineText = lineTextPast.Mid(0,idx) + lineText;
                    end += idx;
                    pos = control->PositionFromLine(line2);
                }
            }
            line2--;
        }
    }
    else //fsfFixed
    {
        if (lineText.Len() >= 6)
        {
            wxChar contS = lineText.GetChar(5);
            if (contS != ' ' && contS != '0')
            {
                lineText = lineText.Mid(6);
                pos += 6;
                end -= 6;
                int line2 = line - 1;
                while (line2 > 0)
                {
                    wxString lineTextPast = control->GetLine(line2).BeforeFirst('!');
                    lineTextPast = lineTextPast.Trim();
                    if (!lineTextPast.IsEmpty())
                    {
                        lineText = lineTextPast + lineText;
                        end += lineTextPast.Len();
                        pos = control->PositionFromLine(line2);
                        if (lineTextPast.Len() >= 6)
                        {
                            wxChar contS2 = lineTextPast.GetChar(5);
                            if (contS2 == ' ' || contS2 == '0')
                                break;
                            else
                            {
                                lineText = lineText.Mid(6);
                                pos += 6;
                                end -= 6;
                            }
                        }
                        else
                            break;
                    }
                    line2--;
                }
            }
        }
        else
        {
            return;
        }
    }

    wxString lineTextUntilPos = lineText.Mid(0,end);
    int nest = 0;

    while (end > 0)
    {
        --end;
        if (lineText.GetChar(end) == ')')
            --nest;
        else if (lineText.GetChar(end) == '(')
        {
            ++nest;
            if (nest > 0)
            {
                // count commas (nesting parentheses again) to see how far we 're in arguments
                commasAll = CountCommas(lineText, end + 1);
                commasUntilPos = CountCommas(lineTextUntilPos, end + 1);
                break;
            }
        }
    }
    if (!end)
        return;

    lineText.Truncate(end);
    pos += lineText.Len();
    lastName = GetLastName(lineText);
}

void NativeParserF::GetDummyVarName(cbEditor* ed, wxString& lastDummyVar)
{
    // Get dummy arg name like 'vnam' in 'call sub1(a, b, vnam=...'
    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return;
    int clin = control->GetCurrentLine();
    int lpos = control->PositionFromLine(clin);
    int cpos = control->GetCurrentPos();
    cpos = control->WordEndPosition(cpos, true);
    while (cpos < control->GetLength())
    {
        wxChar c = control->GetCharAt(cpos);
        if (c == ' ' || c == '=')
            cpos++;
        else
            break;
    }
    wxString line = control->GetTextRange(lpos, cpos);

    if (line.Find('!') != wxNOT_FOUND)
        return;

    int asig = line.Find('=', true);
    if (asig == wxNOT_FOUND)
        return;

    int endIdx = 0;
    int nest = 0;
    bool inA  = false;
    bool inDA = false;
    for (int i=line.Len()-1; i>=0; --i)
    {
        wxChar c = line.GetChar(i);
        if (c == '\'' && !inA && !inDA)
            inA = true;
        else if (c == '\'' && inA)
            inA = false;
        else if (c == '"' && !inA && !inDA)
            inDA = true;
        else if (c == '"' && inDA)
            inDA = false;
        else if ((c == ')' || c == ']') && !inA && !inDA)
            nest++;
        else if ((c == '(' || c == '[') && nest == 0 && !inA && !inDA)
            break;
        else if ((c == '(' || c == '[') && !inA && !inDA)
            nest--;
        else if (c == ',' && nest == 0 && !inA && !inDA)
            break;
        else if (c == '=' && nest == 0 && !inA && !inDA)
        {
            endIdx = i;
            break;
        }
    }

    if (endIdx == 0)
        return;

    lastDummyVar = GetLastName(line.Mid(0, endIdx));
}

void NativeParserF::GetCallTips(const wxString& name, bool onlyUseAssoc, bool onlyPublicNames, wxArrayString& callTips, TokensArrayFlat* result)
{
    int tokKind;
    if (Manager::Get()->GetConfigManager(_T("fortran_project"))->ReadBool(_T("/call_tip_arrays"), true))
        tokKind = tkFunction | tkSubroutine | tkInterface | tkType | tkVariable;
    else
        tokKind = tkFunction | tkSubroutine | tkInterface | tkType;

    int resCountOld = result->GetCount();
    if (onlyUseAssoc)
    {
        cbEditor* ed =  Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
        if (!ed)
            return;
        m_Parser.FindUseAssociatedTokens(onlyPublicNames, ed, name, false, *result, tokKind, false);
        int noChildrenOf = tkInterface | tkModule | tkSubmodule | tkFunction | tkSubroutine | tkProgram;
        m_Parser.FindMatchTokensDeclared(name, *result, tokKind, false, noChildrenOf, false, true); // take global procedures only

        if (tokKind & tkVariable)
        {
            TokensArrayFlatClass tokensTmp;
            TokensArrayFlat* resultTmp = tokensTmp.GetTokens();
            m_Parser.FindMatchDeclarationsInCurrentScope(name, ed, *resultTmp, false);
            for (size_t i=0; i<resultTmp->GetCount(); i++)
            {
                if (resultTmp->Item(i)->m_TokenKind == tkVariable)
                    result->Add(new TokenFlat(resultTmp->Item(i)));
            }
        }
    }
    else
    {
        int noChildrenOf = tkInterface | tkFunction | tkSubroutine | tkProgram;
        m_Parser.FindMatchTokensDeclared(name, *result, tokKind, false, noChildrenOf, onlyPublicNames);
    }

    int tokkindFS = tkFunction | tkSubroutine;
    int resCount = result->GetCount();
    for (int i=resCountOld; i<resCount; ++i)
    {
        if (result->Item(i)->m_ParentTokenKind == tkSubmodule && (result->Item(i)->m_TokenKind & tokkindFS))
        {
            for (int j=i+1; j<resCount; ++j)
            {
                if (result->Item(j)->m_ParentTokenKind == tkInterfaceExplicit &&
                    result->Item(j)->m_TokenKind == result->Item(i)->m_TokenKind &&
                    result->Item(j)->m_Name.IsSameAs(result->Item(i)->m_Name) )
                {
                    result->RemoveAt(i);
                    resCount--;
                    i--;
                    break;
                }
            }
        }
    }

    resCount = result->GetCount();
    for (int i=resCountOld; i<resCount; ++i)
    {
        if (result->Item(i)->m_TokenKind == tkInterface)
        {
            m_Parser.FindChildrenOfInterface(result->Item(i), *result);
            result->RemoveAt(i);
            resCount--;
            i--;
        }
    }

    resCount = result->GetCount();
    for (int i=resCountOld; i<resCount; ++i)
    {
        if (result->Item(i)->m_TokenKind == tkVariable)
        {
            wxString callTipArr;
            GetCallTipsForVariable(result->Item(i), callTipArr);
            if (!callTipArr.IsEmpty())
                callTips.Add(callTipArr);
        }
        else if (result->Item(i)->m_TokenKind == tkType)
        {
            if (resCountOld+1 != int(result->GetCount()))
            {
                // remove 'type' if it is not unique
                result->RemoveAt(i);
                resCount--;
                i--;
            }
            else
            {
                // Default structure-constructor
                wxString callTipType;
                GetCallTipsForType(result->Item(i), callTipType);
                if (!callTipType.IsEmpty())
                    callTips.Add(callTipType);
                else
                {
                    result->RemoveAt(i);
                    resCount--;
                    i--;
                }
            }
        }
        else
            callTips.Add(result->Item(i)->m_Args);
    }
}

void NativeParserF::GetCallTipsForGenericTypeBoundProc(TokensArrayFlat* result, wxArrayString& callTips, wxArrayInt& idxFuncSub)
{
    if (result->GetCount() >= 3 && result->Item(0)->m_TokenKind == tkInterface)
    {
        int tokKind = tkFunction | tkSubroutine;
        for (size_t i=1; i<result->GetCount()-1; i+=2)
        {
            if (result->Item(i)->m_TokenKind == tkInterface)
                i++;
            if (i+1 >= result->GetCount())
                return;
            if (result->Item(i)->m_TokenKind != tkProcedure || !(result->Item(i+1)->m_TokenKind & tokKind))
                return;

            TokensArrayFlatClass tokensTmpCl;
            TokensArrayFlat* tokensTmp = tokensTmpCl.GetTokens();
            tokensTmp->Add(new TokenFlat(result->Item(i)));
            tokensTmp->Add(new TokenFlat(result->Item(i+1)));
            GetCallTipsForTypeBoundProc(tokensTmp, callTips);
            idxFuncSub.Add(i+1);
        }
    }
}

void NativeParserF::GetCallTipsForTypeBoundProc(TokensArrayFlat* result, wxArrayString& callTips)
{
    if (result->GetCount() != 2)
        return;
    if (!(result->Item(0)->m_TokenKind == tkProcedure))
        return;

    int tokKind = tkFunction | tkSubroutine;
    if (!(result->Item(1)->m_TokenKind & tokKind))
        return;

    TokenFlat tbProcTok(result->Item(0));
    m_Parser.ChangeArgumentsTypeBoundProc(tbProcTok, result->Item(1));
    callTips.Add(tbProcTok.m_Args);
}

void NativeParserF::GetCallTipsForVariable(TokenFlat* token, wxString& callTip)
{
    callTip = wxEmptyString;
    if (!(token->m_TokenKind == tkVariable))
        return;

    int dstart = token->m_TypeDefinition.Lower().Find(_T("dimension"));
    if (dstart != wxNOT_FOUND)
    {
        wxString dim = token->m_TypeDefinition.Mid(dstart+9);
        if (dim.size() > 0 && dim[0] == '(')
        {
            int last = dim.Find(')');
            if (last != wxNOT_FOUND)
                callTip = dim.Mid(0,last+1);
        }
    }
    else if (token->m_Args.StartsWith(_T("(")))
    {
        int last = token->m_Args.Find(')');
        if (last != wxNOT_FOUND)
            callTip = token->m_Args.Mid(0,last+1);
    }
}

void NativeParserF::GetCallTipsForType(TokenFlat* token, wxString& callTip)
{
    callTip = wxEmptyString;
    if (!(token->m_TokenKind == tkType))
        return;

    if (token->m_IsAbstract || !token->m_ExtendsType.IsEmpty())  // no default constructor for Abstract or Extended type
        return;
    TokensArrayFlatClass tokensTmp;
    TokensArrayFlat* resultTmp = tokensTmp.GetTokens();
    m_Parser.GetTypeComponentsInFile(token->m_Filename, token->m_LineStart, token->m_Name, resultTmp);

    wxString names;
    for (size_t i=0; i<resultTmp->GetCount(); i++)
    {
        if (resultTmp->Item(i)->m_TokenKind != tkVariable)
            continue;

        names << resultTmp->Item(i)->m_DisplayName << _T(", ");
    }

    if (!names.IsEmpty())
    {
        callTip << _T("(") << names.Mid(0,names.Length()-2) << _T(")");
    }
}

// set start and end to the calltip highlight region, based on commasWas (calculated in GetCallTips())
void NativeParserF::GetCallTipHighlight(const wxString& calltip, int commasWas, int& start, int& end)
{
    m_Parser.GetCallTipHighlight(calltip, commasWas, start, end);
}

void NativeParserF::MarkCurrentSymbol(bool selectCurrentSymbol)
{
    if (!m_pWorkspaceBrowser || Manager::IsAppShuttingDown())
        return;

    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (!ed)
        return;
    wxString activeFilename = ed->GetFilename();
    if (activeFilename.IsEmpty())
        return;
    if (!IsFileFortran(activeFilename))
        return;
    cbStyledTextCtrl* control = ed->GetControl();
    int currentLine = control->GetCurrentLine() + 1;

    wxCriticalSectionLocker locker(s_CritSect);
    wxString fname = UnixFilename(activeFilename);
    m_pWorkspaceBrowser->MarkSymbol(fname, currentLine);
    if (selectCurrentSymbol)
        m_pWorkspaceBrowser->SelectSymbol(fname, currentLine);
}

void NativeParserF::RereadOptions()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));
    // disabled?
    if (cfg->ReadBool(_("/use_symbols_browser"), true))
    {
        if (!m_pWorkspaceBrowser)
        {
            CreateWorkspaceBrowser();
        }
        // change class-browser docking settings
        else if (m_WorkspaceBrowserIsFloating != cfg->ReadBool(_T("/as_floating_window"), false))
        {
            RemoveWorkspaceBrowser();
            CreateWorkspaceBrowser();
        }
        else
        {
            m_pWorkspaceBrowser->RereadOptions();
        }
        UpdateWorkspaceBrowser();
    }
    else if (m_pWorkspaceBrowser)
    {
        RemoveWorkspaceBrowser();
    }
    else
    {
        //m_pWorkspaceBrowser->RereadOptions();
    }

    m_Parser.RereadOptions();
}

JumpTracker* NativeParserF::GetJumpTracker()
{
    return &m_JumpTracker;
}

FortranProject* NativeParserF::GetFortranProject()
{
    return m_pFortranProject;
}

void NativeParserF::GenMakefile()
{
    cbProject* project = Manager::Get()->GetProjectManager()->GetActiveProject();
    if (!project)
    {
        Manager::Get()->GetLogManager()->Log(_T("No active project was found. Makefile was not generated."));
        cbMessageBox(_("No active project was found.\nMakefile was not generated."), _("Error"), wxICON_ERROR);
        return;
    }

    UpdateProjectFilesDependency(project);

    wxString fn = project->GetFilename();
    WSDependencyMap::iterator pos;
    pos = m_WSDependency.find(fn);
    if (pos == m_WSDependency.end())
        return;

    if (pos->second->GetSizeFiles() > 0)
        MakefileGen::GenerateMakefile(project, pos->second, this);
    else
    {
        Manager::Get()->GetLogManager()->Log(_T("Active project doesn't have Fortran files."));
        cbMessageBox(_("Active project doesn't have Fortran files.\nMakefile was not generated."), _("Information"), wxICON_INFORMATION);
    }
}

void NativeParserF::GetCurrentBuffer(wxString& buffer, wxString& filename, wxString& projFilename)
{
    wxCriticalSectionLocker locker(s_CurrentBufferCritSect);
    buffer   = m_CurrentEditorBuffer;
    filename = m_CurrentEditorFilename;
    projFilename = m_CurrentEditorProjectFN;
}

void NativeParserF::ReparseCurrentEditor()
{
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if(!ed)
        return;
    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return;

    {
        wxCriticalSectionLocker locker(s_CurrentBufferCritSect);
        m_CurrentEditorBuffer = control->GetText();
        m_CurrentEditorFilename = ed->GetFilename();

        ProjectFile* pf = ed->GetProjectFile();
        if (pf)
        {
            cbProject* pr = pf->GetParentProject();
            if (pr)
            {
                m_CurrentEditorProjectFN = pr->GetFilename();
            }
        }
        else
            m_CurrentEditorProjectFN = _T("");
    }

    if (BufferParserThread::s_BPTInstances <= 1)
    {
        BufferParserThread* thread = new BufferParserThread(this, idBPThreadEvent);
        m_ThreadPool.AddTask(thread, true);
    }
}

void NativeParserF::OnUpdateCurrentFileTokens(wxCommandEvent& /*event*/)
{
    m_Parser.ConnectToNewCurrentTokens();
}

wxArrayString NativeParserF::GetProjectSearchDirs(cbProject* project)
{
    wxArrayString dirs;
    if (!project)
        return dirs;
    wxString pfn = project->GetFilename();
    if (m_ASearchDirs.count(pfn) == 0)
        return dirs;

    return m_ASearchDirs[pfn];
}

void NativeParserF::SetProjectSearchDirs(cbProject* project, wxArrayString& searchDirs)
{
    if (!project)
        return;

    m_ASearchDirs[project->GetFilename()] = searchDirs;
}

bool NativeParserF::HasFortranFiles(cbProject* project)
{
    if (!project)
        return false;

    wxString pfn = project->GetFilename();
    for (size_t i=0; i<m_WSFilePFN.size(); ++i)
    {
        if (m_WSFilePFN[i] == pfn)
            return true;
    }
    return false;
}

void NativeParserF::DelProjectSearchDirs(cbProject* project)
{
    if (!project)
        return;

    m_ASearchDirs.erase(project->GetFilename());
}

void NativeParserF::ForceReparseProjectSearchDirs()
{
    if (Manager::IsAppShuttingDown())
        return;

    m_ASearchDirsReparseTimer.Start(1500, wxTIMER_ONE_SHOT);
}

void NativeParserF::OnASearchDirsReparseTimer(wxTimerEvent& /*event*/)
{
    if (Manager::IsAppShuttingDown())
        return;

    if (s_AdditionalDirParserMutex.TryLock() == wxMUTEX_NO_ERROR)
    {
        MakeADirFileList();
        s_AdditionalDirParserMutex.Unlock();

        ADirParserThread* thread = new ADirParserThread(this, idADirPThreadEvent);
        m_ThreadPool.AddTask(thread, true);
    }
}
