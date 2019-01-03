/*
 * This file is licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef NATIVEPARSERF_H
#define NATIVEPARSERF_H

#include <map>
#include <set>
#include <wx/string.h>
#include <wx/event.h>
#include "parserf.h"
#include "workspacebrowserf.h"
#include "projectdependencies.h"
#include "jumptracker.h"

#include <cbthreadpool.h>
#include "workspaceparserthread.h"

typedef std::map<wxString,ProjectDependencies*>  WSDependencyMap;

// forward decls
class EditorBase;
class cbProject;
class TokenF;
class FortranProject;

class NativeParserF : public wxEvtHandler
{
    public:
        NativeParserF(FortranProject* forproj);
        ~NativeParserF();

        void AddParser(cbProject* project);
        void ClearParser();
        void RemoveFromParser(cbProject* project);
        void AddFileToParser(const wxString& filename);
        void RemoveFileFromParser(const wxString& filename);
        void ReparseFile(const wxString& filename);
        void ReparseProject(cbProject* project);
        void ParseProject(cbProject* project);
        void ForceReparseWorkspace();
        void OnReparseWorkspaceTimer(wxTimerEvent& event);
        void UpdateProjectFilesDependency(cbProject* project);
        ParserF* GetParser();
        bool IsFileFortran(const wxString& filename);

        void CreateWorkspaceBrowser();
        WorkspaceBrowserF* GetWorkspaceBrowser();
        void RemoveWorkspaceBrowser();
        void UpdateWorkspaceBrowser();
        wxImageList* GetImageList();
        int GetTokenKindImageIdx(TokenF* token);
        void GetCallTips(const wxString& name, bool onlyUseAssoc, bool onlyPublicNames, wxArrayString& callTips, TokensArrayFlat* result);
        void GetCallTipsForGenericTypeBoundProc(TokensArrayFlat* result, wxArrayString& callTips, wxArrayInt& idxFuncSub);
        void GetCallTipsForTypeBoundProc(TokensArrayFlat* result, wxArrayString& callTips);
        void GetCallTipsForVariable(TokenFlat* token, wxString& callTip);
        void GetCallTipsForType(TokenFlat* token, wxString& callTip);
        int CountCommas(const wxString& lineText, int start, bool nesting=true);
        void CollectInformationForCallTip(int& commasAll, int& commasUntilPos, wxString& lastName, bool& isempty, bool& isAfterPercent, int& argsPos, TokensArrayFlat* result);
        void CountCommasInEditor(int& commasAll, int& commasUntilPos, wxString& lastName, bool& isempty, wxString& lineText, int& pos);
        void GetCallTipHighlight(const wxString& calltip, int commasWas, int& start, int& end);
        void MarkCurrentSymbol();
        void RereadOptions();
        JumpTracker* GetJumpTracker();
        FortranProject* GetFortranProject();
        void GenMakefile();
        wxArrayString* GetWSFiles();
        ArrayOfFortranSourceForm* GetWSFileForms();
        void GetCurrentBuffer(wxString& buffer, wxString& filename);
        void ReparseCurrentEditor();

    protected:
    private:
        friend class FortranProject;

        void OnEditorActivated(EditorBase* editor);
        void OnEditorClose(EditorBase* editor);
        void OnProjectActivated(cbProject* project);
        void UpdateWorkspaceFilesDependency();
        void ClearWSDependency();
        void RemoveProjectFilesDependency(cbProject* project);

        bool IsFileFortran(const wxString& filename, FortranSourceForm& fsForm);
        //void BreakUpInLines(wxString& str, const wxString& original_str, int chars_per_line);
        wxString GetLastName(const wxString& line);

        void MakeWSFileList();

        void OnUpdateWorkspaceBrowser(wxCommandEvent& event);
        void OnUpdateCurrentFileTokens(wxCommandEvent& event);

        ParserF m_Parser;
        WorkspaceBrowserF* m_pWorkspaceBrowser;
        bool m_WorkspaceBrowserIsFloating;
        FortranProject* m_pFortranProject;
        wxTimer m_WorkspaceReparseTimer;

        WSDependencyMap m_WSDependency;

        JumpTracker m_JumpTracker;

        cbThreadPool m_ThreadPool;

        wxArrayString m_WSFiles;
        ArrayOfFortranSourceForm m_WSFileForms;

        wxString m_CurrentEditorBuffer;
        wxString m_CurrentEditorFilename;

        DECLARE_EVENT_TABLE();
};

#endif // NATIVEPARSERF_H

