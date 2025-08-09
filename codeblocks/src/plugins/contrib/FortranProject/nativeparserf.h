/*
 * This file is licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef NATIVEPARSERF_H
#define NATIVEPARSERF_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/string.h>
    #include <wx/event.h>
#endif
#include <map>
#include <set>

#include <cbthreadpool.h>

#include "jumptracker.h"
#include "parserf.h"
#include "projectdependencies.h"
#include "workspacebrowserf.h"
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
        void AddFileToParser(const wxString& projectFilename, const wxString& filename);
        void RemoveFileFromParser(const wxString& filename);
        bool ReparseFile(const wxString& projectFilename, const wxString& filename);
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
        void UpdateWorkspaceBrowser(bool selectCurrentSymbol=false);
        int GetTokenKindImageIdx(TokenF* token);
        void GetCallTips(const wxString& name, bool onlyUseAssoc, bool onlyPublicNames, wxArrayString& callTips, TokensArrayFlat* result);
        void GetCallTipsForGenericTypeBoundProc(TokensArrayFlat* result, wxArrayString& callTips, wxArrayInt& idxFuncSub);
        void GetCallTipsForTypeBoundProc(TokensArrayFlat* result, wxArrayString& callTips);
        void GetCallTipsForVariable(TokenFlat* token, wxString& callTip);
        void GetCallTipsForType(TokenFlat* token, wxString& callTip);
        int CountCommas(const wxString& lineText, int start, bool nesting=true);
        void CollectInformationForCallTip(int& commasAll, int& commasUntilPos, wxString& argNameUnderCursor, wxString& lastName, bool& isAfterPercent, int& argsPos, TokensArrayFlat* result);
        void CountCommasInEditor(int& commasAll, int& commasUntilPos, wxString& lastName, wxString& lineText, int& pos);
        void GetCallTipHighlight(const wxString& calltip, int commasWas, int& start, int& end);
        void MarkCurrentSymbol(bool selectCurrentSymbol);
        void RereadOptions();
        JumpTracker* GetJumpTracker();
        FortranProject* GetFortranProject();
        void GenMakefile();
        wxArrayString* GetWSFiles();
        ArrayOfFortranSourceForm* GetWSFileForms();
        wxArrayString* GetWSFileProjFilenames();
        wxArrayString* GetADirFiles();
        ArrayOfFortranSourceForm* GetADirFileForms();
        std::map<wxString,wxString>* GetAdditionalIncludeFiles();
        wxString FindIncludeFile(const wxString& checkDir,  const wxString& filename);
        void GetCurrentBuffer(wxString& buffer, wxString& filename, wxString& projFilename);
        void SetInterpretCPP(bool interpretCPP, bool cppShadow, const wxColour& cppShadowColour, int cppShadowOpacity);
        bool DoInterpretCPP();
        void ReparseCurrentEditor();
        wxArrayString GetProjectSearchDirs(cbProject* project);
        void SetProjectSearchDirs(cbProject* project, wxArrayString& searchDirs);
        void SetProjectIncludeDirs(cbProject* project, wxArrayString& includeDirs);
        void MakeAIncludeFileList();
        wxArrayString GetProjectIncludeDirs(cbProject* project);
        void DelProjectIncludeDirs(cbProject* project);
        const std::vector<wxString>* GetProjectCPPMacros(const wxString& projFilename);
        std::vector<wxString>* GetProjectCPPMacrosCopy(const wxString& projFilename);
        void SetProjectCPPMacros(cbProject* project, const wxString& strMacros);
        bool HasFortranFiles(cbProject* project);
        void DelProjectSearchDirs(cbProject* project);
        void ForceReparseProjectSearchDirs();
        void OnASearchDirsReparseTimer(wxTimerEvent& event);

    protected:
    private:
        friend class FortranProject;

        void OnEditorActivated(EditorBase* editor);
        void OnEditorClose(EditorBase* editor);
        void MarkDisabledLines(cbEditor* editor);
        void OnProjectActivated(cbProject* project);
        void UpdateWorkspaceFilesDependency();
        void UpdateWSFilesDependency();
        void ClearWSDependency();
        void RemoveProjectFilesDependency(cbProject* project);

        bool IsFileFortran(const wxString& filename, FortranSourceForm& fsForm);
        //void BreakUpInLines(wxString& str, const wxString& original_str, int chars_per_line);
        wxString GetLastName(const wxString& line);

        void MakeWSFileList();
        void MakeADirFileList();

        void OnWSParserThreadFinished(wxCommandEvent& event);
        void OnUpdateADirTokens(wxCommandEvent& event);
        void OnUpdateCurrentFileTokens(wxCommandEvent& event);

        void GetDummyVarName(cbEditor* ed, wxString& lastDummyVar);

        ParserF m_Parser;
        WorkspaceBrowserF* m_pWorkspaceBrowser;
        bool m_WorkspaceBrowserIsFloating;
        FortranProject* m_pFortranProject;
        wxTimer m_WorkspaceReparseTimer;

        WSDependencyMap m_WSDependency;

        JumpTracker m_JumpTracker;

        cbThreadPool m_ThreadPool;

        wxArrayString m_WSFiles;                   ///<  list of workspace filenames
        ArrayOfFortranSourceForm m_WSFileForms;    ///<  sorce form of each WS file
        wxArrayString m_WSFilePFN;                 ///<  to which project depands each WS file

        wxArrayString m_ADirFiles;                 ///<  list of filenames from additional directories
        ArrayOfFortranSourceForm m_ADirFileForms;  ///<  sorce form of each additional file
        wxTimer m_ASearchDirsReparseTimer;

        wxString m_CurrentEditorBuffer;
        wxString m_CurrentEditorFilename;
        wxString m_CurrentEditorProjectFN;

        std::map<wxString,wxArrayString> m_ASearchDirs;
        std::map<wxString,wxArrayString> m_ADirFNameToProjMap;

        std::map<wxString,wxArrayString> m_AIncludeDirs; ///< dirs with additional include files for each project
        std::map<wxString,wxString> m_AIncludeFiles;     ///< additional include files
        bool m_InterpretCPP;
        bool m_CppShadow;
        wxColour m_CppShadowColour;
        int m_CppShadowOpacity;
        std::map<wxString,std::vector<wxString>> m_CPPMacros;     ///< project_name->macros

        DECLARE_EVENT_TABLE();
};

#endif // NATIVEPARSERF_H

