/***************************************************************
 * Name:      fortranproject
 * Purpose:   Code::Blocks plugin
 * Author:    Darius Markauskas, darmar.lt@gmail.com (code based on Code-completion plugin)
 * Created:   2009-07-11
 * Copyright:
 * License:   GPL
 **************************************************************/

#ifndef FORTRANPROJECT_H_INCLUDED
#define FORTRANPROJECT_H_INCLUDED

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/timer.h>

    #include <cbplugin.h> // for "class cbToolPlugin"
    #include <loggers.h>
#endif
#include <list>
#include <map>

#include "nativeparserf.h"
#include "keywordsparserf.h"
#include "finfowindow.h"
#include "lineaddress.h"
#include "autoinsert.h"
#include "constrhighlighter.h"
#include "calltree.h"

enum DocsShowOptions
{
    dsoAlways,
    dsoOnly,
    dsoNot
};


class FortranProject : public cbCodeCompletionPlugin
{
    public:

        FortranProject();
        ~FortranProject();

        virtual void OnAttach();
        virtual void OnRelease(bool appShutDown);

        void OnUpdateUI(wxUpdateUIEvent& event);
        void OnUpdateUICallTree(wxUpdateUIEvent& event);
        void BuildMenu(wxMenuBar* menuBar);
        virtual void BuildModuleMenu(const ModuleType type, wxMenu* menu, const FileTreeData* data = 0);
        void OnViewWorkspaceBrowser(wxCommandEvent& event);
        virtual bool BuildToolBar(wxToolBar* toolBar);
        void ShowCallTipEvt(CodeBlocksEvent& event);
        void MakeCompleteCode();


        void EditorEventHook(cbEditor* editor, wxScintillaEvent& event);

        virtual int GetConfigurationGroup() const { return cgEditor; }
        void ShowInfoLog(TokensArrayFlat* result, bool isAfterPercent);
        cbConfigurationPanel* GetConfigurationPanel(wxWindow* parent);
        cbConfigurationPanel* GetProjectConfigurationPanel(wxWindow* parent, cbProject* project);
        void OnProjectLoadingHook(cbProject* prj, TiXmlElement* elem, bool loading);
        void RereadOptions();
        void CheckEnableToolbar();
        bool GotoToken(TokenFlat* pToken, cbEditor* cured);
        void ShowCallTree(bool showCallTree);

        // override
        virtual CCProviderStatus GetProviderStatusFor(cbEditor* ed);
        virtual std::vector<CCToken> GetAutocompList(bool isAuto, cbEditor* ed, int& tknStart, int& tknEnd);
        virtual void DoAutocomplete(const CCToken& token, cbEditor* ed);
        virtual wxString GetDocumentation(const CCToken& token);
        virtual std::vector<CCCallTip> GetCallTips(int pos, int style, cbEditor* ed, int& argsPos);
        virtual std::vector<CCToken> GetTokenAt(int pos, cbEditor* ed, bool& allowCallTip);
        virtual wxString OnDocumentationLink(wxHtmlLinkEvent& event, bool& dismissPopup);

    private:

        void OnAppDoneStartup(CodeBlocksEvent& event);
        void OnWorkspaceChanged(CodeBlocksEvent& event);
        void OnProjectActivated(CodeBlocksEvent& event);
        void OnProjectClosed(CodeBlocksEvent& event);
        void OnProjectSaved(CodeBlocksEvent& event);
        void OnProjectFileAdded(CodeBlocksEvent& event);
        void OnProjectEndAddFiles(CodeBlocksEvent& event);
        void OnProjectFileRemoved(CodeBlocksEvent& event);
        void OnProjectEndRemoveFiles(CodeBlocksEvent& event);
        void OnCompilerStarted(CodeBlocksEvent& event);
        void OnCleanProjectStarted(CodeBlocksEvent& event);
        void OnCleanWorkspaceStarted(CodeBlocksEvent& event);
        void OnGotoDeclaration(wxCommandEvent& event);
        void OnEditorSave(CodeBlocksEvent& event);
        void OnEditorActivated(CodeBlocksEvent& event);
        void OnEditorDeactivated(CodeBlocksEvent& event);
        void OnEditorClose(CodeBlocksEvent& event);
        void OnReparseEditorTimer(wxTimerEvent& event);
        void CodeCompletePreprocessor(int tknStart, int tknEnd, cbEditor* ed, std::vector<CCToken>& tokens);
        void DoCodeComplete(int caretPos, cbEditor* ed, std::vector<CCToken>& tokens);
        void CodeComplete(const int pos, cbEditor* ed, std::vector<CCToken>& tokens);
        void OnValueTooltip(CodeBlocksEvent& event);
        void WriteToLog(const wxString& text);
        void CreateLogWindow();
        void RemoveLogWindow(bool appShutDown);
        void OnJumpBack(wxCommandEvent& event);
        void OnJumpHome(wxCommandEvent& event);
        void OnJumpForward(wxCommandEvent& event);
        void JumpToLine(const LineAddress& adr);
        wxString GetIncludeFilename(cbStyledTextCtrl* control);
        void OnMenuEditPaste(wxCommandEvent& event);

        void OnDebuggerStarted(CodeBlocksEvent& event);
        void OnDebuggerFinished(CodeBlocksEvent& event);
        bool m_IsDebugging;

        void OnGenerateMakefile(wxCommandEvent& event);
        void OnChangeCase(wxCommandEvent& event);
        void OnTab2Space(wxCommandEvent& event);
        void OnBindTo(wxCommandEvent& event);
        void OnFormatIndent(wxCommandEvent& event);
        void OnShowCallTreeView(wxCommandEvent& event);
        void OnShowCallTree(wxCommandEvent& event);
        void OnShowCalledByTree(wxCommandEvent& event);
        void LoadFortranKeywordImages();
        wxBitmap GetFortranKeywordImage(int height);
        void PrepareImageList(cbStyledTextCtrl* control);

        bool m_InitDone;

        NativeParserF* m_pNativeParser;
        CallTree* m_pCallTree;

        int                                m_ProjectLoadingHookID;
        int                                m_EditorHookId;
        int                                m_LastPosForCodeCompletion;
        wxTimer                            m_TimerCodeCompletion;
        cbEditor*                          m_pCodeCompletionLastEditor;
        wxMenu*                            m_ViewMenu;
        wxMenu*                            m_FortranToolsMenu;

        wxToolBar*                         m_pToolbar;

        bool                               m_ShowedCallTip;

        bool                               m_WasCallTipActive;

        bool                               m_IsAutoPopup;
        int                                m_ActiveCalltipsNest;
        int                                m_ActiveCalltipsPosition;
        int                                m_CurrentLine;
        bool                               m_LexerKeywordsToInclude[4];
        bool                               m_ReplaceAlwaysCC;
        bool                               m_UseSmartCC;
        size_t                             m_MaxMatch;

        bool                               m_LogUseWindow;
        bool                               m_LogComAbove;
        bool                               m_LogComBelow;
        bool                               m_LogDeclar;
        bool                               m_LogComVariab;
        bool                               m_LogOnlyUseAssoc;
        bool                               m_LogOnlyPublicNames;
        bool                               m_LogShowTypeVariables;

        DocsShowOptions                    m_DocsShowOption;

        bool                               m_AutoCorrectIndentEnabled;
        bool                               m_AutoInsertEnabled;
        AutoInsert                         m_AutoInsert;

        ConstrHighlighter                  m_ConstrHighlighter;

        FInfoWindow*                       m_pFortranLog;

        KeywordsParserF*                   m_pKeywordsParser;
        wxString                           m_LastCallTipName;
        bool                               m_WasCallTipInfoLog;

        std::map<wxString,int>             m_IdxCallTipPage;
        wxTimer                            m_TimerReparseEditor;

        TokensArrayFlatClass               m_TokensCCList;
        std::map<int,wxBitmap>             m_FKImages;
        FPImageList*                       m_pImageList;

        DECLARE_EVENT_TABLE()
};

#endif // FORTRANPROJECT_H

