/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 */

#include "fpoptionsprjdlg.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/button.h>
    #include <wx/intl.h>
    #include <wx/listbox.h>
    #include <wx/xrc/xmlres.h>

    #include <cbproject.h>
    #include <cbstyledtextctrl.h>
    #include <globals.h>
    #include <logmanager.h>
    #include <manager.h>
#endif

#include <wx/tokenzr.h>

#include <editpathdlg.h>

BEGIN_EVENT_TABLE(FPOptionsProjectDlg, wxPanel)
    EVT_UPDATE_UI(-1, FPOptionsProjectDlg::OnUpdateUI)
    EVT_BUTTON(XRCID("btnAddDir"), FPOptionsProjectDlg::OnAddDir)
    EVT_BUTTON(XRCID("btnAddFile"), FPOptionsProjectDlg::OnAddFile)
    EVT_BUTTON(XRCID("btnEdit"), FPOptionsProjectDlg::OnEdit)
    EVT_BUTTON(XRCID("btnDelete"), FPOptionsProjectDlg::OnDelete)
    EVT_BUTTON(XRCID("btnAddInclude"), FPOptionsProjectDlg::OnAddInclude)
    EVT_BUTTON(XRCID("btnEditInclude"), FPOptionsProjectDlg::OnEditInclude)
    EVT_BUTTON(XRCID("btnDeleteInclude"), FPOptionsProjectDlg::OnDeleteInclude)
END_EVENT_TABLE()

FPOptionsProjectDlg::FPOptionsProjectDlg(wxWindow* parent, cbProject* project, NativeParserF* np) :
    m_pProject(project),
    m_pNativeParser(np)
{
    wxXmlResource::Get()->LoadPanel(this, parent, "pnlProjectFPOptions");
    m_OldPaths = m_pNativeParser->GetProjectSearchDirs(m_pProject);
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);
    control->Clear();
    for (size_t i = 0; i < m_OldPaths.GetCount(); ++i)
        control->Append(m_OldPaths[i]);

    m_OldPathsInclude = m_pNativeParser->GetProjectIncludeDirs(m_pProject);
    control = XRCCTRL(*this, "lstPathsInclude", wxListBox);
    control->Clear();
    for (size_t i = 0; i < m_OldPathsInclude.GetCount(); ++i)
        control->Append(m_OldPathsInclude[i]);

    m_OldCPPMacros = wxEmptyString;
    if (m_pProject)
    {
        const std::vector<wxString>* strMacrosVec = m_pNativeParser->GetProjectCPPMacros(m_pProject->GetFilename());
        if (strMacrosVec)
        {
            for (const auto& m : *strMacrosVec)
            {
                m_OldCPPMacros << (m + "; ");
            }
        }
    }

    wxTextCtrl* txtMacros = XRCCTRL(*this, "txtCPPMacros", wxTextCtrl);
    txtMacros->SetValue(m_OldCPPMacros);
}

FPOptionsProjectDlg::~FPOptionsProjectDlg()
{
}

void FPOptionsProjectDlg::OnAddDir(cb_unused wxCommandEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);

    EditPathDlg dlg(this,
                    m_pProject ? m_pProject->GetBasePath() : "",
                    m_pProject ? m_pProject->GetBasePath() : "",
                    _("Add directory"));

    PlaceWindow(&dlg);
    if (dlg.ShowModal() == wxID_OK)
    {
        wxString path = dlg.GetPath();
        control->Append(path);
    }
}

void FPOptionsProjectDlg::OnAddFile(cb_unused wxCommandEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);

    EditPathDlg dlg(this,
                    m_pProject ? m_pProject->GetBasePath() : "",
                    m_pProject ? m_pProject->GetBasePath() : "",
                    _("Add file"), "", false, true);

    PlaceWindow(&dlg);
    if (dlg.ShowModal() == wxID_OK)
    {
        wxString pathAll = dlg.GetPath();
        wxStringTokenizer tokenizer(pathAll, ";", wxTOKEN_STRTOK);
        while (tokenizer.HasMoreTokens())
        {
            wxString path = tokenizer.GetNextToken();
            control->Append(path);
        }
    }
}

void FPOptionsProjectDlg::OnEdit(cb_unused wxCommandEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);
    int sel = control->GetSelection();
    if (sel < 0)
        return;

    bool isDir = false;
    wxString selStr = control->GetString(sel);
    if (wxDirExists(selStr))
        isDir = true;
    EditPathDlg dlg(this,
                    selStr,
                    m_pProject ? m_pProject->GetBasePath() : "",
                    isDir ? _("Edit directory") : _("Edit file"), "", isDir);

    PlaceWindow(&dlg);
    if (dlg.ShowModal() == wxID_OK)
    {
        wxString path = dlg.GetPath();
        control->SetString(sel, path);
    }
}

void FPOptionsProjectDlg::OnDelete(cb_unused wxCommandEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);
    int sel = control->GetSelection();
    if (sel < 0)
        return;

    control->Delete(sel);
}

void FPOptionsProjectDlg::OnAddInclude(cb_unused wxCommandEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPathsInclude", wxListBox);

    EditPathDlg dlg(this,
                    m_pProject ? m_pProject->GetBasePath() : "",
                    m_pProject ? m_pProject->GetBasePath() : "",
                    _("Add directory"));

    PlaceWindow(&dlg);
    if (dlg.ShowModal() == wxID_OK)
    {
        wxString path = dlg.GetPath();
        control->Append(path);
    }
}

void FPOptionsProjectDlg::OnEditInclude(cb_unused wxCommandEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPathsInclude", wxListBox);
    int sel = control->GetSelection();
    if (sel < 0)
        return;

    EditPathDlg dlg(this,
                    control->GetString(sel),
                    m_pProject ? m_pProject->GetBasePath() : "",
                    _("Edit directory"));

    PlaceWindow(&dlg);
    if (dlg.ShowModal() == wxID_OK)
    {
        wxString path = dlg.GetPath();
        control->SetString(sel, path);
    }
}

void FPOptionsProjectDlg::OnDeleteInclude(cb_unused wxCommandEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPathsInclude", wxListBox);
    int sel = control->GetSelection();
    if (sel < 0)
        return;

    control->Delete(sel);
}

void FPOptionsProjectDlg::OnUpdateUI(cb_unused wxUpdateUIEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);
    bool en = control->GetSelection() >= 0;
    XRCCTRL(*this, "btnEdit", wxButton)->Enable(en);
    XRCCTRL(*this, "btnDelete", wxButton)->Enable(en);

    control = XRCCTRL(*this, "lstPathsInclude", wxListBox);
    en = control->GetSelection() >= 0;
    XRCCTRL(*this, "btnEditInclude", wxButton)->Enable(en);
    XRCCTRL(*this, "btnDeleteInclude", wxButton)->Enable(en);
}

void FPOptionsProjectDlg::OnApply()
{
    if (!m_pNativeParser || !m_pProject)
        return;

    wxArrayString newpathsSearch;
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);
    for (int i = 0; i < (int)control->GetCount(); ++i)
    {
        wxString dir = UnixFilename(control->GetString(i));
        newpathsSearch.Add(dir);
    }

    if (m_OldPaths != newpathsSearch)
    {
        m_pNativeParser->SetProjectSearchDirs(m_pProject, newpathsSearch);
        m_pNativeParser->ForceReparseProjectSearchDirs();
    }

    bool forceReparseWorkspace = false;
    wxArrayString newpathsInclude;
    control = XRCCTRL(*this, "lstPathsInclude", wxListBox);
    for (int i = 0; i < (int)control->GetCount(); ++i)
    {
        wxString dir = UnixFilename(control->GetString(i));
        newpathsInclude.Add(dir);
    }

    if (m_OldPathsInclude != newpathsInclude)
    {
        m_pNativeParser->SetProjectIncludeDirs(m_pProject, newpathsInclude);
        m_pNativeParser->MakeAIncludeFileList();
        forceReparseWorkspace = true;
    }

    wxTextCtrl* txtMacros = XRCCTRL(*this, "txtCPPMacros", wxTextCtrl);
    wxStringTokenizer tokenizer(txtMacros->GetValue(), " ;\t\r\n", wxTOKEN_STRTOK);
    std::set<wxString> macrosSet;
    while ( tokenizer.HasMoreTokens() )
    {
        wxString token = tokenizer.GetNextToken();
        macrosSet.insert(token);
    }
    wxString strMacros;
    for (auto m : macrosSet)
    {
        strMacros << (m + "; ");
    }

    if (strMacros != m_OldCPPMacros)
    {
        m_pNativeParser->SetProjectCPPMacros(m_pProject, strMacros);
        forceReparseWorkspace = true;
    }

    if (forceReparseWorkspace)
        m_pNativeParser->ForceReparseWorkspace();
}
