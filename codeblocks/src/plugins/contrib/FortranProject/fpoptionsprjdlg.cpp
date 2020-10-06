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

#include <editpathdlg.h>

BEGIN_EVENT_TABLE(FPOptionsProjectDlg, wxPanel)
    EVT_UPDATE_UI(-1, FPOptionsProjectDlg::OnUpdateUI)
    EVT_BUTTON(XRCID("btnAdd"), FPOptionsProjectDlg::OnAdd)
    EVT_BUTTON(XRCID("btnEdit"), FPOptionsProjectDlg::OnEdit)
    EVT_BUTTON(XRCID("btnDelete"), FPOptionsProjectDlg::OnDelete)
END_EVENT_TABLE()

FPOptionsProjectDlg::FPOptionsProjectDlg(wxWindow* parent, cbProject* project, NativeParserF* np) :
    m_pProject(project),
    m_pNativeParser(np)
{
    wxXmlResource::Get()->LoadPanel(this, parent, _T("pnlProjectFPOptions"));
    m_OldPaths = m_pNativeParser->GetProjectSearchDirs(m_pProject);

    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);
    control->Clear();
    for (size_t i = 0; i < m_OldPaths.GetCount(); ++i)
        control->Append(m_OldPaths[i]);
}

FPOptionsProjectDlg::~FPOptionsProjectDlg()
{
}

void FPOptionsProjectDlg::OnAdd(cb_unused wxCommandEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);

    EditPathDlg dlg(this,
                    m_pProject ? m_pProject->GetBasePath() : _T(""),
                    m_pProject ? m_pProject->GetBasePath() : _T(""),
                    _("Add directory"));

    PlaceWindow(&dlg);
    if (dlg.ShowModal() == wxID_OK)
    {
        wxString path = dlg.GetPath();
        control->Append(path);
    }
}

void FPOptionsProjectDlg::OnEdit(cb_unused wxCommandEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);
    int sel = control->GetSelection();
    if (sel < 0)
        return;

    EditPathDlg dlg(this,
                    control->GetString(sel),
                    m_pProject ? m_pProject->GetBasePath() : _T(""),
                    _("Edit directory"));

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

void FPOptionsProjectDlg::OnUpdateUI(cb_unused wxUpdateUIEvent& event)
{
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);
    bool en = control->GetSelection() >= 0;

    XRCCTRL(*this, "btnEdit", wxButton)->Enable(en);
    XRCCTRL(*this, "btnDelete", wxButton)->Enable(en);
}

void FPOptionsProjectDlg::OnApply()
{
    wxArrayString newpaths;
    wxListBox* control = XRCCTRL(*this, "lstPaths", wxListBox);
    for (int i = 0; i < (int)control->GetCount(); ++i)
        newpaths.Add(control->GetString(i));

    if (!m_pNativeParser || !m_pProject)
        return;

    if (m_OldPaths != newpaths)
    {
        m_pNativeParser->SetProjectSearchDirs(m_pProject, newpaths);
        m_pNativeParser->ForceReparseProjectSearchDirs();
    }
}
