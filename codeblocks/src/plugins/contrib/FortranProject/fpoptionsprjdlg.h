/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef CCOPTIONSPRJDLG_H
#define CCOPTIONSPRJDLG_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/intl.h>
    #include <settings.h>
#endif

#include <configurationpanel.h>

#include "nativeparserf.h"
#include "parserf.h"

class cbProject;

class FPOptionsProjectDlg : public cbConfigurationPanel
{
public:
    FPOptionsProjectDlg(wxWindow* parent, cbProject* project, NativeParserF* np);
    virtual ~FPOptionsProjectDlg();

    virtual wxString GetTitle() const          { return _("Fortran"); }
    virtual wxString GetBitmapBaseName() const { return _T("generic-plugin"); }
    virtual void OnApply();
    virtual void OnCancel(){}

protected:
    void OnAddDir(wxCommandEvent& event);
    void OnAddFile(wxCommandEvent& event);
    void OnEdit(wxCommandEvent& event);
    void OnDelete(wxCommandEvent& event);
    void OnAddInclude(cb_unused wxCommandEvent& event);
    void OnEditInclude(cb_unused wxCommandEvent& event);
    void OnDeleteInclude(cb_unused wxCommandEvent& event);
    void OnUpdateUI(wxUpdateUIEvent& event);

private:
    cbProject*    m_pProject;
    NativeParserF* m_pNativeParser;
    wxArrayString m_OldPaths;
    wxArrayString m_OldPathsInclude;
    wxString      m_OldCPPMacros;

    DECLARE_EVENT_TABLE()
};

#endif // CCOPTIONSPRJDLG_H
