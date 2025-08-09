/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU Lesser General Public License, version 3
 * http://www.gnu.org/licenses/lgpl-3.0.html
 *
 * $Revision: 10912 $
 * $Id: genericmultilinenotesdlg.cpp 10912 2016-09-25 16:10:13Z fuscated $
 * $HeadURL: https://svn.code.sf.net/p/codeblocks/code/branches/release-25.03/src/sdk/genericmultilinenotesdlg.cpp $
 */

#include "sdk_precomp.h"

#ifndef CB_PRECOMP
    #include <wx/button.h>
    #include <wx/textctrl.h>
    #include <wx/xrc/xmlres.h>
#endif

#include "genericmultilinenotesdlg.h"

BEGIN_EVENT_TABLE(GenericMultiLineNotesDlg, wxScrollingDialog)
    //
END_EVENT_TABLE()

GenericMultiLineNotesDlg::GenericMultiLineNotesDlg(wxWindow* parent, const wxString& caption, const wxString& notes, bool readOnly)
    : m_Notes(notes),
    m_ReadOnly(readOnly)
{
    //ctor
    wxXmlResource::Get()->LoadObject(this, parent, _T("dlgGenericMultiLineNotes"),_T("wxScrollingDialog"));
    SetTitle(caption);

    wxTextCtrl *notesCtrl = XRCCTRL(*this, "txtNotes", wxTextCtrl);

    notesCtrl->SetValue(m_Notes);
    if (m_ReadOnly)
    {
        notesCtrl->SetEditable(false);
        if (wxWindow* win = FindWindowById(wxID_CANCEL, this))
        {
            win->Enable(false);
        }
        // If the control is editable the user cannot activate the default button with
        // the enter key, so we set the default button only for read only notes.
        XRCCTRL(*this, "wxID_OK", wxButton)->SetDefault();
    }
    else
        notesCtrl->SetFocus();
}

GenericMultiLineNotesDlg::~GenericMultiLineNotesDlg()
{
    //dtor
}

void GenericMultiLineNotesDlg::EndModal(int retCode)
{
    if (retCode == wxID_OK && !m_ReadOnly)
    {
        m_Notes = XRCCTRL(*this, "txtNotes", wxTextCtrl)->GetValue();
    }
    wxScrollingDialog::EndModal(retCode);
}
