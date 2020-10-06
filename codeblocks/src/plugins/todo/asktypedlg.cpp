/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * $Revision: 7897 $
 * $Id: asktypedlg.cpp 7897 2012-03-13 21:39:52Z danselmi $
 * $HeadURL: svn://svn.code.sf.net/p/codeblocks/code/branches/release-20.xx/src/plugins/todo/asktypedlg.cpp $
 */

#include "sdk.h"
#ifndef CB_PRECOMP
  #include <wx/choice.h>
  #include <wx/intl.h>
  #include <wx/string.h>
  #include <wx/xrc/xmlres.h>
#endif
#include "asktypedlg.h"


AskTypeDlg::AskTypeDlg(wxWindow* parent, const wxString StreamStart, const wxString StreamEnd)
{
    wxXmlResource::Get()->LoadObject(this, parent, _T("dlgAskType"),_T("wxScrollingDialog"));

    wxChoice *choice = XRCCTRL(*this, "chcCorrect", wxChoice);
    choice->Clear();
    choice->Append( _T("keep line comment style and move it to the end of the line") );
    choice->Append( _T("keep line comment style at the current position") );

    if (!StreamStart.IsEmpty())
    {
        wxString str = _("switch to stream style comment (") + StreamStart + _T(" ... ") + StreamEnd + _T(")");
        choice->Append(str);
    }
    choice->SetSelection(0);
}

AskTypeDlg::~AskTypeDlg()
{
    //dtor
}

TypeCorrection AskTypeDlg::GetTypeCorrection() const
{
    return static_cast<TypeCorrection>(XRCCTRL(*this, "chcCorrect", wxChoice)->GetSelection());
}

