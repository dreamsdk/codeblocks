/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */

#ifndef FINFOWINDOW_H
#define FINFOWINDOW_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/panel.h>
    #include <cbstyledtextctrl.h>
#endif


class FInfoWindow : public wxPanel
{
    public:
        FInfoWindow();
        ~FInfoWindow();
        void RemoveFromNotebook();
        void WriteToInfoWindow(const wxString& text);
    protected:
    private:
        void SetFoldingIndicator();
        void SetMarkerStyle(int marker, int markerType, wxColor fore, wxColor back);

        cbStyledTextCtrl* m_pView;
};

#endif // FINFOWINDOW_H
