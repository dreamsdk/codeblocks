/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef FPOPTIONSDLG_H
#define FPOPTIONSDLG_H

#include <wx/intl.h>
#include "configurationpanel.h"
#include <settings.h>
#include "fortranproject.h"
#include "workspacebrowserf.h"
#include "autoinsert.h"

class FortranProject;

class FPOptionsDlg : public cbConfigurationPanel
{
	public:
		FPOptionsDlg(wxWindow* parent, NativeParserF* np, FortranProject* fp);
		virtual ~FPOptionsDlg();

        virtual wxString GetTitle() const { return _("FortranProject"); }
        virtual wxString GetBitmapBaseName() const { return _T("generic-plugin"); }
        virtual void OnApply();
        virtual void OnCancel(){}
	protected:
        void OnAddRepl(wxCommandEvent& event);
        void OnEditRepl(wxCommandEvent& event);
        void OnDelRepl(wxCommandEvent& event);
		void OnOK(wxCommandEvent& event);
		void OnChooseColour(wxCommandEvent& event);
		void OnSliderScroll(wxScrollEvent& event);
		void ShowCurrientAInsert(int idx);
		void OnAISelectionChanged(wxCommandEvent& event);
		void OnUpdateUI(wxUpdateUIEvent& event);
		void FillAutoInsert();
	private:
	    void ReadAIChoice();
		bool ValidateReplacementToken(wxString& from, wxString& to);
		NativeParserF* m_pNativeParser;
        FortranProject* m_pFortranProject;
        AutoInsert m_AInsert;

        bool m_cbAIAlign_wasEnabled;
        bool m_cbAIAddName_wasEnabled;
        int m_AISelIdx;

		DECLARE_EVENT_TABLE()
};

#endif // FPOPTIONSDLG_H
