#ifndef MAKEFILEDLG_H
#define MAKEFILEDLG_H

//(*Headers(MakefileDlg)
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
//*)

class MakefileDlg: public wxDialog
{
	public:

		MakefileDlg(wxWindow* parent,wxWindowID id=wxID_ANY);
		virtual ~MakefileDlg();

		void SetFilename(const wxString& fname);
		wxString GetFilename();

		//(*Declarations(MakefileDlg)
		wxButton* btChooseFileName;
		wxStaticText* StaticText2;
		wxTextCtrl* tcMakefileName;
		//*)

	protected:

		//(*Identifiers(MakefileDlg)
		static const long ID_STATICTEXT2;
		static const long ID_TEXTCTRL1;
		static const long ID_BUTTON1;
		//*)

	private:

		//(*Handlers(MakefileDlg)
		void OnInit(wxInitDialogEvent& event);
		void OnbtChooseFileNameClick(wxCommandEvent& event);
		//*)

		DECLARE_EVENT_TABLE()
};

#endif
