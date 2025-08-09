#ifndef BINDTONEWTYPE_H
#define BINDTONEWTYPE_H

//(*Headers(BindtoNewType)
#include <wx/dialog.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
//*)

class BindtoNewType: public wxDialog
{
	public:

		BindtoNewType(wxWindow* parent,wxWindowID id=wxID_ANY,const wxPoint& pos=wxDefaultPosition,const wxSize& size=wxDefaultSize);
		virtual ~BindtoNewType();

		//(*Declarations(BindtoNewType)
		wxTextCtrl* tc_BindC;
		wxTextCtrl* tc_C;
		wxTextCtrl* tc_Fortran;
		//*)
		void SetEditType(const wxString& ft, const wxString& bt, const wxString& ct);
		wxString GetFortranType();
		wxString GetBindCType();
		wxString GetCType();

	protected:

		//(*Identifiers(BindtoNewType)
		static const wxWindowID ID_TEXTCTRL1;
		static const wxWindowID ID_TEXTCTRL2;
		static const wxWindowID ID_TEXTCTRL3;
		//*)

	private:

		//(*Handlers(BindtoNewType)
		//*)
		void OnOK(wxCommandEvent& event);

		DECLARE_EVENT_TABLE()
};

#endif
