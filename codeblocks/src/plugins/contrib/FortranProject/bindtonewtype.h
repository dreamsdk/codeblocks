#ifndef BINDTONEWTYPE_H
#define BINDTONEWTYPE_H

//(*Headers(BindtoNewType)
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/dialog.h>
//*)

class BindtoNewType: public wxDialog
{
	public:

		BindtoNewType(wxWindow* parent,wxWindowID id=wxID_ANY,const wxPoint& pos=wxDefaultPosition,const wxSize& size=wxDefaultSize);
		virtual ~BindtoNewType();

		//(*Declarations(BindtoNewType)
		wxTextCtrl* tc_Fortran;
		wxTextCtrl* tc_C;
		wxTextCtrl* tc_BindC;
		//*)
		void SetEditType(const wxString& ft, const wxString& bt, const wxString& ct);
		wxString GetFortranType();
		wxString GetBindCType();
		wxString GetCType();

	protected:

		//(*Identifiers(BindtoNewType)
		static const long ID_TEXTCTRL1;
		static const long ID_TEXTCTRL2;
		static const long ID_TEXTCTRL3;
		//*)

	private:

		//(*Handlers(BindtoNewType)
		//*)
		void OnOK(wxCommandEvent& event);

		DECLARE_EVENT_TABLE()
};

#endif
