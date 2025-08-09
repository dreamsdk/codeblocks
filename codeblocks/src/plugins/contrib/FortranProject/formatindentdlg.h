#ifndef FORMATINDENT_H
#define FORMATINDENT_H

//(*Headers(FormatIndentDlg)
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/notebook.h>
#include <wx/panel.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>
//*)

class FormatIndentDlg: public wxDialog
{
	public:

	    enum FormatIndentScope{ fisProject, fisSelection, fisCurrentFile };

		FormatIndentDlg(wxWindow* parent);
		virtual ~FormatIndentDlg();

		//(*Declarations(FormatIndentDlg)
		wxCheckBox* cb_CONTMod;
		wxCheckBox* cb_CONTModAfter;
		wxCheckBox* cb_CONTProc;
		wxCheckBox* cb_CONTProcAfter;
		wxCheckBox* cb_CONTType;
		wxCheckBox* cb_CONTTypeAfter;
		wxCheckBox* cb_MODafter;
		wxCheckBox* cb_PROGafter;
		wxCheckBox* cb_SELECTCASEafter;
		wxCheckBox* cb_SELECTTYPEafter;
		wxCheckBox* cb_TabAsEditor;
		wxCheckBox* cb_TrimLines;
		wxCheckBox* cb_UseTabs;
		wxNotebook* Notebook1;
		wxPanel* Panel1;
		wxPanel* Panel2;
		wxPanel* Panel3;
		wxRadioButton* rb_ActiveProject;
		wxRadioButton* rb_CurrentFile;
		wxRadioButton* rb_Selection;
		wxSpinCtrl* spc_Spaces;
		wxStaticText* StaticText1;
		wxStaticText* StaticText2;
		wxStaticText* StaticText3;
		wxStaticText* stxt_TabSpaces;
		//*)

        FormatIndentScope GetFormatScope();

	protected:

		//(*Identifiers(FormatIndentDlg)
		static const wxWindowID ID_STATICTEXT1;
		static const wxWindowID ID_STATICTEXT3;
		static const wxWindowID ID_RADIOBUTTON1;
		static const wxWindowID ID_RADIOBUTTON2;
		static const wxWindowID ID_RADIOBUTTON3;
		static const wxWindowID ID_PANEL1;
		static const wxWindowID ID_STATICTEXT2;
		static const wxWindowID ID_CHECKBOX2;
		static const wxWindowID ID_CHECKBOX3;
		static const wxWindowID ID_CHECKBOX4;
		static const wxWindowID ID_CHECKBOX5;
		static const wxWindowID ID_CHECKBOX6;
		static const wxWindowID ID_CHECKBOX1;
		static const wxWindowID ID_CHECKBOX7;
		static const wxWindowID ID_CHECKBOX8;
		static const wxWindowID ID_CHECKBOX9;
		static const wxWindowID ID_CHECKBOX10;
		static const wxWindowID ID_PANEL3;
		static const wxWindowID ID_CHECKBOX11;
		static const wxWindowID ID_CHECKBOX12;
		static const wxWindowID ID_CHECKBOX13;
		static const wxWindowID ID_STATICTEXT4;
		static const wxWindowID ID_SPINCTRL1;
		static const wxWindowID ID_PANEL2;
		static const wxWindowID ID_NOTEBOOK1;
		//*)

	private:

		//(*Handlers(FormatIndentDlg)
		void OnCbTabAsEditorClick(wxCommandEvent& event);
		//*)

		void OnOK(wxCommandEvent& event);


		DECLARE_EVENT_TABLE()
};

#endif
