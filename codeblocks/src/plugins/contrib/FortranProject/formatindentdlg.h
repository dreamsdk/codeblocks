#ifndef FORMATINDENT_H
#define FORMATINDENT_H

//(*Headers(FormatIndentDlg)
#include <wx/notebook.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/checkbox.h>
#include <wx/spinctrl.h>
#include <wx/radiobut.h>
#include <wx/panel.h>
#include <wx/dialog.h>
//*)

class FormatIndentDlg: public wxDialog
{
	public:

	    enum FormatIndentScope{ fisProject, fisSelection, fisCurrentFile };

		FormatIndentDlg(wxWindow* parent);
		virtual ~FormatIndentDlg();

		//(*Declarations(FormatIndentDlg)
		wxRadioButton* rb_CurrentFile;
		wxSpinCtrl* spc_Spaces;
		wxNotebook* Notebook1;
		wxStaticText* stxt_TabSpaces;
		wxCheckBox* cb_SELECTTYPEafter;
		wxStaticText* StaticText2;
		wxCheckBox* cb_CONTModAfter;
		wxCheckBox* cb_SELECTCASEafter;
		wxCheckBox* cb_MODafter;
		wxPanel* Panel1;
		wxStaticText* StaticText1;
		wxRadioButton* rb_Selection;
		wxStaticText* StaticText3;
		wxCheckBox* cb_CONTMod;
		wxCheckBox* cb_CONTProcAfter;
		wxPanel* Panel3;
		wxCheckBox* cb_CONTTypeAfter;
		wxCheckBox* cb_TrimLines;
		wxCheckBox* cb_TabAsEditor;
		wxRadioButton* rb_ActiveProject;
		wxCheckBox* cb_PROGafter;
		wxCheckBox* cb_UseTabs;
		wxPanel* Panel2;
		wxCheckBox* cb_CONTType;
		wxCheckBox* cb_CONTProc;
		//*)

        FormatIndentScope GetFormatScope();

	protected:

		//(*Identifiers(FormatIndentDlg)
		static const long ID_STATICTEXT1;
		static const long ID_STATICTEXT3;
		static const long ID_RADIOBUTTON1;
		static const long ID_RADIOBUTTON2;
		static const long ID_RADIOBUTTON3;
		static const long ID_PANEL1;
		static const long ID_STATICTEXT2;
		static const long ID_CHECKBOX2;
		static const long ID_CHECKBOX3;
		static const long ID_CHECKBOX4;
		static const long ID_CHECKBOX5;
		static const long ID_CHECKBOX6;
		static const long ID_CHECKBOX1;
		static const long ID_CHECKBOX7;
		static const long ID_CHECKBOX8;
		static const long ID_CHECKBOX9;
		static const long ID_CHECKBOX10;
		static const long ID_PANEL3;
		static const long ID_CHECKBOX11;
		static const long ID_CHECKBOX12;
		static const long ID_CHECKBOX13;
		static const long ID_STATICTEXT4;
		static const long ID_SPINCTRL1;
		static const long ID_PANEL2;
		static const long ID_NOTEBOOK1;
		//*)

	private:

		//(*Handlers(FormatIndentDlg)
		void OnCbTabAsEditorClick(wxCommandEvent& event);
		//*)

		void OnOK(wxCommandEvent& event);


		DECLARE_EVENT_TABLE()
};

#endif
