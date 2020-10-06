#ifndef CHANGECASE_H
#define CHANGECASE_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include "scrollingdialog.h"
#endif

class cbEditor;

//(*Headers(ChangeCase)
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/radiobut.h>
#include <wx/stattext.h>
//*)

enum ChangeCaseIn
{
    chciProject = 0,
    chciFile,
    chciSelection,
};

enum ChangeCaseFor
{
    chcfKeywords = 0x0001,
    chcfOther = 0x0002,
};

enum ChangeCaseTo
{
    chctAllCaps = 0,
    chctFirstCap,
    chctAllLower,
};

class ChangeCase: public wxScrollingDialog
{
	public:

		ChangeCase(wxWindow* parent);
		virtual ~ChangeCase();

		//(*Declarations(ChangeCase)
		wxRadioButton* rb_ChCActiveProject;
		wxRadioButton* rb_ChCFirstCap;
		wxRadioButton* rb_ChCAllLower;
		wxStaticText* StaticText1;
		wxRadioButton* rb_ChCAllCaps;
		wxStaticText* StaticText3;
		wxRadioButton* rb_ChCSelection;
		wxCheckBox* cb_ChCOtherItems;
		wxCheckBox* cb_ChCKeywords;
		wxRadioButton* rb_ChCCurrentFile;
		wxStaticText* StaticText2;
		//*)

	protected:

		//(*Identifiers(ChangeCase)
		//*)

	private:

		//(*Handlers(ChangeCase)
		//*)
		void OnOK(wxCommandEvent& event);
		void MakeChangeCase(ChangeCaseIn chin, int chfor, ChangeCaseTo chto);
		void FileChangeCase(wxString filename, ChangeCaseIn chin, int chfor, ChangeCaseTo chto);
        bool EditorChangeCase(cbEditor* ed, ChangeCaseIn chin, int chfor, ChangeCaseTo chto);

		DECLARE_EVENT_TABLE()
};

#endif
