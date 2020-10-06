#ifndef Tab2Space_H
#define Tab2Space_H

#include <sdk.h> // Code::Blocks SDK
#ifndef CB_PRECOMP
    #include "scrollingdialog.h"
#endif


class cbEditor;

//(*Headers(Tab2Space)
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/radiobut.h>
#include <wx/stattext.h>
#include <wx/spinctrl.h>
//*)

enum Tab2SpaceIn
{
    t2siProject = 0,
    t2siFile,
    t2siSelection,
};

class Tab2Space: public wxScrollingDialog
{
	public:

		Tab2Space(wxWindow* parent);
		virtual ~Tab2Space();

		//(*Declarations(Tab2Space)
		wxRadioButton* rb_ChCActiveProject;
		wxRadioButton* rb_ChCCurrentFile;
		wxRadioButton* rb_ChCSelection;
		wxStaticText* StaticText1;
		wxStaticText* StaticText2;
		wxSpinCtrl* sc_TabSize;
		//*)

	protected:

		//(*Identifiers(Tab2Space)
		//*)

	private:

		//(*Handlers(Tab2Space)
		//*)
		void OnOK(wxCommandEvent& event);
		void MakeTab2Space(Tab2SpaceIn chin, int tabSize);
		void FileTab2Space(wxString filename, Tab2SpaceIn chin, int tabSize);
        bool EditorTab2Space(cbEditor* ed, Tab2SpaceIn chin, int tabSize);

		DECLARE_EVENT_TABLE()
};

#endif
