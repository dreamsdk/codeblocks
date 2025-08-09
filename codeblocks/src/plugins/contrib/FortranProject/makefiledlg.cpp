
#include "makefiledlg.h"

#include <sdk.h>
#include "configmanager.h"
#ifndef CB_PRECOMP
    #include <wx/filedlg.h>
#endif

//(*InternalHeaders(MakefileDlg)
#include <wx/intl.h>
#include <wx/string.h>
//*)

//(*IdInit(MakefileDlg)
const wxWindowID MakefileDlg::ID_STATICTEXT2 = wxNewId();
const wxWindowID MakefileDlg::ID_TEXTCTRL1 = wxNewId();
const wxWindowID MakefileDlg::ID_BUTTON1 = wxNewId();
//*)

BEGIN_EVENT_TABLE(MakefileDlg,wxDialog)
	//(*EventTable(MakefileDlg)
	//*)
END_EVENT_TABLE()

MakefileDlg::MakefileDlg(wxWindow* parent,wxWindowID id)
{
	//(*Initialize(MakefileDlg)
	wxBoxSizer* BoxSizer1;
	wxBoxSizer* BoxSizer2;
	wxBoxSizer* BoxSizer3;
	wxBoxSizer* BoxSizer4;
	wxStdDialogButtonSizer* StdDialogButtonSizer1;

	Create(parent, wxID_ANY, _("Generate Makefile"), wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER, _T("wxID_ANY"));
	BoxSizer1 = new wxBoxSizer(wxVERTICAL);
	BoxSizer2 = new wxBoxSizer(wxVERTICAL);
	BoxSizer4 = new wxBoxSizer(wxHORIZONTAL);
	StaticText2 = new wxStaticText(this, ID_STATICTEXT2, _("Choose a name for the make file:"), wxDefaultPosition, wxDefaultSize, 0, _T("ID_STATICTEXT2"));
	BoxSizer4->Add(StaticText2, 0, wxALL, 5);
	BoxSizer4->Add(100,-1,0, wxALL|wxEXPAND, 5);
	BoxSizer2->Add(BoxSizer4, 0, wxALL|wxEXPAND, 5);
	BoxSizer3 = new wxBoxSizer(wxHORIZONTAL);
	tcMakefileName = new wxTextCtrl(this, ID_TEXTCTRL1, _("Text"), wxDefaultPosition, wxSize(-1,-1), 0, wxDefaultValidator, _T("ID_TEXTCTRL1"));
	BoxSizer3->Add(tcMakefileName, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	btChooseFileName = new wxButton(this, ID_BUTTON1, _T("..."), wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT, wxDefaultValidator, _T("ID_BUTTON1"));
	BoxSizer3->Add(btChooseFileName, 0, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer2->Add(BoxSizer3, 1, wxALL|wxEXPAND, 0);
	BoxSizer1->Add(BoxSizer2, 1, wxALL|wxEXPAND, 5);
	StdDialogButtonSizer1 = new wxStdDialogButtonSizer();
	StdDialogButtonSizer1->AddButton(new wxButton(this, wxID_OK, wxEmptyString));
	StdDialogButtonSizer1->AddButton(new wxButton(this, wxID_CANCEL, wxEmptyString));
	StdDialogButtonSizer1->Realize();
	BoxSizer1->Add(StdDialogButtonSizer1, 0, wxALL|wxALIGN_RIGHT, 5);
	SetSizer(BoxSizer1);
	BoxSizer1->SetSizeHints(this);
	Center();

	Connect(ID_BUTTON1,wxEVT_COMMAND_BUTTON_CLICKED,wxCommandEventHandler(MakefileDlg::OnbtChooseFileNameClick));
	Connect(wxID_ANY,wxEVT_INIT_DIALOG,wxInitDialogEventHandler(MakefileDlg::OnInit));
	//*)
}

MakefileDlg::~MakefileDlg()
{
	//(*Destroy(MakefileDlg)
	//*)
}


void MakefileDlg::OnInit(wxInitDialogEvent& event)
{
}

void MakefileDlg::SetFilename(const wxString& fname)
{
    tcMakefileName->SetValue(fname);
}

wxString MakefileDlg::GetFilename()
{
    return tcMakefileName->GetValue();
}

void MakefileDlg::OnbtChooseFileNameClick(wxCommandEvent& event)
{
    wxFileDialog saveFileDialog(this, _("Choose file name"), wxEmptyString, wxEmptyString, "All files (*)|*", wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
    saveFileDialog.SetPath(tcMakefileName->GetValue());
    PlaceWindow(&saveFileDialog);
    if (saveFileDialog.ShowModal() != wxID_OK)
        return;
    tcMakefileName->SetValue(saveFileDialog.GetPath());
}
