#include "bindtonewtype.h"
#include "globals.h"

//(*InternalHeaders(BindtoNewType)
#include <wx/intl.h>
#include <wx/button.h>
#include <wx/string.h>
//*)

//(*IdInit(BindtoNewType)
const long BindtoNewType::ID_TEXTCTRL1 = wxNewId();
const long BindtoNewType::ID_TEXTCTRL2 = wxNewId();
const long BindtoNewType::ID_TEXTCTRL3 = wxNewId();
//*)

BEGIN_EVENT_TABLE(BindtoNewType,wxDialog)
	//(*EventTable(BindtoNewType)
	//*)
	EVT_BUTTON  (wxID_OK, BindtoNewType::OnOK)
END_EVENT_TABLE()

BindtoNewType::BindtoNewType(wxWindow* parent,wxWindowID id,const wxPoint& pos,const wxSize& size)
{
	//(*Initialize(BindtoNewType)
	wxStaticText* StaticText2;
	wxStaticText* StaticText1;
	wxStaticText* StaticText3;
	wxBoxSizer* BoxSizer1;
	wxFlexGridSizer* FlexGridSizer1;
	wxStdDialogButtonSizer* StdDialogButtonSizer1;

	Create(parent, wxID_ANY, _("Add New Type"), wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER, _T("wxID_ANY"));
	SetClientSize(wxSize(400,180));
	SetMinSize(wxSize(200,100));
	BoxSizer1 = new wxBoxSizer(wxVERTICAL);
	FlexGridSizer1 = new wxFlexGridSizer(3, 2, 0, 0);
	FlexGridSizer1->AddGrowableCol(1);
	FlexGridSizer1->AddGrowableRow(2);
	StaticText1 = new wxStaticText(this, wxID_ANY, _("Fortran type:"), wxDefaultPosition, wxDefaultSize, 0, _T("wxID_ANY"));
	FlexGridSizer1->Add(StaticText1, 0, wxTOP|wxBOTTOM|wxRIGHT|wxALIGN_CENTER_VERTICAL, 5);
	tc_Fortran = new wxTextCtrl(this, ID_TEXTCTRL1, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_TEXTCTRL1"));
	FlexGridSizer1->Add(tc_Fortran, 1, wxTOP|wxBOTTOM|wxEXPAND, 5);
	StaticText2 = new wxStaticText(this, wxID_ANY, _("Fortran Bind(C):"), wxDefaultPosition, wxDefaultSize, 0, _T("wxID_ANY"));
	FlexGridSizer1->Add(StaticText2, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5);
	tc_BindC = new wxTextCtrl(this, ID_TEXTCTRL2, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_TEXTCTRL2"));
	FlexGridSizer1->Add(tc_BindC, 1, wxTOP|wxBOTTOM|wxEXPAND, 5);
	StaticText3 = new wxStaticText(this, wxID_ANY, _("C type:"), wxDefaultPosition, wxDefaultSize, 0, _T("wxID_ANY"));
	FlexGridSizer1->Add(StaticText3, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5);
	tc_C = new wxTextCtrl(this, ID_TEXTCTRL3, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_TEXTCTRL3"));
	FlexGridSizer1->Add(tc_C, 1, wxTOP|wxBOTTOM|wxEXPAND, 5);
	BoxSizer1->Add(FlexGridSizer1, 0, wxALL|wxEXPAND, 5);
	BoxSizer1->Add(-1,-1,1, wxALL|wxALIGN_CENTER_HORIZONTAL, 5);
	StdDialogButtonSizer1 = new wxStdDialogButtonSizer();
	StdDialogButtonSizer1->AddButton(new wxButton(this, wxID_OK, wxEmptyString));
	StdDialogButtonSizer1->AddButton(new wxButton(this, wxID_CANCEL, wxEmptyString));
	StdDialogButtonSizer1->Realize();
	BoxSizer1->Add(StdDialogButtonSizer1, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 5);
	SetSizer(BoxSizer1);
	SetSizer(BoxSizer1);
	Layout();
	//*)
}

BindtoNewType::~BindtoNewType()
{
	//(*Destroy(BindtoNewType)
	//*)
}

void BindtoNewType::OnOK(wxCommandEvent& event)
{
    if (tc_Fortran->GetValue().Trim() == wxEmptyString ||
        tc_BindC->GetValue().Trim() == wxEmptyString ||
        tc_C->GetValue().Trim() == wxEmptyString)
    {
        wxString mstr = _T("All text fields should be filled!");
        cbMessageBox(mstr, _("Error"), wxICON_ERROR);
        return;
    }
    EndModal(wxID_OK);
}

void BindtoNewType::SetEditType(const wxString& ft, const wxString& bt, const wxString& ct)
{
    this->SetTitle(_("Edit Type"));
    tc_Fortran->SetValue(ft);
    tc_BindC->SetValue(bt);
    tc_C->SetValue(ct);
}

wxString BindtoNewType::GetFortranType()
{
    return tc_Fortran->GetValue().Lower();
}

wxString BindtoNewType::GetBindCType()
{
    return tc_BindC->GetValue().Lower();
}

wxString BindtoNewType::GetCType()
{
    return tc_C->GetValue();
}
