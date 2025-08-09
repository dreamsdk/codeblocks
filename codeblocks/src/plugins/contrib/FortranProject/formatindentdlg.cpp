
#include "formatindentdlg.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <configmanager.h>
#endif

#include <manager.h>
#include <cbstyledtextctrl.h>
#include <editormanager.h>
#include "cbeditor.h"

//(*InternalHeaders(FormatIndentDlg)
#include <wx/button.h>
#include <wx/intl.h>
#include <wx/string.h>
//*)

//(*IdInit(FormatIndentDlg)
const wxWindowID FormatIndentDlg::ID_STATICTEXT1 = wxNewId();
const wxWindowID FormatIndentDlg::ID_STATICTEXT3 = wxNewId();
const wxWindowID FormatIndentDlg::ID_RADIOBUTTON1 = wxNewId();
const wxWindowID FormatIndentDlg::ID_RADIOBUTTON2 = wxNewId();
const wxWindowID FormatIndentDlg::ID_RADIOBUTTON3 = wxNewId();
const wxWindowID FormatIndentDlg::ID_PANEL1 = wxNewId();
const wxWindowID FormatIndentDlg::ID_STATICTEXT2 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX2 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX3 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX4 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX5 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX6 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX1 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX7 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX8 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX9 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX10 = wxNewId();
const wxWindowID FormatIndentDlg::ID_PANEL3 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX11 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX12 = wxNewId();
const wxWindowID FormatIndentDlg::ID_CHECKBOX13 = wxNewId();
const wxWindowID FormatIndentDlg::ID_STATICTEXT4 = wxNewId();
const wxWindowID FormatIndentDlg::ID_SPINCTRL1 = wxNewId();
const wxWindowID FormatIndentDlg::ID_PANEL2 = wxNewId();
const wxWindowID FormatIndentDlg::ID_NOTEBOOK1 = wxNewId();
//*)

BEGIN_EVENT_TABLE(FormatIndentDlg,wxDialog)
	//(*EventTable(FormatIndentDlg)
	//*)
	EVT_BUTTON  (wxID_OK, FormatIndentDlg::OnOK)
END_EVENT_TABLE()

FormatIndentDlg::FormatIndentDlg(wxWindow* parent)
{
	//(*Initialize(FormatIndentDlg)
	wxBoxSizer* BoxSizer1;
	wxBoxSizer* BoxSizer2;
	wxBoxSizer* BoxSizer3;
	wxBoxSizer* BoxSizer4;
	wxBoxSizer* BoxSizer5;
	wxBoxSizer* BoxSizer6;
	wxBoxSizer* BoxSizer7;
	wxBoxSizer* BoxSizer8;
	wxBoxSizer* BoxSizer9;
	wxStaticBoxSizer* StaticBoxSizer1;
	wxStdDialogButtonSizer* StdDialogButtonSizer1;

	Create(parent, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE, _T("wxID_ANY"));
	BoxSizer1 = new wxBoxSizer(wxVERTICAL);
	Notebook1 = new wxNotebook(this, ID_NOTEBOOK1, wxDefaultPosition, wxDefaultSize, 0, _T("ID_NOTEBOOK1"));
	Panel1 = new wxPanel(Notebook1, ID_PANEL1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, _T("ID_PANEL1"));
	BoxSizer4 = new wxBoxSizer(wxVERTICAL);
	BoxSizer3 = new wxBoxSizer(wxVERTICAL);
	StaticText1 = new wxStaticText(Panel1, ID_STATICTEXT1, _("Format indent for Fortran code."), wxDefaultPosition, wxDefaultSize, 0, _T("ID_STATICTEXT1"));
	BoxSizer3->Add(StaticText1, 0, wxALL|wxALIGN_LEFT, 5);
	StaticText3 = new wxStaticText(Panel1, ID_STATICTEXT3, _("Format indent for:"), wxDefaultPosition, wxDefaultSize, 0, _T("ID_STATICTEXT3"));
	BoxSizer3->Add(StaticText3, 0, wxALL|wxALIGN_LEFT, 5);
	BoxSizer4->Add(BoxSizer3, 0, wxTOP|wxLEFT|wxRIGHT|wxEXPAND, 5);
	BoxSizer2 = new wxBoxSizer(wxHORIZONTAL);
	BoxSizer2->Add(-1,-1,0, wxLEFT|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 9);
	rb_ActiveProject = new wxRadioButton(Panel1, ID_RADIOBUTTON1, _("Active project"), wxDefaultPosition, wxDefaultSize, wxRB_GROUP, wxDefaultValidator, _T("ID_RADIOBUTTON1"));
	BoxSizer2->Add(rb_ActiveProject, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5);
	rb_CurrentFile = new wxRadioButton(Panel1, ID_RADIOBUTTON2, _("Current file"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_RADIOBUTTON2"));
	BoxSizer2->Add(rb_CurrentFile, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5);
	rb_Selection = new wxRadioButton(Panel1, ID_RADIOBUTTON3, _("Selection"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_RADIOBUTTON3"));
	BoxSizer2->Add(rb_Selection, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer4->Add(BoxSizer2, 0, wxBOTTOM|wxLEFT|wxRIGHT|wxALIGN_LEFT, 5);
	Panel1->SetSizer(BoxSizer4);
	Panel3 = new wxPanel(Notebook1, ID_PANEL3, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, _T("ID_PANEL3"));
	BoxSizer6 = new wxBoxSizer(wxVERTICAL);
	BoxSizer7 = new wxBoxSizer(wxHORIZONTAL);
	StaticText2 = new wxStaticText(Panel3, ID_STATICTEXT2, _("Options for statements:"), wxDefaultPosition, wxDefaultSize, 0, _T("ID_STATICTEXT2"));
	BoxSizer7->Add(StaticText2, 0, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer6->Add(BoxSizer7, 0, wxTOP|wxLEFT|wxRIGHT|wxALIGN_LEFT, 5);
	BoxSizer8 = new wxBoxSizer(wxVERTICAL);
	cb_PROGafter = new wxCheckBox(Panel3, ID_CHECKBOX2, _("Indent after PROGRAM, FUNCTION, SUBROUTINE"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX2"));
	cb_PROGafter->SetValue(false);
	BoxSizer8->Add(cb_PROGafter, 1, wxALL|wxALIGN_LEFT, 2);
	cb_MODafter = new wxCheckBox(Panel3, ID_CHECKBOX3, _("Indent after MODULE, SUBMODULE"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX3"));
	cb_MODafter->SetValue(false);
	BoxSizer8->Add(cb_MODafter, 1, wxALL|wxALIGN_LEFT, 2);
	cb_CONTMod = new wxCheckBox(Panel3, ID_CHECKBOX4, _("Unindent CONTAINS at module level"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX4"));
	cb_CONTMod->SetValue(false);
	BoxSizer8->Add(cb_CONTMod, 1, wxALL|wxALIGN_LEFT, 2);
	cb_CONTModAfter = new wxCheckBox(Panel3, ID_CHECKBOX5, _("Indent after CONTAINS at module level"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX5"));
	cb_CONTModAfter->SetValue(false);
	BoxSizer8->Add(cb_CONTModAfter, 1, wxALL|wxALIGN_LEFT, 2);
	cb_CONTProc = new wxCheckBox(Panel3, ID_CHECKBOX6, _("Unindent CONTAINS at procedure level"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX6"));
	cb_CONTProc->SetValue(false);
	BoxSizer8->Add(cb_CONTProc, 1, wxALL|wxALIGN_LEFT, 2);
	cb_CONTProcAfter = new wxCheckBox(Panel3, ID_CHECKBOX1, _("Indent after CONTAINS at procedure level"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX1"));
	cb_CONTProcAfter->SetValue(false);
	BoxSizer8->Add(cb_CONTProcAfter, 1, wxALL|wxALIGN_LEFT, 2);
	cb_CONTType = new wxCheckBox(Panel3, ID_CHECKBOX7, _("Unindent CONTAINS at type definition level"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX7"));
	cb_CONTType->SetValue(false);
	BoxSizer8->Add(cb_CONTType, 1, wxALL|wxALIGN_LEFT, 2);
	cb_CONTTypeAfter = new wxCheckBox(Panel3, ID_CHECKBOX8, _("Indent after CONTAINS at type definition level"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX8"));
	cb_CONTTypeAfter->SetValue(false);
	BoxSizer8->Add(cb_CONTTypeAfter, 1, wxALL|wxALIGN_LEFT, 2);
	cb_SELECTCASEafter = new wxCheckBox(Panel3, ID_CHECKBOX9, _("Indent after SELECT CASE"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX9"));
	cb_SELECTCASEafter->SetValue(false);
	BoxSizer8->Add(cb_SELECTCASEafter, 1, wxALL|wxALIGN_LEFT, 2);
	cb_SELECTTYPEafter = new wxCheckBox(Panel3, ID_CHECKBOX10, _("Indent after SELECT TYPE"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX10"));
	cb_SELECTTYPEafter->SetValue(false);
	BoxSizer8->Add(cb_SELECTTYPEafter, 1, wxALL|wxALIGN_LEFT, 2);
	BoxSizer6->Add(BoxSizer8, 1, wxALL|wxEXPAND, 5);
	Panel3->SetSizer(BoxSizer6);
	Panel2 = new wxPanel(Notebook1, ID_PANEL2, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, _T("ID_PANEL2"));
	BoxSizer5 = new wxBoxSizer(wxVERTICAL);
	cb_TrimLines = new wxCheckBox(Panel2, ID_CHECKBOX11, _("Trim lines from right"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX11"));
	cb_TrimLines->SetValue(false);
	BoxSizer5->Add(cb_TrimLines, 0, wxALL|wxALIGN_LEFT, 5);
	StaticBoxSizer1 = new wxStaticBoxSizer(wxVERTICAL, Panel2, _("Tab options"));
	cb_TabAsEditor = new wxCheckBox(Panel2, ID_CHECKBOX12, _("Same as C::B editor"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX12"));
	cb_TabAsEditor->SetValue(false);
	StaticBoxSizer1->Add(cb_TabAsEditor, 0, wxALL|wxALIGN_LEFT, 2);
	cb_UseTabs = new wxCheckBox(Panel2, ID_CHECKBOX13, _("Use tabs instead of spaces"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX13"));
	cb_UseTabs->SetValue(false);
	StaticBoxSizer1->Add(cb_UseTabs, 0, wxALL|wxALIGN_LEFT, 2);
	BoxSizer9 = new wxBoxSizer(wxHORIZONTAL);
	stxt_TabSpaces = new wxStaticText(Panel2, ID_STATICTEXT4, _("Indent size in spaces:"), wxDefaultPosition, wxDefaultSize, 0, _T("ID_STATICTEXT4"));
	BoxSizer9->Add(stxt_TabSpaces, 0, wxRIGHT|wxALIGN_CENTER_VERTICAL, 4);
	spc_Spaces = new wxSpinCtrl(Panel2, ID_SPINCTRL1, _T("0"), wxDefaultPosition, wxDefaultSize, 0, 1, 16, 0, _T("ID_SPINCTRL1"));
	spc_Spaces->SetValue(_T("0"));
	BoxSizer9->Add(spc_Spaces, 1, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 2);
	StaticBoxSizer1->Add(BoxSizer9, 0, wxALL|wxALIGN_LEFT, 2);
	BoxSizer5->Add(StaticBoxSizer1, 0, wxALL|wxALIGN_LEFT, 5);
	Panel2->SetSizer(BoxSizer5);
	Notebook1->AddPage(Panel1, _("Scope"), false);
	Notebook1->AddPage(Panel3, _("Statements"), false);
	Notebook1->AddPage(Panel2, _("Others"), false);
	BoxSizer1->Add(Notebook1, 1, wxALL|wxEXPAND, 5);
	StdDialogButtonSizer1 = new wxStdDialogButtonSizer();
	StdDialogButtonSizer1->AddButton(new wxButton(this, wxID_OK, wxEmptyString));
	StdDialogButtonSizer1->AddButton(new wxButton(this, wxID_CANCEL, wxEmptyString));
	StdDialogButtonSizer1->Realize();
	BoxSizer1->Add(StdDialogButtonSizer1, 0, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	SetSizer(BoxSizer1);
	BoxSizer1->SetSizeHints(this);
	Center();

	Connect(ID_CHECKBOX12,wxEVT_COMMAND_CHECKBOX_CLICKED,wxCommandEventHandler(FormatIndentDlg::OnCbTabAsEditorClick));
	//*)

	rb_Selection->Disable();
	if (!Manager::Get()->GetEditorManager())
        return;
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (!ed)
    {
        rb_ActiveProject->SetValue(true);
        rb_CurrentFile->Disable();
        return;
    }
    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return;
    if (!control->GetSelectedText().IsEmpty())
    {
        rb_Selection->Enable();
        rb_Selection->SetValue(true);
    }
    else
        rb_CurrentFile->SetValue(true);

    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");

    cb_PROGafter->SetValue(cfg->ReadBool("/indent_prog_fun_sub_after", true));
    cb_MODafter->SetValue(cfg->ReadBool("/indent_module_after", true));
    cb_CONTMod->SetValue(cfg->ReadBool("/indent_contains_module", true));
    cb_CONTModAfter->SetValue(cfg->ReadBool("/indent_contains_module_after", true));
    cb_CONTProc->SetValue(cfg->ReadBool("/indent_contains_procedure", true));
    cb_CONTProcAfter->SetValue(cfg->ReadBool("/indent_contains_procedure_after", true));
    cb_CONTType->SetValue(cfg->ReadBool("/indent_contains_typedef", true));
    cb_CONTTypeAfter->SetValue(cfg->ReadBool("/indent_contains_typedef_after", true));
    cb_SELECTCASEafter->SetValue(cfg->ReadBool("/indent_selectcase_after", true));
    cb_SELECTTYPEafter->SetValue(cfg->ReadBool("/indent_selecttype_after", true));

    cb_TrimLines->SetValue(cfg->ReadBool("/indent_trim_right", true));
    bool sae = cfg->ReadBool("/indent_same_as_editor", true);
    cb_TabAsEditor->SetValue(sae);
    cb_UseTabs->SetValue(cfg->ReadBool("/indent_use_tabs", false));
    spc_Spaces->SetValue(cfg->ReadInt("/indent_tab_width", 4));

    cb_UseTabs->Enable(!sae);
    spc_Spaces->Enable(!sae);
    stxt_TabSpaces->Enable(!sae);
}

FormatIndentDlg::~FormatIndentDlg()
{
	//(*Destroy(FormatIndentDlg)
	//*)
}

void FormatIndentDlg::OnOK(wxCommandEvent& event)
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");

    cfg->Write("/indent_prog_fun_sub_after", (bool)cb_PROGafter->GetValue());
    cfg->Write("/indent_module_after", (bool)cb_MODafter->GetValue());
    cfg->Write("/indent_contains_module", (bool)cb_CONTMod->GetValue());
    cfg->Write("/indent_contains_module_after", (bool)cb_CONTModAfter->GetValue());
    cfg->Write("/indent_contains_procedure", (bool)cb_CONTProc->GetValue());
    cfg->Write("/indent_contains_procedure_after", (bool)cb_CONTProcAfter->GetValue());
    cfg->Write("/indent_contains_typedef", (bool)cb_CONTType->GetValue());
    cfg->Write("/indent_contains_typedef_after", (bool)cb_CONTTypeAfter->GetValue());
    cfg->Write("/indent_selectcase_after", (bool)cb_SELECTCASEafter->GetValue());
    cfg->Write("/indent_selecttype_after", (bool)cb_SELECTTYPEafter->GetValue());

    cfg->Write("/indent_trim_right", (bool)cb_TrimLines->GetValue());
    cfg->Write("/indent_same_as_editor", (bool)cb_TabAsEditor->GetValue());
    cfg->Write("/indent_use_tabs", (bool)cb_UseTabs->GetValue());
    cfg->Write("/indent_tab_width", (int)spc_Spaces->GetValue());

    EndModal(wxID_OK);
}

FormatIndentDlg::FormatIndentScope FormatIndentDlg::GetFormatScope()
{
    FormatIndentScope  sc;
    if (rb_ActiveProject->GetValue())
        sc = fisProject;
    else if (rb_CurrentFile->GetValue())
        sc = fisCurrentFile;
    else
        sc = fisSelection;
    return sc;
}


void FormatIndentDlg::OnCbTabAsEditorClick(wxCommandEvent& event)
{
    bool sae = cb_TabAsEditor->GetValue();
    cb_UseTabs->Enable(!sae);
    spc_Spaces->Enable(!sae);
    stxt_TabSpaces->Enable(!sae);
}
