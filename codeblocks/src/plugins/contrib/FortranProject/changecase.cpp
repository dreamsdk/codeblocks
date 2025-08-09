
#include "changecase.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <cbauibook.h>
    #include <cbeditor.h>
    #include <cbproject.h>
    #include <editormanager.h>
    #include <editorcolourset.h>
    #include <logmanager.h>
    #include <projectmanager.h>
    #include <projectfile.h>
#endif
#include <set>

#include <wx/tokenzr.h>

#include <cbstyledtextctrl.h>

#include "fortranfileext.h"
#include "textcutter.h"

extern FortranFileExt g_FortranFileExt;

//(*InternalHeaders(ChangeCase)
#include <wx/xrc/xmlres.h>
//*)

//(*IdInit(ChangeCase)
//*)

BEGIN_EVENT_TABLE(ChangeCase,wxScrollingDialog)
	//(*EventTable(ChangeCase)
	//*)
	EVT_BUTTON  (wxID_OK,   ChangeCase::OnOK)
END_EVENT_TABLE()

ChangeCase::ChangeCase(wxWindow* parent)
{
	//(*Initialize(ChangeCase)
	wxXmlResource::Get()->LoadObject(this,parent,_T("ChangeCase"),_T("wxScrollingDialog"));
	StaticText1 = (wxStaticText*)FindWindow(XRCID("ID_STATICTEXT1"));
	rb_ChCActiveProject = (wxRadioButton*)FindWindow(XRCID("ID_CHCACTIVEPROJECT"));
	rb_ChCCurrentFile = (wxRadioButton*)FindWindow(XRCID("ID_CHCCURRENTFILE"));
	rb_ChCSelection = (wxRadioButton*)FindWindow(XRCID("ID_CHCSELECTION"));
	StaticText2 = (wxStaticText*)FindWindow(XRCID("ID_STATICTEXT2"));
	cb_ChCKeywords = (wxCheckBox*)FindWindow(XRCID("ID_CHCKEYWORDS"));
	cb_ChCOtherItems = (wxCheckBox*)FindWindow(XRCID("ID_CHCOTHERITEMS"));
	StaticText3 = (wxStaticText*)FindWindow(XRCID("ID_STATICTEXT3"));
	rb_ChCAllCaps = (wxRadioButton*)FindWindow(XRCID("ID_CHCALLCAPS"));
	rb_ChCFirstCap = (wxRadioButton*)FindWindow(XRCID("ID_CHCFIRSTCAP"));
	rb_ChCAllLower = (wxRadioButton*)FindWindow(XRCID("ID_CHCALLLOWER"));
	//*)

	rb_ChCCurrentFile->SetValue(true);
	cb_ChCKeywords->SetValue(true);
	cb_ChCOtherItems->SetValue(false);
	rb_ChCAllLower->SetValue(true);

	if (!Manager::Get()->GetEditorManager())
        return;
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (!ed)
        return;
    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return;
    if (control->GetSelectedText().IsEmpty())
        rb_ChCSelection->Disable();
    else
        rb_ChCSelection->SetValue(true);
}

ChangeCase::~ChangeCase()
{
	//(*Destroy(ChangeCase)
	//*)
}

void ChangeCase::OnOK(cb_unused wxCommandEvent& event)
{
    Manager::Get()->GetLogManager()->DebugLog("ChangeCase::OnOK is called");

    ChangeCaseIn chin;
    if (rb_ChCActiveProject->GetValue())
        chin = chciProject;
    else if (rb_ChCCurrentFile->GetValue())
        chin = chciFile;
    else
        chin = chciSelection;

    int chfor = 0;
    if (cb_ChCKeywords->GetValue())
        chfor = chcfKeywords;
    if (cb_ChCOtherItems->GetValue())
        chfor = chfor | chcfOther;

    ChangeCaseTo chto;
    if (rb_ChCAllCaps->GetValue())
        chto = chctAllCaps;
    else if (rb_ChCFirstCap->GetValue())
        chto = chctFirstCap;
    else
        chto = chctAllLower;

    MakeChangeCase(chin, chfor, chto);

    EndModal(wxID_OK);
}

void ChangeCase::MakeChangeCase(ChangeCaseIn chin, int chfor, ChangeCaseTo chto)
{
    if (!Manager::Get()->GetEditorManager())
        return;

    if (chin == chciProject)
    {
        cbProject* project = Manager::Get()->GetProjectManager()->GetActiveProject();
        if (!project)
            return;

        wxArrayString nonFFiles;
        for (FilesList::iterator it = project->GetFilesList().begin(); it != project->GetFilesList().end(); ++it)
        {
            ProjectFile* pf = *it;
            FortranSourceForm fsForm;
            if (g_FortranFileExt.IsFileFortran(pf->file.GetFullPath(), fsForm))
                FileChangeCase(pf->file.GetFullPath(), chin, chfor, chto);
            else
                nonFFiles.Add(pf->file.GetFullName());
        }

        if (nonFFiles.size() > 0)
        {
            wxString mstr;
            if (nonFFiles.size() == 1)
            {
                mstr = wxString::Format(_("File \"%s\" was not recognized as a Fortran file."), nonFFiles[0]);
                mstr << _(" The change-case was not applied for it.");
            }
            else
            {
                mstr = _("Files");
                size_t i=0;
                size_t imax=5;
                while (i < nonFFiles.size() && i < imax)
                {
                    mstr << "\n\"" << nonFFiles[i] << "\"";
                    i++;
                }
                if (nonFFiles.size() > imax)
                    mstr << "...\n";
                else
                    mstr << "\n";
                mstr << wxString::Format(_("(%zu files) were not recognized as the Fortran files."), nonFFiles.size());
                mstr << _(" The change-case was not applied for them.");
                cbMessageBox(mstr, _("Info"), wxICON_INFORMATION);
            }
        }

    }
    else if (chin == chciFile || chin == chciSelection)
    {
        cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
        if (!ed)
            return;
        FileChangeCase(ed->GetFilename(), chin, chfor, chto);
    }
}

void ChangeCase::FileChangeCase(wxString filename, ChangeCaseIn chin, int chfor, ChangeCaseTo chto)
{
    if (!Manager::Get()->GetEditorManager())
        return;

    cbEditor* ed = Manager::Get()->GetEditorManager()->IsBuiltinOpen(filename);

    if (ed) // File is already opened
        EditorChangeCase(ed, chin, chfor, chto);
    else
    {
        // File is not opened.
        ed = Manager::Get()->GetEditorManager()->Open( filename );
        if (ed)
        {
            bool changed = EditorChangeCase(ed, chin, chfor, chto);
            if ( !changed ) // We opened a file and it didn't change.  Close it.
                Manager::Get()->GetEditorManager()->Close( filename );
        }
    }
}

bool ChangeCase::EditorChangeCase(cbEditor* ed, ChangeCaseIn chin, int chfor, ChangeCaseTo chto)
{
    if (!ed)
        return false;

    cbStyledTextCtrl* control = ed->GetControl();
    if (!control)
        return false;

    if (control->GetReadOnly())
    {
        cbMessageBox(_("The file is read-only!"), _("Error"), wxICON_ERROR);
        return false;
    }

    std::set<wxString> kwset;

    EditorColourSet* theme = ed->GetColourSet();
    if (!theme)
        return false;
    HighlightLanguage lang = "Fortran";

    for (int i = 0; i <= wxSCI_KEYWORDSET_MAX; ++i)
    {
        wxString keywords = theme->GetKeywords(lang, i);
        wxStringTokenizer tkz(keywords, " \t\r\n", wxTOKEN_STRTOK);
        while (tkz.HasMoreTokens())
        {
            kwset.insert(tkz.GetNextToken().Lower());
        }
    }
    FortranSourceForm fsForm = fsfFree;
    if (!g_FortranFileExt.IsFileFortran(ed->GetFilename(), fsForm))
    {
        if (cbMessageBox(wxString::Format(_("Are you sure '%s' is a Fortran Source File?\nContinue to change-case?"), ed->GetFilename()),
            _("Question"),
            wxICON_QUESTION | wxYES_NO | wxNO_DEFAULT ) == wxID_NO)
            return false;
    }

    wxString allText;
    if (chin == chciSelection)
    {
        allText = control->GetSelectedText();
        if (allText.IsEmpty())
            return false;
    }
    else
        allText = control->GetText();

    TextCutter cutter(allText, fsForm);
    wxString allTextNew;

    while (1)
	{
	    wxString tok;
	    bool isWord;
		cutter.GetChunk(tok, isWord);
		if (tok.IsEmpty())
			break;

        if (!isWord)
            allTextNew.Append(tok);
        else
        {
            wxString tok_low = tok.Lower();
            bool changeCase = false;

            if (kwset.count(tok_low) == 1)
            {
                if (chfor & chcfKeywords)
                    changeCase = true;
            }
            else
            {
                if (chfor & chcfOther)
                    changeCase = true;
            }

            if (changeCase)
            {
                wxString tokCase;
                if (chto == chctAllCaps)
                    tokCase = tok_low.Upper();
                else if (chto == chctFirstCap)
                    tokCase = tok_low.Mid(0,1).Upper() + tok_low.Mid(1);
                else // if (chto == chctAllLower)
                    tokCase = tok_low;
                allTextNew.Append(tokCase);
            }
            else
                allTextNew.Append(tok);
        }
	}
    if (!allText.IsSameAs(allTextNew))
    {
        if (chin == chciSelection)
            control->ReplaceSelection(allTextNew);
        else
            control->SetText(allTextNew);
        return true;
    }
    return false;
}

