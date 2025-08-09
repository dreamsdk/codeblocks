
#include "tab2space.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <cbeditor.h>
    #include <cbproject.h>
    #include <editormanager.h>
    #include <logmanager.h>
    #include <projectmanager.h>
#endif
#include <set>

#include <wx/tokenzr.h>

#include <cbstyledtextctrl.h>

#include "fortranfileext.h"
#include "textcutter.h"

extern FortranFileExt g_FortranFileExt;

//(*InternalHeaders(Tab2Space)
#include <wx/xrc/xmlres.h>
//*)

//(*IdInit(Tab2Space)
//*)

BEGIN_EVENT_TABLE(Tab2Space,wxScrollingDialog)
	//(*EventTable(Tab2Space)
	//*)
	EVT_BUTTON  (wxID_OK,   Tab2Space::OnOK)
END_EVENT_TABLE()

Tab2Space::Tab2Space(wxWindow* parent)
{
	//(*Initialize(Tab2Space)
	wxXmlResource::Get()->LoadObject(this,parent,_T("Tab2Space"),_T("wxScrollingDialog"));
	StaticText1 = (wxStaticText*)FindWindow(XRCID("ID_STATICTEXT1"));
	rb_ChCActiveProject = (wxRadioButton*)FindWindow(XRCID("ID_CHCACTIVEPROJECT"));
	rb_ChCCurrentFile = (wxRadioButton*)FindWindow(XRCID("ID_CHCCURRENTFILE"));
	rb_ChCSelection = (wxRadioButton*)FindWindow(XRCID("ID_CHCSELECTION"));
	StaticText2 = (wxStaticText*)FindWindow(XRCID("ID_STATICTEXT2"));
	sc_TabSize = (wxSpinCtrl*)FindWindow(XRCID("ID_SPINCTRL1"));
	//*)

	rb_ChCCurrentFile->SetValue(true);

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

    int tabW = control->GetTabWidth();
    if (tabW <= 0) tabW = 4;

	sc_TabSize->SetValue(tabW);
}

Tab2Space::~Tab2Space()
{
	//(*Destroy(Tab2Space)
	//*)
}

void Tab2Space::OnOK(cb_unused wxCommandEvent& event)
{
    Manager::Get()->GetLogManager()->DebugLog("Tab2Space::OnOK is called");

    Tab2SpaceIn chin;
    if (rb_ChCActiveProject->GetValue())
        chin = t2siProject;
    else if (rb_ChCCurrentFile->GetValue())
        chin = t2siFile;
    else
        chin = t2siSelection;

    int tabSize = sc_TabSize->GetValue();
    if (tabSize <= 0)
        tabSize = 4;

    MakeTab2Space(chin, tabSize);

    EndModal(wxID_OK);
}

void Tab2Space::MakeTab2Space(Tab2SpaceIn chin, int tabSize)
{
    if (!Manager::Get()->GetEditorManager())
        return;

    if (chin == t2siProject)
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
                FileTab2Space(pf->file.GetFullPath(), chin, tabSize);
            else
                nonFFiles.Add(pf->file.GetFullName());
        }

        if (nonFFiles.size() > 0)
        {
            wxString mstr;
            if (nonFFiles.size() == 1)
            {
                mstr = wxString::Format(_("File \"%s\" was not recognized as a Fortran file."), nonFFiles[0]);
                mstr << _(" The tab2space was not applied for it.");
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
                mstr << _(" The tab2space was not applied for them.");
                cbMessageBox(mstr, _("Info"), wxICON_INFORMATION);
            }
        }

    }
    else if (chin == t2siFile || chin == t2siSelection)
    {
        cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
        if (!ed)
            return;
        FileTab2Space(ed->GetFilename(), chin, tabSize);
    }
}

void Tab2Space::FileTab2Space(wxString filename, Tab2SpaceIn chin, int tabSize)
{
    if (!Manager::Get()->GetEditorManager())
        return;

    cbEditor* ed = Manager::Get()->GetEditorManager()->IsBuiltinOpen(filename);

    if (ed) // File is already opened
        EditorTab2Space(ed, chin, tabSize);
    else
    {
        // File is not opened.
        ed = Manager::Get()->GetEditorManager()->Open( filename );
        if (ed)
        {
            bool changed = EditorTab2Space(ed, chin, tabSize);
            if ( !changed ) // We opened a file and it didn't change.  Close it.
                Manager::Get()->GetEditorManager()->Close( filename );
        }
    }
}

bool Tab2Space::EditorTab2Space(cbEditor* ed, Tab2SpaceIn chin, int tabSize)
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

    FortranSourceForm fsForm;
    if (!g_FortranFileExt.IsFileFortran(ed->GetFilename(), fsForm))
    {
        cbMessageBox(wxString::Format(_("The file \n%s\n is not recognized as a Fortran Source File."), ed->GetFilename()), _("Info"),
            wxICON_INFORMATION);
        return false;
    }

    wxString allText;
    if (chin == t2siSelection)
    {
        allText = control->GetSelectedText();
        if (allText.IsEmpty())
            return false;
    }
    else
        allText = control->GetText();

    wxString allTextNew;

    if (allText.size() == 0)
        return false;

    wxString contDigit = "123456789";

    size_t ncur = 0;
    while (1)
	{
	    wxString line;
	    size_t ncur_old = ncur;
	    while (ncur < allText.size())
        {
            if (allText.GetChar(ncur) == '\n' || ncur == (allText.size()-1))
            {
                ncur++;
                line = allText.Mid(ncur_old, ncur-ncur_old);
                break;
            }
            ncur++;
        }

        if (line.size() == 0)
            break;

        wxString lineNew;

        if (fsForm == fsfFixed)
        {
            if (line.GetChar(0) == '\t' && contDigit.Find(line.GetChar(1)) != wxNOT_FOUND)
                lineNew << "     ";
            else if (line.GetChar(0) == '\t')
                lineNew << "      ";
            else if (line.GetChar(0) == '\n')
                lineNew << "\n";
            else
                lineNew << line.GetChar(0);
        }
        else
        {
            if (line.GetChar(0) == '\t')
                lineNew.Append(' ',tabSize);
            else
                lineNew << line.GetChar(0);
        }

        for (size_t i=1; i<line.size(); i++)
        {
            if (line.GetChar(i) == '\t')
            {
                int nspace;
                if ((lineNew.size() >= 6) || (fsForm == fsfFree))
                    nspace = tabSize - lineNew.size()%tabSize;
                else
                    nspace = 6 - lineNew.size();

                lineNew.Append(' ', nspace);
            }
            else
            {
                lineNew << line.GetChar(i);
            }
        }
        allTextNew.Append(lineNew);

        if (ncur >= allText.size())
            break;
	}

    if (!allText.IsSameAs(allTextNew))
    {
        if (chin == t2siSelection)
            control->ReplaceSelection(allTextNew);
        else
            control->SetText(allTextNew);
        return true;
    }
    return false;
}

