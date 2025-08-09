
#include "formatindent.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <projectmanager.h>
    #include <cbproject.h>
    #include <editormanager.h>
    #include <configmanager.h>
    #include "cbeditor.h"
#endif
#include <vector>

#include <wx/tokenzr.h>

#include <cbstyledtextctrl.h>

#include "formatindentdlg.h"
#include "fortranfileext.h"

extern FortranFileExt g_FortranFileExt;

// constructor
FormatIndent::FormatIndent()
{
}

// destructor
FormatIndent::~FormatIndent()
{
}

void FormatIndent::Format()
{
    FormatIndentDlg fidlg(Manager::Get()->GetAppWindow());
    PlaceWindow(&fidlg);
    if (fidlg.ShowModal() == wxID_OK)
    {
        FormatIndentDlg::FormatIndentScope scope;
        scope = fidlg.GetFormatScope();
        if (scope == FormatIndentDlg::fisProject)
            FormatProject();
        else if (scope == FormatIndentDlg::fisCurrentFile)
            FormatActiveFile();
        else
            FormatSelection();
    }
}

void FormatIndent::FormatProject()
{
    cbProject* project = Manager::Get()->GetProjectManager()->GetActiveProject();
    if (!project)
    {
        cbMessageBox(_("No active project was found!"), _("Error"), wxICON_ERROR);
        return;
    }

    wxArrayString nonFFiles;
    for (FilesList::iterator it = project->GetFilesList().begin(); it != project->GetFilesList().end(); ++it)
    {
        ProjectFile* pf = *it;
        FortranSourceForm fsForm;
        bool isF = g_FortranFileExt.IsFileFortran(pf->file.GetFullPath(), fsForm);
        if (isF && fsForm == fsfFree)
            FormatFile(pf->file.GetFullPath());
        else
            nonFFiles.Add(pf->file.GetFullName());
    }

    if (nonFFiles.size() > 0)
    {
        wxString mstr;
        if (nonFFiles.size() == 1)
        {
            mstr = wxString::Format(_("File \"%s\" was not recognized as a free-form Fortran file."), nonFFiles[0]);
            mstr << _(" The indent formating was not applied for it.");
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
            mstr << wxString::Format(_("(%zu files) were not recognized as the free-form Fortran files."), nonFFiles.size());
            mstr << _(" The indent formating was not applied for them.");
            cbMessageBox(mstr, _("Info"), wxICON_INFORMATION);
        }
    }
}

void FormatIndent::FormatActiveFile()
{
	if (!Manager::Get()->GetEditorManager())
        return;
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (!ed)
        return;
    cbStyledTextCtrl* control = ed->GetControl();
    if( wxSCI_LEX_FORTRAN != control->GetLexer() )
	{
        if( cbMessageBox( "Are you sure \n" + ed->GetFilename() +
            "\nis a Fortran Free Format Source File?\nContinue to Format the Indent?", _("Error Message"),
            wxICON_QUESTION | wxYES_NO | wxNO_DEFAULT ) != wxID_YES )
            return;
	}

    FormatFile(ed->GetFilename());
}

void FormatIndent::FormatFile(const wxString &filename)
{
    cbEditor* ed = Manager::Get()->GetEditorManager()->IsBuiltinOpen(filename);
    bool wasOpened = true;

    if (!ed)
    {
        // File is not open.  We must open it.
        ed = Manager::Get()->GetEditorManager()->Open(filename);
        if (!ed)
            return;
        wasOpened = false;
    }

    cbStyledTextCtrl* control = ed->GetControl();
    if (control->GetReadOnly())
    {
        cbMessageBox(_("The file is read-only!"), _("Error"), wxICON_ERROR);
        return;
    }

    wxString eolChars = GetEOLChars(control);

    ReadConfig();
    const int pos_cur = control->GetCurrentPos();
    wxString text = control->GetText();
    wxString formattedText;
    FormatText(text, 0, eolChars, formattedText);

    bool changed = m_IndentEstimator.BuffersDiffer(formattedText, text);
    if (changed)
    {
        ReplaceTextInEditor(formattedText, false, control);
        control->GotoPos(pos_cur);
    }
    else if (!changed && !wasOpened)
        Manager::Get()->GetEditorManager()->Close(filename);

}

void FormatIndent::FormatSelection()
{
    if (!Manager::Get()->GetEditorManager())
        return;
    cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
    if (!ed)
        return;
    cbStyledTextCtrl* control = ed->GetControl();
    if (control->GetReadOnly())
    {
        cbMessageBox(_("The file is read-only!"), _("Error"), wxICON_ERROR);
        return;
    }

    ReadConfig();
    int pos_selStart = control->GetSelectionStart();
    int pos_selEnd = control->GetSelectionEnd();
    int indexLineStart = 0, indexLineEnd = 0;
    int nLines = control->GetLineCount();

    if( pos_selStart != pos_selEnd )
    {
        indexLineStart = control->LineFromPosition( pos_selStart );
        control->GotoLine( indexLineStart );
        pos_selStart = control->GetCurrentPos();
        indexLineEnd = control->LineFromPosition( pos_selEnd );
        if( indexLineEnd == nLines )
        {
            control->GotoLine(indexLineEnd);
            control->LineEnd();
        }
        else
        {
            control->GotoLine(indexLineEnd + 1);
        }
        pos_selEnd = control->GetCurrentPos();
        control->SetSelectionStart(pos_selStart);
        control->SetSelectionEnd(pos_selEnd);
        wxString text = control->GetTextRange(pos_selStart, pos_selEnd);
        wxString eolChars = GetEOLChars(control);

        nLines = indexLineEnd + 1;
        int indentW = ed->GetLineIndentInSpaces( indexLineStart );
        int tabW = control->GetTabWidth();
        int indentNum = indentW / tabW;
        if( (indentW % tabW) > 0 )
            indentNum++;

        wxString formattedText;
        FormatText(text, indentNum, eolChars, formattedText);

        bool changed = m_IndentEstimator.BuffersDiffer(formattedText, text);
        if (changed)
        {
            ReplaceTextInEditor(formattedText, true, control);
        }
    }
}

void FormatIndent::ReadConfig()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");
    if (cfg->ReadBool("/indent_same_as_editor", true))
    {
        cbEditor* ed = Manager::Get()->GetEditorManager()->GetBuiltinActiveEditor();
        if (!ed)
            return;
        cbStyledTextCtrl* control = ed->GetControl();
        if (!control)
            return;
        if (control->GetUseTabs())
            m_IndentStr = "\t";
        else
        {
            int tabWidth = control->GetTabWidth();
            m_IndentStr = wxString(' ', tabWidth);
        }
    }
    else
    {
        if (cfg->ReadBool("/indent_use_tabs", false))
            m_IndentStr = "\t";
        else
        {
            int tabWidth = cfg->ReadInt("/indent_tab_width", 4);
            m_IndentStr = wxString(' ', tabWidth);
        }
    }
    m_TrimFromRight = cfg->ReadBool("/indent_trim_right", true);

    m_IndentEstimator.ReadConfig();
}

void FormatIndent::FormatText(const wxString& textIn, int indentStart, const wxString& eolChars, wxString& formattedText)
{
    wxString str = textIn;
    str.Replace("\r\n", "\n");
    str.Replace("\r", "\n");
    wxStringTokenizer tokenizer(str, "\n", wxTOKEN_RET_EMPTY_ALL);
    std::vector<wxString> textLines;
    while (tokenizer.HasMoreTokens())
        textLines.push_back(tokenizer.GetNextToken());

    int indentNum = indentStart;
    int nLines = textLines.size();


    m_IndentEstimator.Initialize(indentNum);

    IsMultiLines isMultiLines;
    wxString tmpLine;
    wxString tmpMultiLines;

    int indentNumNext;

    ///formattedText
    for(int i=0; i < nLines; ++i)
    {
        tmpLine = textLines[i];

        if(m_IndentEstimator.GetIsHasPreprocessor(tmpLine))
        {
            if (!isMultiLines.haveMultiLines)
            {
                if(m_TrimFromRight)
                    tmpLine.Trim(); //trim from right
                if( i < nLines-1 )
                    tmpLine += eolChars;
                formattedText += tmpLine;
            }
            continue;
        }

        tmpLine = tmpLine.Trim(false); // trim from left
        if(m_IndentEstimator.GetIsHasLineContinuation(tmpLine))
        {
            wxString tempLine = tmpLine;

            if(!isMultiLines.haveMultiLines)
            {
                isMultiLines.haveMultiLines = true;
                isMultiLines.iFirstLineNo = i;
                tmpMultiLines.Empty();
            }

            m_IndentEstimator.CutStringAndComment(tempLine);
            m_IndentEstimator.DelLineContinuation(tempLine);

            if(tempLine.Len() > 0)
            {
                if('&' == tempLine[0])
                    tempLine = tempLine.Mid(1);
            }
            tmpMultiLines += tempLine;
            continue ;
        }

        indentNumNext = indentNum;

        if(isMultiLines.haveMultiLines)
        {
            isMultiLines.iEndLineNo = i;

            wxString tempLine = tmpLine;

            if(tempLine.Len() > 0)
            {
                if('&' == tempLine[0])
                    tempLine = tempLine.Mid(1);
            }

            tmpMultiLines += tempLine;
            m_IndentEstimator.GetFortranIndentLine(tmpMultiLines, indentNum, indentNumNext);

            for(int j = isMultiLines.iFirstLineNo; j <= isMultiLines.iEndLineNo; ++j)
            {
                if(m_IndentEstimator.GetIsHasPreprocessor(textLines[j]))
                {
                    tempLine = textLines[j];
                    if(m_TrimFromRight)
                        tempLine.Trim(); //trim from right
                    if(j < nLines-1)
                        tempLine += eolChars;
                    formattedText += tempLine;
                }
                else
                {
                    tempLine = textLines[j].Trim(false); // trim from left

                    if(m_TrimFromRight)
                        tempLine.Trim(); //trim from right
                    if(j < nLines-1)
                        tempLine += eolChars;

                    if(j != isMultiLines.iFirstLineNo)
                        indentNum += 1;

                    for(int k=0; k < indentNum; ++k)
                        formattedText += m_IndentStr;

                    formattedText += tempLine;
                    indentNum = indentNumNext;
                }
            }

            isMultiLines.reset();
        }
        else
        {
            if(tmpLine.Len() == 0 && i < nLines-1)
                tmpLine += eolChars;
            else
            {
                if(m_TrimFromRight)
                {
                    tmpLine.Trim(); //trim from right
                }
                if(i < nLines-1)
                    tmpLine += eolChars;
                m_IndentEstimator.GetFortranIndentLine(tmpLine, indentNum, indentNumNext);

                for(int k = 0; k < indentNum; ++k)
                    formattedText += m_IndentStr;
            }

            formattedText += tmpLine;
            indentNum = indentNumNext;
        }
    }
}

void FormatIndent::ReplaceTextInEditor(const wxString& text, bool isSelection, cbStyledTextCtrl* control)
{
    if (isSelection)
        control->ReplaceSelection(text);
    else
        control->SetText(text);
}

wxString FormatIndent::GetEOLChars(cbStyledTextCtrl* control)
{
    wxString eolChars;
    switch (control->GetEOLMode())
    {
        case wxSCI_EOL_CRLF:
            eolChars = "\r\n";
            break;

        case wxSCI_EOL_CR:
            eolChars = "\r";
            break;

        case wxSCI_EOL_LF:
            eolChars = "\n";
            break;
    }
    return eolChars;
}
