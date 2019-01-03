#include "fortranfileext.h"
#include <configmanager.h>
#include <wx/tokenzr.h>

#include <editormanager.h>
#include <logmanager.h>


FortranFileExt::FortranFileExt()
{
    m_ExtDone = false;
}

FortranFileExt::~FortranFileExt()
{
    //dtor
}

bool FortranFileExt::IsFileFortran(const wxString& filename, FortranSourceForm& fsForm)
{
    if (!m_ExtDone)
    {
        RereadFileExtensions();
        m_ExtDone = true;
    }

    bool isf;
    wxString ext = filename.AfterLast(_T('.')).Lower();

    if (m_FortranExtFree.count(ext))
    {
        fsForm = fsfFree;
        isf = true;
    }
    else if (m_FortranExtFixed.count(ext))
    {
        fsForm = fsfFixed;
        isf = true;
    }
    else
        isf = false;

   return isf;
}

void FortranFileExt::RereadFileExtensions()
{
    EditorColourSet* colSet = Manager::Get()->GetEditorManager()->GetColourSet();
    if (colSet)
        colSet = new EditorColourSet(*colSet);
    else
        colSet = new EditorColourSet();

    RereadFileExtensionsLang(colSet, _T("fortran77"), m_FortranExtFixed);
    RereadFileExtensionsLang(colSet, _T("fortran"), m_FortranExtFree);
}

void FortranFileExt::RereadFileExtensionsLang(EditorColourSet* colSet, wxString langName, StringSet& extSet)
{
    extSet.clear();
    HighlightLanguage lang = colSet->GetHighlightLanguage(langName);
    const wxArrayString& fileMasks = colSet->GetFileMasks(lang);

    wxString exts;
    for (size_t i=0; i < fileMasks.GetCount(); i++)
    {
        exts << _T(" ") + fileMasks[i];
    }

    //Manager::Get()->GetLogManager()->DebugLog(_T("FortranProject ")+langName+_T("=")+exts);

    wxStringTokenizer tkz(exts, _T(" ;,*.\t\r\n"), wxTOKEN_STRTOK);
    while ( tkz.HasMoreTokens() )
    {
        wxString token = tkz.GetNextToken();
        extSet.insert(token.Lower());
    }
}

void FortranFileExt::RereadOptions()
{
    m_ExtDone = false;
}

