
#ifndef FORTRANFILEEXT_H
#define FORTRANFILEEXT_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <editorcolourset.h>
#endif

#include "tokenizerf.h"
#include "includedb.h"

class FortranFileExt
{
	public:
	    FortranFileExt();
        ~FortranFileExt();
		bool IsFileFortran(const wxString& filename, FortranSourceForm& fsForm);
		void RereadOptions();
    private:
		void RereadFileExtensions();
		void RereadFileExtensionsLang(EditorColourSet* colSet, wxString langName, StringSet& extSet);
        bool m_ExtDone;
        StringSet m_FortranExtFree;
        StringSet m_FortranExtFixed;
};

#endif // FORTRANFILEEXT_H
