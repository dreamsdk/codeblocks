#ifndef INCLUDEDB_H
#define INCLUDEDB_H

//#include "projectdependencies.h"

#include <wx/string.h>
#include <wx/arrstr.h>
#include <map>
#include <set>

typedef std::set<wxString> StringSet;
typedef std::map<wxString,StringSet*> StringStringSetPMap;

class IncludeFile
{
    public:
        IncludeFile();
        ~IncludeFile();

    private:
        wxString  m_Filename;
        StringSet m_MyParentFiles;
};

class IncludeDB
{
    public:
        IncludeDB();
        ~IncludeDB();

        void SetInclude(const wxString& parentFilename, const wxArrayString& include);
        void RemoveFile(const wxString& name);
        bool IsIncludeFile(const wxString& name);
        void Clear();
        bool IsEmpty();
        wxString GetOneParentFile(const wxString& name);

    private:
        StringStringSetPMap m_IncludeFiles;
};

#endif // INCLUDEDB_H
