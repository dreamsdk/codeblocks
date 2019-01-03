#ifndef USETOKENF_H
#define USETOKENF_H

#include "tokenf.h"
#include <wx/arrstr.h>
#include <set>
#include <list>

enum ModuleNature
{
    mnIntrinsic = 1,
    mnNonIntrinsic
};

class UseTokenF : public TokenF
{
    public:
        UseTokenF();
        UseTokenF(const wxString& name, const wxString& filename, unsigned int line);
        virtual ~UseTokenF();
        void SetOnly(bool hasOnly);
        void SetModuleNature(ModuleNature modNature);
        void AddToNamesList(wxString& localName);
        void AddToRenameList(wxString& localName, wxString& externalName);

        ModuleNature GetModuleNature() {return m_ModuleNature;};
        bool HasOnly() {return m_HasOnly;};
        std::set<wxString>* GetNamesList() {return &m_NamesList;};
        std::list<wxArrayString>* GetRenameList() {return &m_RenameList;};
    //protected:
    private:
        ModuleNature m_ModuleNature;
        bool m_HasOnly;
        std::set<wxString> m_NamesList;
        std::list<wxArrayString> m_RenameList;
};

#endif // USETOKENF_H
