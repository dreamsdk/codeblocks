#include "usetokenf.h"

UseTokenF::UseTokenF():
    TokenF()
{
    m_HasOnly = false;
}

UseTokenF::UseTokenF(const wxString& name, const wxString& filename, unsigned int line):
    TokenF(name, filename, line)
{
    m_HasOnly = false;
}

UseTokenF::~UseTokenF()
{
    //dtor
}

void UseTokenF::SetOnly(bool hasOnly)
{
    m_HasOnly = hasOnly;
}

void UseTokenF::SetModuleNature(ModuleNature modNature)
{
    m_ModuleNature = modNature;
}

void UseTokenF::AddToNamesList(wxString& localName)
{
    m_NamesList.insert(localName.Lower());
}

void UseTokenF::AddToRenameList(wxString& localName, wxString& externalName)
{
    wxArrayString pair;
    pair.Add(localName);
    pair.Add(externalName.Lower());
    m_RenameList.push_back(pair);
}


