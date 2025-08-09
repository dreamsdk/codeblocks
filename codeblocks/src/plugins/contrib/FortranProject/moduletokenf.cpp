#include "moduletokenf.h"

ModuleTokenF::ModuleTokenF():
    TokenF(),
    m_DefaultPublic(true)
{
}

ModuleTokenF::~ModuleTokenF()
{
    //dtor
}

void ModuleTokenF::SetDefaultPublic(bool defPublic)
{
    m_DefaultPublic = defPublic;
}

void ModuleTokenF::AddToPrivateList(wxString& privateTName)
{
    if (m_PrivateTList.Index(privateTName) == wxNOT_FOUND)
        m_PrivateTList.Add(privateTName);
}

void ModuleTokenF::AddToPublicList(wxString& publicTName)
{
    if (m_PublicTList.Index(publicTName) == wxNOT_FOUND)
        m_PublicTList.Add(publicTName);
}

bool ModuleTokenF::HasNameInPrivateList(wxString& str)
{
    if (m_PrivateTList.Index(str) != wxNOT_FOUND)
        return true;
    else
        return false;
}

bool ModuleTokenF::HasNameInPublicList(wxString& str)
{
    if (m_PublicTList.Index(str) != wxNOT_FOUND)
        return true;
    else
        return false;
}





