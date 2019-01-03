#include "includedb.h"

IncludeDB::IncludeDB()
{
    //ctor
}

IncludeDB::~IncludeDB()
{
    //dtor
    Clear();
}

//void IncludeDB::SetInclude(const wxString& parentFilename, const wxArrayString& include)
//{
//    for (size_t i=0; i<include.size(); i++)
//    {
//        StringStringSetPMap::iterator it = m_IncludeFiles.find(include[i]);
//        if (it == m_IncludeFiles.end())
//        {
//            StringSet* parSet = new StringSet();
//            m_IncludeFiles[include[i]] = parSet;
//        }
//    }
//
//    StringStringSetPMap::iterator it = m_IncludeFiles.begin();
//    while (it != m_IncludeFiles.end())
//    {
//        bool foundIncl = false;
//        for (size_t i=0; i<include.size(); i++)
//        {
//            if ((*it).first.IsSameAs(include[i]))
//            {
//                (*it).second->insert(parentFilename);
//                foundIncl = true;
//                break;
//            }
//        }
//        if (!foundIncl)
//        {
//            (*it).second->erase(parentFilename);
//            if ((*it).second->size() == 0)
//            {
//                delete (*it).second;
//                m_IncludeFiles.erase(it++);
//            }
//            else
//                ++it;
//        }
//        else
//            ++it;
//    }
//}

void IncludeDB::SetInclude(const wxString& parentFilename, const wxArrayString& include)
{
    for (size_t i=0; i<include.size(); i++)
    {
        StringStringSetPMap::iterator it = m_IncludeFiles.find(include[i]);
        if (it == m_IncludeFiles.end())
        {
            StringSet* parSet = new StringSet();
            parSet->insert(parentFilename);
            m_IncludeFiles[include[i]] = parSet;
        }
        else
            (*it).second->insert(parentFilename);
    }
}

void IncludeDB::RemoveFile(const wxString& name)
{
    StringStringSetPMap::iterator it = m_IncludeFiles.begin();
    while (it != m_IncludeFiles.end())
    {
        (*it).second->erase(name);
        if ((*it).second->size() == 0)
        {
            delete (*it).second;
            m_IncludeFiles.erase(it++);
        }
        else
            ++it;
    }
}

bool IncludeDB::IsIncludeFile(const wxString& name)
{
    if (m_IncludeFiles.count(name) == 1)
        return true;
    return false;
}

void IncludeDB::Clear()
{
    StringStringSetPMap::iterator it = m_IncludeFiles.begin();
    while (it != m_IncludeFiles.end())
    {
        delete (*it).second;
        m_IncludeFiles.erase(it++);
    }
}

bool IncludeDB::IsEmpty()
{
    return m_IncludeFiles.empty();
}

wxString IncludeDB::GetOneParentFile(const wxString& name)
{
    if (m_IncludeFiles.count(name) == 0)
        return wxEmptyString;

    StringSet* parSet = m_IncludeFiles[name];
    if (parSet->size() > 0)
        return *(parSet->begin());

    return wxEmptyString;
}
