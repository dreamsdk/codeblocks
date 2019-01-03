/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */

#include "projectdependencies.h"
#include <projectmanager.h>
#include <logmanager.h>
#include <compiler.h>
#include <compilerfactory.h>
#include <macrosmanager.h>
#include <wx/filefn.h>
#include <wx/dir.h>

#include "nativeparserf.h"


ProjectDependencies::ProjectDependencies(cbProject* project)
{
    m_Project = project;
    //ctor
}

ProjectDependencies::~ProjectDependencies()
{
    //dtor
    Clear();
}

void ProjectDependencies::Clear()
{
    m_prFilesArr.clear();
    m_FileIndexMap.clear();
    for (size_t i=0; i<m_pUseModules.size(); i++)
    {
        delete m_pUseModules[i];
    }
    m_pUseModules.clear();
    for (size_t i=0; i<m_pDeclaredModules.size(); i++)
    {
        delete m_pDeclaredModules[i];
    }
    m_pDeclaredModules.clear();
    for (size_t i=0; i<m_pExtendsSModules.size(); i++)
    {
        delete m_pExtendsSModules[i];
    }
    m_pExtendsSModules.clear();
    for (size_t i=0; i<m_pDeclaredSubmodules.size(); i++)
    {
        delete m_pDeclaredSubmodules[i];
    }
    m_pDeclaredSubmodules.clear();

    for (size_t i=0; i<m_pIncludes.size(); i++)
    {
        delete m_pIncludes[i];
    }
    m_pIncludes.clear();

    m_ModuleFileIdxMap.clear();
    m_SubmoduleFileIdxMap.clear();
    m_IncludeFileIdxMap.clear();

    for (size_t i=0; i<m_ChildrenTable.size(); i++)
    {
        delete m_ChildrenTable[i];
    }
    m_ChildrenTable.clear();
    m_WasInfiniteLoop = false;
    m_FileWeights.Empty();

    m_MadeChildrenSet.clear();
}

void ProjectDependencies::MakeProjectFilesDependencies(ProjectFilesArray& prFilesArr, ParserF& parser)
{
    Clear();

    m_prFilesArr = prFilesArr;
    wxArrayString fnames;

    size_t nfil = m_prFilesArr.size();
	for (size_t i = 0; i < nfil; ++i)
	{
        ProjectFile* pf = m_prFilesArr[i];
        wxString ffp = pf->file.GetFullPath();
        m_FileIndexMap.insert(std::make_pair(ffp,i));

        wxString fname = pf->file.GetName() + _T(".") + pf->file.GetExt();
        fnames.Add(fname);
	}

	for (size_t i = 0; i < nfil; ++i)
	{
        StringSet* fileUseModules = new StringSet;
        StringSet* fileDeclaredModules = new StringSet;
        StringSet* fileExtendsSModules = new StringSet;
        StringSet* fileDeclaredSubmodules = new StringSet;
        StringSet* fileIncludes = new StringSet;

        parser.ObtainUsedDeclaredModules(m_prFilesArr[i]->file.GetFullPath(), fileUseModules, fileDeclaredModules,
                                         fileExtendsSModules, fileDeclaredSubmodules, fileIncludes);
        m_pUseModules.push_back(fileUseModules);
        m_pDeclaredModules.push_back(fileDeclaredModules);
        m_pExtendsSModules.push_back(fileExtendsSModules);
        m_pDeclaredSubmodules.push_back(fileDeclaredSubmodules);
        m_pIncludes.push_back(fileIncludes);

        StringSet::iterator pos;
	    for (pos = fileDeclaredModules->begin(); pos != fileDeclaredModules->end(); ++pos)
	    {
	        if (!m_ModuleFileIdxMap.count(*pos))
                m_ModuleFileIdxMap[*pos] = i;
	    }

	    for (pos = fileDeclaredSubmodules->begin(); pos != fileDeclaredSubmodules->end(); ++pos)
	    {
	        if (!m_SubmoduleFileIdxMap.count(*pos))
                m_SubmoduleFileIdxMap[*pos] = i;
	    }

	    for (pos = fileIncludes->begin(); pos != fileIncludes->end(); ++pos)
	    {
	        wxString inc = *pos;
	        for (size_t j=0; j < nfil; ++j)
	        {
	            wxString fn = fnames.Item(j);
	            if (inc.IsSameAs(fn) || inc.IsSameAs(fn.BeforeLast('.')))
	            {
	                m_IncludeFileIdxMap[*pos] = j;
	                break;
	            }
	        }
	    }
	}

    m_MadeChildrenSet.resize(nfil,false);
    m_ChildrenTable.resize(nfil);

	for (size_t i = 0; i < nfil; ++i)
	{
	    if (m_MadeChildrenSet[i] == false)
        {
            IntSet* children = new IntSet;
            m_Deep = 0;
            m_BreakChain = false;
            MakeFileChildren(children, i);
            m_ChildrenTable[i] = children;
        }
	}

    //PrintChildrenTable();

    m_FileWeights.Alloc(nfil);
    m_FileWeights.Add(-1,nfil);
}


bool ProjectDependencies::HasInfiniteDependences()
{
    return m_WasInfiniteLoop;
}


unsigned short int ProjectDependencies::GetFileWeight(wxString& fileName)
{
    if (!m_FileIndexMap.count(fileName))
        return 50; //error. default value

    m_Deep = 0;
    m_BreakChain = false;
    unsigned short int wt = GetFileWeightByIndex(m_FileIndexMap[fileName]);
    return wt;
}


unsigned short int ProjectDependencies::GetFileWeightByIndex(size_t idx)
{
    if (m_FileWeights[idx] != -1)
        return m_FileWeights[idx];
    if (m_Deep > 99)
    {
        m_WasInfiniteLoop = true;
        m_BreakChain = true;
        return 0;
    }
    else if (m_BreakChain)
    {
        return 0;
    }
    unsigned short int wt;
    unsigned short int wt_max = 0;
    StringSet* fileUseModules = m_pUseModules[idx];
    StringSet::iterator pos;
    for (pos = fileUseModules->begin(); pos != fileUseModules->end(); ++pos)
    {
        if (m_ModuleFileIdxMap.count(*pos) != 1)
        {
            continue;
        }
        else
        {
            size_t fidx = m_ModuleFileIdxMap[*pos];
            if (fidx == idx)
                continue; // module defined and is used in the same file.
            m_Deep++;
            wt = 1 + GetFileWeightByIndex(fidx);
            m_Deep--;

            if (wt > wt_max)
                wt_max = wt;
        }
    }

    StringSet* fileExtendsSModules = m_pExtendsSModules[idx];
    for (pos = fileExtendsSModules->begin(); pos != fileExtendsSModules->end(); ++pos)
    {
        size_t fidx;
        bool found = false;
        if (m_ModuleFileIdxMap.count(*pos) == 1)
        {
            fidx = m_ModuleFileIdxMap[*pos];
            found = true;
        }
        else if (m_SubmoduleFileIdxMap.count(*pos) == 1)
        {
            fidx = m_SubmoduleFileIdxMap[*pos];
            found = true;
        }

        if (!found)
            continue;
        else
        {
            if (fidx == idx)
                continue; // module defined and is used in the same file.
            m_Deep++;
            wt = 1 + GetFileWeightByIndex(fidx);
            m_Deep--;

            if (wt > wt_max)
                wt_max = wt;
        }
    }

    StringSet* fileIncludes = m_pIncludes[idx];
    for (pos = fileIncludes->begin(); pos != fileIncludes->end(); ++pos)
    {
        if (m_IncludeFileIdxMap.count(*pos) != 1)
        {
            continue;
        }
        else
        {
            size_t fidx = m_IncludeFileIdxMap[*pos];
            if (fidx == idx)
                continue; // error, includes self
            m_Deep++;
            wt = 1 + GetFileWeightByIndex(fidx);
            m_Deep--;

            if (wt > wt_max)
                wt_max = wt;
        }
    }
    m_FileWeights[idx] = wt_max;
    return wt_max;
}

void ProjectDependencies::MakeFileChildren(IntSet* children, size_t fileIndex)
{
    if (m_Deep > 99)
    {
        m_BreakChain = true;
        return; // maybe infinite reference loop?
    }
    else if (m_BreakChain)
    {
        return;
    }

    StringSet* fileDeclaredModules = m_pDeclaredModules[fileIndex];
    StringSet::iterator pos;
    for (pos = fileDeclaredModules->begin(); pos != fileDeclaredModules->end(); ++pos)
    {
        wxString modName = *pos;
        for (size_t k=0; k < m_pUseModules.size(); ++k)
        {
            if (fileIndex==k)
                continue; // declared and used in the same file

            if (m_pUseModules[k]->count(modName))
            {
                children->insert(k);

                if (m_MadeChildrenSet[k] == true)
                {
                    children->insert(m_ChildrenTable[k]->begin(),m_ChildrenTable[k]->end());
                }
                else
                {
                    IntSet* childrenNew = new IntSet;
                    m_Deep++;
                    MakeFileChildren(childrenNew, k);
                    m_Deep--;
                    children->insert(childrenNew->begin(),childrenNew->end());
                    m_ChildrenTable[k] = childrenNew;
                    m_MadeChildrenSet[k] = true;
                }
            }
        }

        for (size_t k=0; k < m_pExtendsSModules.size(); ++k)
        {
            if (fileIndex==k)
                continue; // declared and used in the same file

            if (m_pExtendsSModules[k]->count(modName))
            {
                children->insert(k);

                if (m_MadeChildrenSet[k] == true)
                {
                    children->insert(m_ChildrenTable[k]->begin(),m_ChildrenTable[k]->end());
                }
                else
                {
                    IntSet* childrenNew = new IntSet;
                    m_Deep++;
                    MakeFileChildren(childrenNew, k);
                    m_Deep--;
                    children->insert(childrenNew->begin(),childrenNew->end());
                    m_ChildrenTable[k] = childrenNew;
                    m_MadeChildrenSet[k] = true;
                }
            }
        }
    }

    StringSet* fileDeclaredSubmodules = m_pDeclaredSubmodules[fileIndex];
    for (pos = fileDeclaredSubmodules->begin(); pos != fileDeclaredSubmodules->end(); ++pos)
    {
        wxString submodName = *pos;
        for (size_t k=0; k < m_pExtendsSModules.size(); ++k)
        {
            if (fileIndex==k)
                continue; // declared and used in the same file

            if (m_pExtendsSModules[k]->count(submodName))
            {
                children->insert(k);

                if (m_MadeChildrenSet[k] == true)
                {
                    children->insert(m_ChildrenTable[k]->begin(),m_ChildrenTable[k]->end());
                }
                else
                {
                    IntSet* childrenNew = new IntSet;
                    m_Deep++;
                    MakeFileChildren(childrenNew, k);
                    m_Deep--;
                    children->insert(childrenNew->begin(),childrenNew->end());
                    m_ChildrenTable[k] = childrenNew;
                    m_MadeChildrenSet[k] = true;
                }
            }
        }
    }

    ProjectFile* pf = m_prFilesArr[fileIndex];
    wxString fname = pf->file.GetName();
    wxString fnameExt = fname + _T(".") + pf->file.GetExt();
    for (size_t k=0; k < m_pIncludes.size(); ++k)
    {
        if (fileIndex==k)
            continue;

        if (m_pIncludes[k]->count(fname) || m_pIncludes[k]->count(fnameExt))
        {
            children->insert(k);

            if (m_MadeChildrenSet[k] == true)
            {
                children->insert(m_ChildrenTable[k]->begin(),m_ChildrenTable[k]->end());
            }
            else
            {
                IntSet* childrenNew = new IntSet;
                m_Deep++;
                MakeFileChildren(childrenNew, k);
                m_Deep--;
                children->insert(childrenNew->begin(),childrenNew->end());
                m_ChildrenTable[k] = childrenNew;
                m_MadeChildrenSet[k] = true;
            }
        }
    }
}

void ProjectDependencies::EnsureUpToDateObjs()
{
    int btc = m_Project->GetBuildTargetsCount();
    for (int it=0; it<btc; ++it)
    {
        ProjectBuildTarget* bTarget = m_Project->GetBuildTarget(it);
        Compiler* compiler = CompilerFactory::GetCompiler(bTarget->GetCompilerID());
        if(!compiler)
            continue;
        for (size_t j=0; j<m_prFilesArr.size(); ++j)
        {
            ProjectFile* pf = m_prFilesArr[j];

            const wxArrayString& btarr = pf->GetBuildTargets();
            if (btarr.Index(bTarget->GetTitle()) == wxNOT_FOUND)
                continue;

            const pfDetails& pfd = pf->GetFileDetails(bTarget);
            wxString objectAbs = (compiler->GetSwitches().UseFlatObjects)?pfd.object_file_flat_absolute_native:pfd.object_file_absolute_native;

            time_t time_src = wxFileModificationTime(pfd.source_file_absolute_native);
            bool delChildren = false;
            bool onlyOlder = false;
            if (wxFileExists(objectAbs))
            {
                time_t time_obj = wxFileModificationTime(objectAbs);
                if (time_src > time_obj)
                {
                    delChildren = true;
                }
            }
            else
            {
                delChildren = true;
                if(!pf->compile)
                {
                    onlyOlder = true;
                }
            }

            if (delChildren)
            {
                IntSet* children = m_ChildrenTable[j];
                IntSet::iterator pos;
                for (pos=children->begin(); pos != children->end(); ++pos)
                {
                    ProjectFile* pfChild = m_prFilesArr[*pos];

                    const wxArrayString& btChild_arr = pfChild->GetBuildTargets();
                    if (btChild_arr.Index(bTarget->GetTitle()) == wxNOT_FOUND)
                        continue;

                    const pfDetails& pfdChild = pfChild->GetFileDetails(bTarget);
                    wxString objectAbsChild = (compiler->GetSwitches().UseFlatObjects)?pfdChild.object_file_flat_absolute_native:pfdChild.object_file_absolute_native;

                    if (wxFileExists(objectAbsChild))
                    {
                        if (onlyOlder)
                        {
                            time_t time_obj = wxFileModificationTime(objectAbsChild);
                            if (time_src > time_obj)
                            {
                                wxRemoveFile(objectAbsChild);
                            }
                        }
                        else
                        {
                            wxRemoveFile(objectAbsChild);
                        }
                    }
                }
            }
        }
    }
}

void ProjectDependencies::RemoveModFiles(cbProject* pr, ProjectBuildTarget* bTarget, NativeParserF* nativeParser)
{
    //Remove all *.mod files
    if (!pr || !bTarget || pr->IsMakefileCustom())
        return;

    wxString comID = bTarget->GetCompilerID();
    if (!CompilerFactory::CompilerInheritsFrom(comID, _T("gfortran")) &&
        !CompilerFactory::CompilerInheritsFrom(comID, _T("g95")) &&
        !CompilerFactory::CompilerInheritsFrom(comID, _T("ifcwin")) &&
        !CompilerFactory::CompilerInheritsFrom(comID, _T("ifclin")) &&
        !CompilerFactory::CompilerInheritsFrom(comID, _T("pgfortran")) &&
        !CompilerFactory::CompilerInheritsFrom(comID, _T("oracfortran")) )
    {
        bool haveFortran = false;
        for (FilesList::iterator it = pr->GetFilesList().begin(); it != pr->GetFilesList().end(); ++it)
        {
            ProjectFile* prjfile = *it;
            if (nativeParser->IsFileFortran(prjfile->file.GetFullPath()))
            {
                haveFortran = true;
                break;
            }
        }
        if (!haveFortran)
            return;
    }

    wxString objDir = bTarget->GetObjectOutput();
    wxDir odir;
    if (odir.Open(objDir))
    {
        wxString filename;
        wxFileName fname;
        fname.AssignDir(objDir);
        wxString filespec = _T("*.mod");
        bool cont = odir.GetFirst(&filename, filespec, wxDIR_FILES);
        while (cont)
        {
            fname.SetFullName(filename);
            wxRemoveFile(fname.GetFullPath());
            cont = odir.GetNext(&filename);
        }

        filespec = _T("*.smod");
        cont = odir.GetFirst(&filename, filespec, wxDIR_FILES);
        while (cont)
        {
            fname.SetFullName(filename);
            wxRemoveFile(fname.GetFullPath());
            cont = odir.GetNext(&filename);
        }
    }
}

void ProjectDependencies::RemoveModFilesWS(NativeParserF* nativeParser)
{
    //Remove all *.mod files in Workspace
    ProjectsArray* projects = Manager::Get()->GetProjectManager()->GetProjects();
    for (size_t i = 0; i < projects->GetCount(); ++i)
    {
        cbProject* pr = projects->Item(i);
        if (!pr->IsMakefileCustom())
        {
            ProjectBuildTarget* bTarget = pr->GetBuildTarget(pr->GetActiveBuildTarget());
            RemoveModFiles(pr, bTarget, nativeParser);
        }
    }
}

void ProjectDependencies::PrintChildrenTable()
{
    Manager::Get()->GetLogManager()->DebugLog(_T("\nProjectDependencies::PrintChildrenTable"));

    for(size_t i=0; i < m_ChildrenTable.size(); i++)
    {
        ProjectFile* pfile = m_prFilesArr[i];

        Manager::Get()->GetLogManager()->DebugLog(_T("\n")+pfile->file.GetName());

        IntSet* children = m_ChildrenTable[i];
        IntSet::iterator pos;
        for (pos=children->begin(); pos != children->end(); ++pos)
        {
            ProjectFile* pf = m_prFilesArr[*pos];
            wxString fname = pf->file.GetName();
            Manager::Get()->GetLogManager()->DebugLog(_T("        ")+fname);
        }
    }
}

size_t ProjectDependencies::GetSizeFiles()
{
    return m_prFilesArr.size();
}

void ProjectDependencies::GetUseFilesFile(const wxString& filename, wxArrayString& useFiles)
{
    if (m_FileIndexMap.count(filename) == 0)
        return;
    size_t fileIndex = m_FileIndexMap[filename];
    StringSet* useModules = m_pUseModules[fileIndex];
    std::set<size_t> fidxSet;
    StringSet::iterator pos;
    for (pos = useModules->begin(); pos != useModules->end(); ++pos)
    {
        wxString modName = *pos;
        if (m_ModuleFileIdxMap.count(modName) == 1)
        {
            size_t fidx = m_ModuleFileIdxMap[modName];
            if (fidx != fileIndex && fidxSet.count(fidx) == 0)
            {
                StringIntMap::const_iterator it;
                for (it = m_FileIndexMap.begin(); it != m_FileIndexMap.end(); ++it)
                {
                    if (it->second == (int) fidx)
                    {
                        useFiles.Add(it->first);
                        fidxSet.insert(fidx);
                        break;
                    }
                }
            }
        }
    }
}

void ProjectDependencies::GetExtendsFilesFile(const wxString& filename, wxArrayString& extFiles)
{
    if (m_FileIndexMap.count(filename) == 0)
        return;
    size_t fileIndex = m_FileIndexMap[filename];
    StringSet* extSModules = m_pExtendsSModules[fileIndex];
    std::set<size_t> fidxSet;
    StringSet::iterator pos;
    for (pos = extSModules->begin(); pos != extSModules->end(); ++pos)
    {
        wxString modName = *pos;
        size_t fidx;
        if (m_ModuleFileIdxMap.count(modName) == 1)
            fidx = m_ModuleFileIdxMap[modName];
        else if (m_SubmoduleFileIdxMap.count(modName) == 1)
            fidx = m_SubmoduleFileIdxMap[modName];
        else
            continue;

        if (fidx != fileIndex && fidxSet.count(fidx) == 0)
        {
            StringIntMap::const_iterator it;
            for (it = m_FileIndexMap.begin(); it != m_FileIndexMap.end(); ++it)
            {
                if (it->second == (int) fidx)
                {
                    extFiles.Add(it->first);
                    fidxSet.insert(fidx);
                    break;
                }
            }
        }
    }
}

void ProjectDependencies::GetIncludeFilesFile(const wxString& filename, wxArrayString& includeFiles)
{
    if (m_FileIndexMap.count(filename) == 0)
        return;
    size_t fileIndex = m_FileIndexMap[filename];
    StringSet* incls = m_pIncludes[fileIndex];
    std::set<size_t> fidxSet;
    StringSet::iterator pos;
    for (pos = incls->begin(); pos != incls->end(); ++pos)
    {
        wxString incName = *pos;
        if (m_IncludeFileIdxMap.count(incName) == 1)
        {
            size_t fidx = m_IncludeFileIdxMap[incName];
            if (fidx != fileIndex && fidxSet.count(fidx) == 0)
            {
                StringIntMap::const_iterator it;
                for (it = m_FileIndexMap.begin(); it != m_FileIndexMap.end(); ++it)
                {
                    if (it->second == (int) fidx)
                    {
                        includeFiles.Add(it->first);
                        fidxSet.insert(fidx);
                        break;
                    }
                }
            }
        }
    }
}
