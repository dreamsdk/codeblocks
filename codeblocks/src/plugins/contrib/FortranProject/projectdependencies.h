/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */

#ifndef PROJECTDEPENDENCIES_H
#define PROJECTDEPENDENCIES_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <cbproject.h>
#endif
#include <set>
#include <vector>
#include <map>

#include "parserf.h"

typedef std::vector<StringSet*> StringSetPVector;
typedef std::map<wxString,int> StringIntMap;
typedef std::map<int,wxString> IntStringMap;
typedef std::set<int> IntSet;
typedef std::vector<IntSet*> PointersToIntSet;
typedef std::vector<ProjectFile*> ProjectFilesArray;
typedef std::vector<bool> BoolVector;

class NativeParserF;


class ProjectDependencies
{
    public:
        ProjectDependencies();
        virtual ~ProjectDependencies();
        void Clear();
        void MakeProjectFilesDependencies(ProjectFilesArray& prFilesArr, ParserF& parser);
        unsigned short int GetFileWeight(wxString& fileName);
        void EnsureUpToDateObjs();
        bool HasInfiniteDependences();
        size_t GetSizeFiles();
        static void RemoveModFiles(cbProject* pr, ProjectBuildTarget* bTarget, NativeParserF* nativeParser);
        static void RemoveModFilesWS(NativeParserF* nativeParser);
        void GetUseFilesFile(const wxString& filename, wxArrayString& use);
        void GetExtendsFilesFile(const wxString& filename, wxArrayString& extFiles);
        void GetIncludeFilesFile(const wxString& filename, wxArrayString& includesFile);

    protected:
    private:
        unsigned short int GetFileWeightByIndex(size_t idx);
        void MakeFileChildren(IntSet* children, size_t fileIndex);

        ProjectFilesArray m_prFilesArr;
    	StringSetPVector m_pUseModules;
        StringSetPVector m_pDeclaredModules;
        StringSetPVector m_pExtendsSModules;
        StringSetPVector m_pDeclaredSubmodules;
        StringSetPVector m_pIncludes;
        StringIntMap m_FileIndexMap;
        StringIntMap m_ModuleFileIdxMap;
        StringIntMap m_SubmoduleFileIdxMap;
        StringIntMap m_IncludeFileIdxMap;
        PointersToIntSet m_ChildrenTable;
        int m_Deep;
        bool m_WasInfiniteLoop;
        bool m_BreakChain;
        bool m_FilesAreUniqueInWorkspace;
        wxArrayInt m_FileWeights;
        BoolVector m_MadeChildrenSet;

        void PrintChildrenTable();
};

#endif // PROJECTDEPENDENCIES_H
