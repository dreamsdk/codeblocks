#ifndef FCONSTRUCT_H
#define FCONSTRUCT_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/string.h>
    #include <wx/regex.h>
#endif
#include <vector>
#include <map>

class FConstruct
{
    public:
        enum FCLid
        {
            fclUnknown,
            fclIf_if_then,
            fclIf_else_if_then,
            fclIf_else,
            fclIf_end_if,
            fclDo_do,
            fclDo_end_do,
            fclInterf_interf,
            fclInterf_end_interf,
            fclSub_sub,
            fclSub_end_sub,
            fclFun_fun,
            fclFun_end_fun,
            fclProg_prog,
            fclProg_end_prog,
            fclMod_module,
            fclMod_end_module,
            fclSubmod_submod,
            fclSubmod_end_submod,
            fclSelect_end,
            fclSelectCase_start,
            fclSelectCase_case,
            fclSelectType_start,
            fclSelectType_type_is,
            fclSelectType_class_is,
            fclSelectType_class_default,
            fclSelectRank_start,
            fclSelectRank_rank,
            fclType_type,
            fclType_end_type,
            fclEnum_enum,
            fclEnum_end_enum,
            fclCritical_critical,
            fclCritical_end_critical,
            fclForall_forall,
            fclForall_end_forall,
            fclAssoc_associate,
            fclAssoc_end_associate,
            fclBlock_block,
            fclBlock_end_block,
            fclTeam_change_team,
            fclTeam_end_team,
            fclWhere_where,
            fclWhere_else_where,
            fclWhere_end_where,
            fclBlockdata_blockdata,
            fclBlockdata_end_blockdata,
            fclProc_mod_proc,
            fclProc_end_proc,

            fclProgGroup_start,
            fclProgGroup_end,

            fclSelGroup_start,
            fclSelGroup_end,
        };

        enum FConstructType
        {
            ctProgramGroup,
            ctSelectGroup,
            ctIf,
            ctDo,
            ctSubroutine,
            ctFunction,
            ctInterface,
            ctAssiciate,
            ctBlock,
            ctBlockdata,
            ctCritical,
            ctModule,
            ctProgram,
            ctCase,
            ctType,
            ctWhere,
            ctEnum,
            ctForall,
            ctSubmodule,
            ctTeam,
            ctProcedure,
            ctEnd,
            ctUnknown
        };

        static std::map<FCLid, wxRegEx*> FCLReMap;
        static void MakeFCLReMap();
        static void DelFCLReMap();

        static std::map<FCLid, std::vector<wxString> > FCLWordMap;
        static void MakeFCLWordMap();
        static void GetWordsFromFCLid(FCLid flid, wxString& word1, wxString& word2, wxString& word3);

        static std::map<wxString, std::vector<FCLid> > WordFCLidMap;
        static void MakeWordFCLidMap();

    public:
        FConstruct();
        virtual ~FConstruct();
        void Clear();
        void AddPart(const wxString& word1, const wxString& word2, const wxString& word3);
        void GetWords(int i, wxString& word1, wxString& word2, wxString& word3, FCLid& flid) const;
        size_t Size() const {return m_Parts.size();};
        void SetType(FConstructType fct) {m_Fct = fct;};
        FConstructType GetType() const {return m_Fct;};

    private:
        FConstructType m_Fct;
        std::vector<std::vector<wxString> > m_Parts;
};

#endif // FCONSTRUCT_H
