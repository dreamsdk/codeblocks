#include "fconstruct.h"

std::map<FConstruct::FCLid, wxRegEx*> FConstruct::FCLReMap;
std::map<FConstruct::FCLid, std::vector<wxString> > FConstruct::FCLWordMap;
std::map<wxString, std::vector<FConstruct::FCLid> > FConstruct::WordFCLidMap;

FConstruct::FConstruct()
{
    //ctor
}

FConstruct::~FConstruct()
{
    //dtor
}

void FConstruct::Clear()
{
    m_Parts.clear();
    m_Fct = ctUnknown;
}

void FConstruct::AddPart(const wxString& word1, const wxString& word2, const wxString& word3)
{
    if (word1.IsEmpty())
        return;
    std::vector<wxString> part;
    part.push_back(word1);

    if (word2.IsEmpty())
    {
        m_Parts.push_back(part);
        return;
    }
    else
        part.push_back(word2);

    if (word3.IsEmpty())
    {
        m_Parts.push_back(part);
        return;
    }
    else
        part.push_back(word3);
    m_Parts.push_back(part);
}

void FConstruct::GetWords(int i, wxString& word1, wxString& word2, wxString& word3, FCLid& flid) const
{
    flid = fclUnknown;
    if (int(m_Parts.size()) <= i)
        return;
    size_t nw = m_Parts[i].size();

    if (nw > 2)
    {
        word1 = m_Parts[i][0];
        word2 = m_Parts[i][1];
        word3 = m_Parts[i][2];
    }
    else if (nw > 1)
    {
        word1 = m_Parts[i][0];
        word2 = m_Parts[i][1];
        word3 = wxEmptyString;
    }
    else
    {
        word1 = m_Parts[i][0];
        word2 = wxEmptyString;
        word3 = wxEmptyString;
    }

    if (word1.IsSameAs(_T("if")) && word2.IsSameAs(_T("then")) && word3.IsSameAs(wxEmptyString))
        flid = fclIf_if_then;
    else if (word1.IsSameAs(_T("else")) && word2.IsSameAs(_T("if")) && word3.IsSameAs(_T("then")))
        flid = fclIf_else_if_then;
    else if (word1.IsSameAs(_T("else")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclIf_else;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("if")) && word3.IsSameAs(wxEmptyString))
        flid = fclIf_end_if;

    else if (word1.IsSameAs(_T("do")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclDo_do;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("do")) && word3.IsSameAs(wxEmptyString))
        flid = fclDo_end_do;

    else if (word1.IsSameAs(_T("interface")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclInterf_interf;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("interface")) && word3.IsSameAs(wxEmptyString))
        flid = fclInterf_end_interf;

    else if (word1.IsSameAs(_T("function")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclFun_fun;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("function")) && word3.IsSameAs(wxEmptyString))
        flid = fclFun_end_fun;

    else if (word1.IsSameAs(_T("subroutine")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclSub_sub;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("subroutine")) && word3.IsSameAs(wxEmptyString))
        flid = fclSub_end_sub;

    else if (word1.IsSameAs(_T("program")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclProg_prog;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("program")) && word3.IsSameAs(wxEmptyString))
        flid = fclProg_end_prog;

    else if (word1.IsSameAs(_T("module")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclMod_module;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("module")) && word3.IsSameAs(wxEmptyString))
        flid = fclMod_end_module;

    else if (word1.IsSameAs(_T("submodule")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclSubmod_submod;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("submodule")) && word3.IsSameAs(wxEmptyString))
        flid = fclSubmod_end_submod;

    else if (word1.IsSameAs(_T("select")) && word2.IsSameAs(_T("case")) && word3.IsSameAs(wxEmptyString))
        flid = fclSelectCase_start;
    else if (word1.IsSameAs(_T("case")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclSelectCase_case;
    else if (word1.IsSameAs(_T("case")) && word2.IsSameAs(_T("default")) && word3.IsSameAs(wxEmptyString))
        flid = fclSelectCase_case;
    else if (word1.IsSameAs(_T("select")) && word2.IsSameAs(_T("type")) && word3.IsSameAs(wxEmptyString))
        flid = fclSelectType_start;
    else if (word1.IsSameAs(_T("type")) && word2.IsSameAs(_T("is")) && word3.IsSameAs(wxEmptyString))
        flid = fclSelectType_type_is;
    else if (word1.IsSameAs(_T("class")) && word2.IsSameAs(_T("is")) && word3.IsSameAs(wxEmptyString))
        flid = fclSelectType_class_is;
    else if (word1.IsSameAs(_T("class")) && word2.IsSameAs(_T("default")) && word3.IsSameAs(wxEmptyString))
        flid = fclSelectType_class_default;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("select")) && word3.IsSameAs(wxEmptyString))
        flid = fclSelect_end;

    else if (word1.IsSameAs(_T("type")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclType_type;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("type")) && word3.IsSameAs(wxEmptyString))
        flid = fclType_end_type;

    else if (word1.IsSameAs(_T("enum")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclEnum_enum;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("enum")) && word3.IsSameAs(wxEmptyString))
        flid = fclEnum_end_enum;

    else if (word1.IsSameAs(_T("critical")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclCritical_critical;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("critical")) && word3.IsSameAs(wxEmptyString))
        flid = fclCritical_end_critical;

    else if (word1.IsSameAs(_T("forall")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclForall_forall;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("forall")) && word3.IsSameAs(wxEmptyString))
        flid = fclForall_end_forall;

    else if (word1.IsSameAs(_T("associate")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclAssoc_associate;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("associate")) && word3.IsSameAs(wxEmptyString))
        flid = fclAssoc_end_associate;

    else if (word1.IsSameAs(_T("block")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclBlock_block;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("block")) && word3.IsSameAs(wxEmptyString))
        flid = fclBlock_end_block;

    else if (word1.IsSameAs(_T("block")) && word2.IsSameAs(_T("data")) && word3.IsSameAs(wxEmptyString))
        flid = fclBlockdata_blockdata;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("block")) && word3.IsSameAs(_T("data")))
        flid = fclBlockdata_end_blockdata;

    else if (word1.IsSameAs(_T("change")) && word2.IsSameAs(_T("team")) && word3.IsSameAs(wxEmptyString))
        flid = fclTeam_change_team;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("team")) && word3.IsSameAs(wxEmptyString))
        flid = fclTeam_end_team;

    else if (word1.IsSameAs(_T("where")) && word2.IsSameAs(wxEmptyString) && word3.IsSameAs(wxEmptyString))
        flid = fclWhere_where;
    else if (word1.IsSameAs(_T("else")) && word2.IsSameAs(_T("where")) && word3.IsSameAs(wxEmptyString))
        flid = fclWhere_else_where;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("where")) && word3.IsSameAs(wxEmptyString))
        flid = fclWhere_end_where;

    else if (word1.IsSameAs(_T("module")) && word2.IsSameAs(_T("procedure")) && word3.IsSameAs(wxEmptyString))
        flid = fclProc_mod_proc;
    else if (word1.IsSameAs(_T("end")) && word2.IsSameAs(_T("procedure")) && word3.IsSameAs(wxEmptyString))
        flid = fclProc_end_proc;
}

void FConstruct::MakeFCLReMap()
{
    int options = wxRE_ADVANCED | wxRE_ICASE;

    FCLReMap[fclIf_if_then] = new wxRegEx( wxT("^(\\s*)(([a-zA-Z0-9_]+)(\\s*)(:)(\\s*))?((if)(\\s*)(\\()(.+)(\\))(\\s*)(then))((\\s*)!(.*))?(\\s*)$"), options);
    FCLReMap[fclIf_else_if_then] = new wxRegEx( wxT("^(\\s*)(else)(\\s*)((if)(\\s*)(\\()(.+)(\\))(\\s*)(then)((\\s+)([a-zA-Z0-9_]+))?)((\\s*)!(.*))?(\\s*)$"), options);
    FCLReMap[fclIf_else] = new wxRegEx( wxT("^(\\s*)(else)(\\s*)((\\s+)([a-zA-Z0-9_]+))?((\\s*)!(.*))?(\\s*)$"), options );
	FCLReMap[fclIf_end_if] = new wxRegEx( wxT("^(\\s*)(end)(\\s*)(if)((\\s+)([a-zA-Z0-9_]+))?(\\s*)"), options );

	FCLReMap[fclDo_do] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(do)(([\\s\\t]*$)|([\\s\\t]+[a-z_]+.*))"), options);
	FCLReMap[fclDo_end_do] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(do)\\y"), options);

    FCLReMap[fclInterf_interf] = new wxRegEx(_T("^[\\s\\t]*(abstract[\\s\\t]+)?interface\\y"), options);
    FCLReMap[fclInterf_end_interf] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(interface)\\y"), options);

    FCLReMap[fclFun_fun] = new wxRegEx(_T("^((.*[\\s\\t]+)|([\\s\\t]*))function(\\s+)([a-z0-9_]+)(\\s*)(\\(.*[\\)&]+)"), options);
    FCLReMap[fclFun_end_fun] = new wxRegEx(_T("^[\\s\\t]*(end|((endfunction|(end(\\s+)function))((\\s)+[a-z0-9_]+)?))(\\s*)$"), options);

    FCLReMap[fclSub_sub] = new wxRegEx(_T("^[\\s\\t]*((elemental|impure|module|non_recursive|pure|recursive)\\s+)*subroutine(\\s+)([a-z0-9_]+)(\\s*)(\\(.*[\\)&]+)?\\s*$"), options );
    FCLReMap[fclSub_end_sub] = new wxRegEx(_T("^[\\s\\t]*(end|((endsubroutine|(end(\\s+)subroutine))((\\s)+[a-z0-9_]+)?))(\\s*)$"), options);

    FCLReMap[fclProg_prog] = new wxRegEx(_T("^[\\s\\t]*program(\\s+)([a-z0-9_]+)([\\s\\t]*)$"), options );
    FCLReMap[fclProg_end_prog] = new wxRegEx(_T("^[\\s\\t]*(end|((endprogram|(end(\\s+)program))((\\s)+[a-z0-9_]+)?))(\\s*)$"), options);

    FCLReMap[fclMod_module] = new wxRegEx(_T("^[\\s\\t]*(module)(\\s+)((?!procedure[\\s:]+)[a-z0-9_]+)([\\s\\t]*)$"), options );
    FCLReMap[fclMod_end_module] = new wxRegEx(_T("^[\\s\\t]*(end|((endmodule|(end(\\s+)module))((\\s)+[a-z0-9_]+)?))(\\s*)$"), options);

    FCLReMap[fclSubmod_submod] = new wxRegEx(_T("^[\\s\\t]*(submodule)(\\s*)(\\(\\s*[a-z0-9_:]+\\s*\\))(\\s*)([a-z0-9_]+)([\\s\\t]*)$"), options);
    FCLReMap[fclSubmod_end_submod] = new wxRegEx(_T("^[\\s\\t]*(end|((endsubmodule|(end(\\s+)submodule))((\\s)+[a-z0-9_]+)?))(\\s*)$"), options);

    FCLReMap[fclSelectCase_start] = new wxRegEx(_T("^[\\s\\t]*(([a-zA-Z0-9_]+)(\\s*)(:)(\\s*))?select(\\s*)case(\\s*)(\\(.*[\\)&]+)"), options);
    FCLReMap[fclSelectCase_case] = new wxRegEx(_T("^[\\s\\t]*((case(\\s+)default)|(case(\\s*)\\(.*\\)))([\\s\\t]*[a-z0-9_]+)?([\\s\\t]*)$"), options);
    FCLReMap[fclSelect_end] = new wxRegEx(_T("^[\\s\\t]*(end(\\s*)select)((\\s)+[a-z0-9_]+)?(\\s*)$"), options);

    FCLReMap[fclSelectType_start] = new wxRegEx(_T("^[\\s\\t]*(([a-zA-Z0-9_]+)(\\s*)(:)(\\s*))?select(\\s*)type(\\s*)(\\(.*[\\)&]+)"), options);
    FCLReMap[fclSelectType_type_is] = new wxRegEx(_T("^[\\s\\t]*type(\\s+)is(\\s*)(\\(.*[\\)&]+)"), options );
    FCLReMap[fclSelectType_class_is] = new wxRegEx(_T("^[\\s\\t]*class(\\s+)is(\\s*)(\\(.*[\\)&]+)"), options );
    FCLReMap[fclSelectType_class_default] = new wxRegEx(_T("^[\\s\\t]*class(\\s+)default((\\s)+[a-z0-9_]+)?(\\s*)$"), options);

    FCLReMap[fclType_type] = new wxRegEx(_T("^([\\s\\t]*)(type)(\\s*)((\\s*,\\s*(([a-z0-9_]+)|(extends\\([a-z0-9_]+\\))))*\\s*::)?(\\s*)([a-z0-9_]+)\\y"), options);
    FCLReMap[fclType_end_type] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(type)\\y"), options);

    FCLReMap[fclEnum_enum] = new wxRegEx(_T("^[\\s\\t]*enum\\y"), options);
    FCLReMap[fclEnum_end_enum] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(enum)\\y"), options);

    FCLReMap[fclCritical_critical] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(critical)\\y"), options);
    FCLReMap[fclCritical_end_critical] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(critical)\\y"), options);

    FCLReMap[fclForall_forall] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(forall)(\\s*)(\\([^\\)]+\\))(\\s*)$"), options);
    FCLReMap[fclForall_end_forall] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(forall)\\y"), options);

    FCLReMap[fclAssoc_associate] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(associate)(\\s*)(\\(.*\\))(\\s*)$"), options);
    FCLReMap[fclAssoc_end_associate] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(associate)\\y"), options);

    FCLReMap[fclBlock_block] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)\\s*:\\s*)?(block)(\\s*)$"), options);
    FCLReMap[fclBlock_end_block] = new wxRegEx(_T("^[\\s\\t]*([0-9]*)([\\s\\t]*)(end)(\\s*)(block)([\\s\\t]*)(([\\s\\t]+)([a-z0-9_]+)\\s*)?$"), options);

    FCLReMap[fclTeam_change_team] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(change\\s+team)\\s*\\(.*\\)(\\s*)$"), options);
    FCLReMap[fclTeam_end_team] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(end\\s*team)\\y"), options);

    FCLReMap[fclBlockdata_blockdata] = new wxRegEx(_T("^([\\s\\t]*)(block\\s*data)(([\\s\\t]+)([a-z0-9_]+))?\\s*$"), options);
    FCLReMap[fclBlockdata_end_blockdata] = new wxRegEx(_T("^([\\s\\t]*)(end|((end\\s*block\\s*data)(([\\s\\t]+)([a-z0-9_]+))?))\\s*$"), options);

    FCLReMap[fclWhere_where] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(where)(\\s*)(\\([^\\)]+\\))(\\s*)$"), options);
    FCLReMap[fclWhere_else_where] = new wxRegEx(_T("^([\\s\\t]*)(else\\s*where)\\y"), options);
    FCLReMap[fclWhere_end_where] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(where)\\y"), options);

    FCLReMap[fclProc_mod_proc] = new wxRegEx(_T("^[\\s\\t]*module(\\s+)procedure(\\s+)([a-z0-9_]+)(\\s*)$"), options );
    FCLReMap[fclProc_end_proc] = new wxRegEx(_T("^[\\s\\t]*(end|((endprocedure|(end(\\s+)procedure))((\\s)+[a-z0-9_]+)?))(\\s*)$"), options);
}

void FConstruct::DelFCLReMap()
{
    for (std::map<FCLid, wxRegEx*>::iterator it=FCLReMap.begin(); it!=FCLReMap.end(); ++it)
        delete it->second;
    FCLReMap.clear();
}

void FConstruct::MakeFCLWordMap()
{
    std::vector<wxString> words;
    words.resize(3);
    words[0] = _T("if");
    words[1] = _T("then");
    words[2] = wxEmptyString;
    FCLWordMap[fclIf_if_then] = words;

    words[0] = _T("else");
    words[1] = _T("if");
    words[2] = _T("then");
    FCLWordMap[fclIf_else_if_then] = words;

    words[0] = _T("else");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclIf_else] = words;

    words[0] = _T("end");
    words[1] = _T("if");
    words[2] = wxEmptyString;
    FCLWordMap[fclIf_end_if] = words;

    words[0] = _T("do");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclDo_do] = words;

    words[0] = _T("end");
    words[1] = _T("do");
    words[2] = wxEmptyString;
    FCLWordMap[fclDo_end_do] = words;

    words[0] = _T("interface");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclInterf_interf] = words;

    words[0] = _T("end");
    words[1] = _T("interface");
    words[2] = wxEmptyString;
    FCLWordMap[fclInterf_end_interf] = words;

    words[0] = _T("function");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclFun_fun] = words;

    words[0] = _T("end");
    words[1] = _T("function");
    words[2] = wxEmptyString;
    FCLWordMap[fclFun_end_fun] = words;

    words[0] = _T("subroutine");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclSub_sub] = words;

    words[0] = _T("end");
    words[1] = _T("subroutine");
    words[2] = wxEmptyString;
    FCLWordMap[fclSub_end_sub] = words;

    words[0] = _T("program");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclProg_prog] = words;

    words[0] = _T("end");
    words[1] = _T("program");
    words[2] = wxEmptyString;
    FCLWordMap[fclProg_end_prog] = words;

    words[0] = _T("module");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclMod_module] = words;

    words[0] = _T("end");
    words[1] = _T("module");
    words[2] = wxEmptyString;
    FCLWordMap[fclMod_end_module] = words;

    words[0] = _T("submodule");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclSubmod_submod] = words;

    words[0] = _T("end");
    words[1] = _T("submodule");
    words[2] = wxEmptyString;
    FCLWordMap[fclSubmod_end_submod] = words;

    words[0] = _T("select");
    words[1] = _T("type");
    words[2] = wxEmptyString;
    FCLWordMap[fclSelectType_start] = words;

    words[0] = _T("type");
    words[1] = _T("is");
    words[2] = wxEmptyString;
    FCLWordMap[fclSelectType_type_is] = words;

    words[0] = _T("class");
    words[1] = _T("is");
    words[2] = wxEmptyString;
    FCLWordMap[fclSelectType_class_is] = words;

    words[0] = _T("class");
    words[1] = _T("default");
    words[2] = wxEmptyString;
    FCLWordMap[fclSelectType_class_default] = words;

    words[0] = _T("select");
    words[1] = _T("case");
    words[2] = wxEmptyString;
    FCLWordMap[fclSelectCase_start] = words;

    words[0] = _T("case");
    words[1] = _T("default");
    words[2] = wxEmptyString;
    FCLWordMap[fclSelectCase_case] = words;

    words[0] = _T("end");
    words[1] = _T("select");
    words[2] = wxEmptyString;
    FCLWordMap[fclSelect_end] = words;

    words[0] = _T("type");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclType_type] = words;

    words[0] = _T("end");
    words[1] = _T("type");
    words[2] = wxEmptyString;
    FCLWordMap[fclType_end_type] = words;

    words[0] = _T("enum");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclEnum_enum] = words;

    words[0] = _T("end");
    words[1] = _T("enum");
    words[2] = wxEmptyString;
    FCLWordMap[fclEnum_end_enum] = words;

    words[0] = _T("critical");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclCritical_critical] = words;

    words[0] = _T("end");
    words[1] = _T("critical");
    words[2] = wxEmptyString;
    FCLWordMap[fclCritical_end_critical] = words;

    words[0] = _T("forall");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclForall_forall] = words;

    words[0] = _T("end");
    words[1] = _T("forall");
    words[2] = wxEmptyString;
    FCLWordMap[fclForall_end_forall] = words;

    words[0] = _T("associate");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclAssoc_associate] = words;

    words[0] = _T("end");
    words[1] = _T("associate");
    words[2] = wxEmptyString;
    FCLWordMap[fclAssoc_end_associate] = words;

    words[0] = _T("block");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclBlock_block] = words;

    words[0] = _T("end");
    words[1] = _T("block");
    words[2] = wxEmptyString;
    FCLWordMap[fclBlock_end_block] = words;

    words[0] = _T("block");
    words[1] = _T("data");
    words[2] = wxEmptyString;
    FCLWordMap[fclBlockdata_blockdata] = words;

    words[0] = _T("end");
    words[1] = _T("block");
    words[2] = _T("data");
    FCLWordMap[fclBlockdata_end_blockdata] = words;

    words[0] = _T("change");
    words[1] = _T("team");
    words[2] = wxEmptyString;
    FCLWordMap[fclTeam_change_team] = words;

    words[0] = _T("end");
    words[1] = _T("team");
    words[2] = wxEmptyString;
    FCLWordMap[fclTeam_end_team] = words;

    words[0] = _T("where");
    words[1] = wxEmptyString;
    words[2] = wxEmptyString;
    FCLWordMap[fclWhere_where] = words;

    words[0] = _T("else");
    words[1] = _T("where");
    words[2] = wxEmptyString;
    FCLWordMap[fclWhere_else_where] = words;

    words[0] = _T("end");
    words[1] = _T("where");
    words[2] = wxEmptyString;
    FCLWordMap[fclWhere_end_where] = words;

    words[0] = _T("module");
    words[1] = _T("procedure");
    words[2] = wxEmptyString;
    FCLWordMap[fclProc_mod_proc] = words;

    words[0] = _T("end");
    words[1] = _T("procedure");
    words[2] = wxEmptyString;
    FCLWordMap[fclProc_end_proc] = words;
}

void FConstruct::GetWordsFromFCLid(FCLid flid, wxString& word1, wxString& word2, wxString& word3)
{
    if (FCLWordMap.count(flid) > 0)
    {
        std::vector<wxString> words;
        words = FCLWordMap[flid];
        word1 = words[0];
        word2 = words[1];
        word3 = words[2];
    }
    else
    {
        word1 = wxEmptyString;
        word2 = wxEmptyString;
        word3 = wxEmptyString;
    }
}

void FConstruct::MakeWordFCLidMap()
{
    std::vector<FCLid> idv;
    wxString kw = _T("if");
    idv.push_back(fclIf_if_then);
    idv.push_back(fclIf_else_if_then);
    idv.push_back(fclIf_end_if);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("then");
    idv.push_back(fclIf_if_then);
    idv.push_back(fclIf_else_if_then);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("else");
    idv.push_back(fclIf_else_if_then);
    idv.push_back(fclIf_else);
    idv.push_back(fclWhere_else_where);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("elseif");
    idv.push_back(fclIf_else_if_then);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("end");
    idv.push_back(fclIf_end_if);
    idv.push_back(fclDo_end_do);
    idv.push_back(fclInterf_end_interf);
    idv.push_back(fclSub_end_sub);
    idv.push_back(fclFun_end_fun);
    idv.push_back(fclProg_end_prog);
    idv.push_back(fclMod_end_module);
    idv.push_back(fclSubmod_end_submod);
    idv.push_back(fclSelect_end);
    idv.push_back(fclType_end_type);
    idv.push_back(fclEnum_end_enum);
    idv.push_back(fclCritical_end_critical);
    idv.push_back(fclForall_end_forall);
    idv.push_back(fclWhere_end_where);
    idv.push_back(fclAssoc_end_associate);
    idv.push_back(fclBlock_end_block);
    idv.push_back(fclTeam_end_team);
    idv.push_back(fclBlockdata_end_blockdata);
    idv.push_back(fclProc_end_proc);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endif");
    idv.push_back(fclIf_end_if);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("do");
    idv.push_back(fclDo_do);
    idv.push_back(fclDo_end_do);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("enddo");
    idv.push_back(fclDo_end_do);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("while");
    idv.push_back(fclDo_do);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("concurrent");
    idv.push_back(fclDo_do);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("abstract");
    idv.push_back(fclInterf_interf);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("interface");
    idv.push_back(fclInterf_interf);
    idv.push_back(fclInterf_end_interf);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endinterface");
    idv.push_back(fclInterf_end_interf);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("function");
    idv.push_back(fclFun_fun);
    idv.push_back(fclFun_end_fun);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endfunction");
    idv.push_back(fclFun_end_fun);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("subroutine");
    idv.push_back(fclSub_sub);
    idv.push_back(fclSub_end_sub);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endsubroutine");
    idv.push_back(fclSub_end_sub);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("program");
    idv.push_back(fclProg_prog);
    idv.push_back(fclProg_end_prog);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endprogram");
    idv.push_back(fclProg_end_prog);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("module");
    idv.push_back(fclMod_module);
    idv.push_back(fclMod_end_module);
    idv.push_back(fclProc_mod_proc);
    idv.push_back(fclProc_end_proc);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endmodule");
    idv.push_back(fclMod_end_module);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("submodule");
    idv.push_back(fclSubmod_submod);
    idv.push_back(fclSubmod_end_submod);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endsubmodule");
    idv.push_back(fclSubmod_end_submod);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("select");
    idv.push_back(fclSelectCase_start);
    idv.push_back(fclSelect_end);
    idv.push_back(fclSelectType_start);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("case");
    idv.push_back(fclSelectCase_start);
    idv.push_back(fclSelectCase_case);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("selectcase");
    idv.push_back(fclSelectCase_start);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("selecttype");
    idv.push_back(fclSelectType_start);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endselect");
    idv.push_back(fclSelect_end);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("default");
    idv.push_back(fclSelectCase_case);
    idv.push_back(fclSelectType_class_default);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("type");
    idv.push_back(fclSelectType_start);
    idv.push_back(fclSelectType_type_is);
    idv.push_back(fclType_type);
    idv.push_back(fclType_end_type);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("class");
    idv.push_back(fclSelectType_class_is);
    idv.push_back(fclSelectType_class_default);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("is");
    idv.push_back(fclSelectType_type_is);
    idv.push_back(fclSelectType_class_is);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endtype");
    idv.push_back(fclType_end_type);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("enum");
    idv.push_back(fclEnum_enum);
    idv.push_back(fclEnum_end_enum);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endenum");
    idv.push_back(fclEnum_end_enum);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("critical");
    idv.push_back(fclCritical_critical);
    idv.push_back(fclCritical_end_critical);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endcritical");
    idv.push_back(fclCritical_end_critical);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("forall");
    idv.push_back(fclForall_forall);
    idv.push_back(fclForall_end_forall);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endforall");
    idv.push_back(fclForall_end_forall);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("associate");
    idv.push_back(fclAssoc_associate);
    idv.push_back(fclAssoc_end_associate);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endassociate");
    idv.push_back(fclAssoc_end_associate);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("block");
    idv.push_back(fclBlock_block);
    idv.push_back(fclBlock_end_block);
    idv.push_back(fclBlockdata_blockdata);
    idv.push_back(fclBlockdata_end_blockdata);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endblock");
    idv.push_back(fclBlock_end_block);
    idv.push_back(fclBlockdata_end_blockdata);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("data");
    idv.push_back(fclBlockdata_blockdata);
    idv.push_back(fclBlockdata_end_blockdata);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("blockdata");
    idv.push_back(fclBlockdata_blockdata);
    idv.push_back(fclBlockdata_end_blockdata);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endblockdata");
    idv.push_back(fclBlockdata_end_blockdata);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("change");
    idv.push_back(fclTeam_change_team);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("team");
    idv.push_back(fclTeam_change_team);
    idv.push_back(fclTeam_end_team);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endteam");
    idv.push_back(fclTeam_end_team);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("where");
    idv.push_back(fclWhere_where);
    idv.push_back(fclWhere_else_where);
    idv.push_back(fclWhere_end_where);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("elsewhere");
    idv.push_back(fclWhere_else_where);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("endwhere");
    idv.push_back(fclWhere_end_where);
    WordFCLidMap[kw] = idv;

    idv.clear();
    kw = _T("procedure");
    idv.push_back(fclProc_mod_proc);
    idv.push_back(fclProc_end_proc);
    WordFCLidMap[kw] = idv;
}

