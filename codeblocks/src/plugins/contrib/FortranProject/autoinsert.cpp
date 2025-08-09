#include "autoinsert.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <configmanager.h>
#endif
#include <algorithm>

#include <cbstyledtextctrl.h>

#include "fortranfileext.h"

extern FortranFileExt g_FortranFileExt;

AutoInsert::AutoInsert()
{
    // ctor
    m_NameMap["if"] = "if (...) then";
    m_NameMap["do"] = "do ...";
    m_NameMap["subroutine"] = "subroutine";
    m_NameMap["function"] = "function";
    m_NameMap["interface"] = "interface";
    m_NameMap["associate"] = "associate";
    m_NameMap["block"] = "block";
    m_NameMap["critical"] = "critical";
    m_NameMap["module"] = "module";
    m_NameMap["program"] = "program";
    m_NameMap["select"] = "select ...";
    m_NameMap["type"] = "type";
    m_NameMap["where"] = "where (...)";
    m_NameMap["enum"] = "enum";
    m_NameMap["forall"] = "forall (...)";
    m_NameMap["submodule"] = "submodule";
    m_NameMap["team"] = "change team (...)";

#if wxCHECK_VERSION(3, 1, 6)
    int opt = wxRE_EXTENDED | wxRE_ICASE | wxRE_NOSUB;
    m_RegMap["if"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*):(\\s*))?(if)(\\s*)(\\(.*\\))(\\s*)then\\b.*", opt);
    m_RegMap["endif"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(if)\\b", opt);
    m_RegMap["do"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(do)(\\b)", opt);
    m_RegMap["enddo"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(do)\\b", opt);
    m_RegMap["subroutine"] = new wxRegEx("(.*)subroutine(\\s+)([a-z0-9_]+)(\\s*)(\\(.*[\\)&]+)", opt);
    m_RegMap["endsubroutine"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(subroutine)\\b", opt);
    m_RegMap["function"] = new wxRegEx("(.*)function(\\s+)([a-z0-9_]+)(\\s*)(\\(.*[\\)&]+)", opt);
    m_RegMap["endfunction"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(function)\\b", opt);
    m_RegMap["interface"] = new wxRegEx("^[\\s\\t]*(abstract(\\s*))?interface(\\b)", opt);
    m_RegMap["endinterface"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(interface)\\b", opt);
    m_RegMap["associate"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(associate)(\\s*)(\\(.*\\))", opt);
    m_RegMap["endassociate"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(associate)\\b", opt);
    m_RegMap["block"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(block)\\b", opt);
    m_RegMap["endblock"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(block)\\b", opt);
    m_RegMap["critical"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(critical)\\b", opt);
    m_RegMap["endcritical"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(critical)\\b", opt);
    m_RegMap["module"] = new wxRegEx("^[\\s\\t]*(module)(\\s+)((?!procedure[\\s:]+)[a-z0-9_]+)\\b", opt);
    m_RegMap["endmodule"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(module)\\b", opt);
    m_RegMap["program"] = new wxRegEx("^[\\s\\t]*program\\b", opt);
    m_RegMap["endprogram"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(program)\\b", opt);
    m_RegMap["select"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(select)(\\s*)(case|type)(\\s*)(\\(.*\\))", opt);
    m_RegMap["endselect"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(select)\\b", opt);
    m_RegMap["type"] = new wxRegEx("^([\\s\\t]*)(type)(\\s*)((\\s*,\\s*([a-z0-9_]+))*\\s*::)?(\\s*)([a-z0-9_]+)\\b", opt);
    m_RegMap["endtype"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(type)\\b", opt);
    m_RegMap["where"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(where)(\\s*)(\\(.*\\))(\\s*)(!(.*))?$", opt);
    m_RegMap["endwhere"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(where)\\b", opt);
    m_RegMap["enum"] = new wxRegEx("^[\\s\\t]*enum\\b", opt);
    m_RegMap["endenum"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(enum)\\b", opt);
    m_RegMap["forall"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(forall)(\\s*)(\\(.*\\))(\\s*)(!(.*))?$", opt);
    m_RegMap["endforall"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(forall)\\b", opt);
    m_RegMap["submodule"] = new wxRegEx("^[\\s\\t]*(submodule)(\\s*)(\\([a-z0-9_]+\\))(\\s*)([a-z0-9_]+)", opt);
    m_RegMap["endsubmodule"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(submodule)\\b", opt);
    m_RegMap["team"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(change)(\\s*)(team)(\\s*)(\\(.*\\))", opt);
    m_RegMap["endteam"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(team)\\b", opt);

    m_RegMap["end"] = new wxRegEx("^[\\s\\t]*[0-9]*[\\s\\t]*\\b(end)\\b(([\\s\\t]*)!(.*))?([\\s\\t]*)$", opt);
    m_RegMap["endunit"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(subroutine|function|program)\\b", opt);
    m_RegMap["contains"] = new wxRegEx("^[\\s\\t]*(contains)([\\s\\t]*)$", opt);

#else // wxCHECK_VERSION
    int opt = wxRE_ADVANCED | wxRE_ICASE | wxRE_NOSUB;
    m_RegMap["if"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*):(\\s*))?(if)(\\s*)(\\(.*\\))(\\s*)then\\y.*", opt);
    m_RegMap["endif"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(if)\\y", opt);
    m_RegMap["do"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(do)(\\y)", opt);
    m_RegMap["enddo"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(do)\\y", opt);
    m_RegMap["subroutine"] = new wxRegEx("(.*)subroutine(\\s+)([a-z0-9_]+)(\\s*)(\\(.*[\\)&]+)", opt);
    m_RegMap["endsubroutine"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(subroutine)\\y", opt);
    m_RegMap["function"] = new wxRegEx("(.*)function(\\s+)([a-z0-9_]+)(\\s*)(\\(.*[\\)&]+)", opt);
    m_RegMap["endfunction"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(function)\\y", opt);
    m_RegMap["interface"] = new wxRegEx("^[\\s\\t]*(abstract(\\s*))?interface(\\y)", opt);
    m_RegMap["endinterface"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(interface)\\y", opt);
    m_RegMap["associate"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(associate)(\\s*)(\\(.*\\))", opt);
    m_RegMap["endassociate"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(associate)\\y", opt);
    m_RegMap["block"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(block)\\y", opt);
    m_RegMap["endblock"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(block)\\y", opt);
    m_RegMap["critical"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(critical)\\y", opt);
    m_RegMap["endcritical"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(critical)\\y", opt);
    m_RegMap["module"] = new wxRegEx("^[\\s\\t]*(module)(\\s+)((?!procedure[\\s:]+)[a-z0-9_]+)\\y", opt);
    m_RegMap["endmodule"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(module)\\y", opt);
    m_RegMap["program"] = new wxRegEx("^[\\s\\t]*program\\y", opt);
    m_RegMap["endprogram"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(program)\\y", opt);
    m_RegMap["select"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(select)(\\s*)(case|type)(\\s*)(\\(.*\\))", opt);
    m_RegMap["endselect"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(select)\\y", opt);
    m_RegMap["type"] = new wxRegEx("^([\\s\\t]*)(type)(\\s*)((\\s*,\\s*([a-z0-9_]+))*\\s*::)?(\\s*)([a-z0-9_]+)\\y", opt);
    m_RegMap["endtype"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(type)\\y", opt);
    m_RegMap["where"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(where)(\\s*)(\\(.*\\))(\\s*)(!(.*))?$", opt);
    m_RegMap["endwhere"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(where)\\y", opt);
    m_RegMap["enum"] = new wxRegEx("^[\\s\\t]*enum\\y", opt);
    m_RegMap["endenum"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(enum)\\y", opt);
    m_RegMap["forall"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(forall)(\\s*)(\\(.*\\))(\\s*)(!(.*))?$", opt);
    m_RegMap["endforall"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(forall)\\y", opt);
    m_RegMap["submodule"] = new wxRegEx("^[\\s\\t]*(submodule)(\\s*)(\\([a-z0-9_]+\\))(\\s*)([a-z0-9_]+)", opt);
    m_RegMap["endsubmodule"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(submodule)\\y", opt);
    m_RegMap["team"] = new wxRegEx("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(change)(\\s*)(team)(\\s*)(\\(.*\\))", opt);
    m_RegMap["endteam"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(team)\\y", opt);

    m_RegMap["end"] = new wxRegEx("^[\\s\\t]*[0-9]*[\\s\\t]*\\y(end)\\y(([\\s\\t]*)!(.*))?([\\s\\t]*)$", opt);
    m_RegMap["endunit"] = new wxRegEx("^[\\s\\t]*(end)(\\s*)(subroutine|function|program)\\y", opt);
    m_RegMap["contains"] = new wxRegEx("^[\\s\\t]*(contains)([\\s\\t]*)$", opt);
#endif // wxCHECK_VERSION

    m_RulesWereChanged = false;
    ReadAIOptions();
}

AutoInsert::~AutoInsert()
{
    // dtor
    std::map<wxString,wxRegEx*>::iterator it;

    for (it = m_RegMap.begin(); it != m_RegMap.end(); ++it)
        delete(it->second);
}

void AutoInsert::EditRule(const wxString& statementName, int aiType, bool doAddName, bool alignToStatement)
{
    wxString statm = FindKey(statementName);
    if (!statm.empty() && m_NameMap.count(statm) > 0)
    {
        if (m_AITMap[statm] != GetAIT(aiType))
        {
            m_AITMap[statm] = GetAIT(aiType);
            m_RulesWereChanged = true;
        }
        if (m_DoAddNameMap[statm] != doAddName)
        {
            m_DoAddNameMap[statm] = doAddName;
            m_RulesWereChanged = true;
        }
        if (m_AlignTSMap[statm] != alignToStatement)
        {
            m_AlignTSMap[statm] = alignToStatement;
            m_RulesWereChanged = true;
        }
    }
}

const std::map<wxString,wxString>* AutoInsert::GetNameMap()
{
    return &m_NameMap;
}

bool AutoInsert::GetItemValues(const wxString& statementName, int& aiType, bool& doAddName, bool& alignToStatement)
{
    wxString key = FindKey(statementName);
    if (key.IsEmpty())
        return false;

    AutoInsertType ait = m_AITMap[key];
    aiType = GetAITInt(ait);

    doAddName = m_DoAddNameMap[key];
    alignToStatement = m_AlignTSMap[key];

    return true;
}

bool AutoInsert::GetItemChoices(const wxString& statementName, wxArrayString& aiTypeStrArr,
                            wxArrayString& alignStrArr, bool& addNameEnabled)
{
    aiTypeStrArr.Empty();
    alignStrArr.Empty();
    addNameEnabled = true;
    wxString key = FindKey(statementName);
    if (key.IsEmpty())
        return false;

    alignStrArr.Add("to statement");
    alignStrArr.Add("to name");

    if (key == "if")
    {
        aiTypeStrArr.Add("end if");
        aiTypeStrArr.Add("endif");
        aiTypeStrArr.Add("EndIf");
        aiTypeStrArr.Add("--none--");
    }
    else if(key == "do")
    {
        aiTypeStrArr.Add("end do");
        aiTypeStrArr.Add("enddo");
        aiTypeStrArr.Add("EndDo");
        aiTypeStrArr.Add("--none--");
    }
    else if(key == "subroutine")
    {
        aiTypeStrArr.Add("end subroutine");
        aiTypeStrArr.Add("endsubroutine");
        aiTypeStrArr.Add("EndSubroutine");
        aiTypeStrArr.Add("--none--");

        alignStrArr.Empty();
    }
    else if(key == "function")
    {
        aiTypeStrArr.Add("end function");
        aiTypeStrArr.Add("endfunction");
        aiTypeStrArr.Add("EndFunction");
        aiTypeStrArr.Add("--none--");

        alignStrArr.Empty();
    }
    else if(key == "interface")
    {
        aiTypeStrArr.Add("end interface");
        aiTypeStrArr.Add("endinterface");
        aiTypeStrArr.Add("EndInterface");
        aiTypeStrArr.Add("--none--");

        alignStrArr.Empty();
    }
    else if(key == "associate")
    {
        aiTypeStrArr.Add("end associate");
        aiTypeStrArr.Add("endassociate");
        aiTypeStrArr.Add("EndAssociate");
        aiTypeStrArr.Add("--none--");
    }
    else if(key == "block")
    {
        aiTypeStrArr.Add("end block");
        aiTypeStrArr.Add("endblock");
        aiTypeStrArr.Add("EndBlock");
        aiTypeStrArr.Add("--none--");
    }
    else if(key == "critical")
    {
        aiTypeStrArr.Add("end critical");
        aiTypeStrArr.Add("endcritical");
        aiTypeStrArr.Add("EndCritical");
        aiTypeStrArr.Add("--none--");
    }
    else if(key == "module")
    {
        aiTypeStrArr.Add("end module");
        aiTypeStrArr.Add("endmodule");
        aiTypeStrArr.Add("EndModule");
        aiTypeStrArr.Add("--none--");

        alignStrArr.Empty();
    }
    else if(key == "program")
    {
        aiTypeStrArr.Add("end program");
        aiTypeStrArr.Add("endprogram");
        aiTypeStrArr.Add("EndProgram");
        aiTypeStrArr.Add("--none--");

        alignStrArr.Empty();
    }
    else if(key == "select")
    {
        aiTypeStrArr.Add("end select");
        aiTypeStrArr.Add("endselect");
        aiTypeStrArr.Add("EndSelect");
        aiTypeStrArr.Add("--none--");
    }
    else if(key == "type")
    {
        aiTypeStrArr.Add("end type");
        aiTypeStrArr.Add("endtype");
        aiTypeStrArr.Add("EndType");
        aiTypeStrArr.Add("--none--");

        alignStrArr.Empty();
    }
    else if(key == "where")
    {
        aiTypeStrArr.Add("end where");
        aiTypeStrArr.Add("endwhere");
        aiTypeStrArr.Add("EndWhere");
        aiTypeStrArr.Add("--none--");
    }
    else if(key == "enum")
    {
        aiTypeStrArr.Add("end enum");
        aiTypeStrArr.Add("endenum");
        aiTypeStrArr.Add("EndEnum");
        aiTypeStrArr.Add("--none--");

        alignStrArr.Empty();
        addNameEnabled = false;
    }
    else if(key == "forall")
    {
        aiTypeStrArr.Add("end forall");
        aiTypeStrArr.Add("endforall");
        aiTypeStrArr.Add("EndForall");
        aiTypeStrArr.Add("--none--");
    }
    else if(key == "submodule")
    {
        aiTypeStrArr.Add("end submodule");
        aiTypeStrArr.Add("endsubmodule");
        aiTypeStrArr.Add("EndSubmodule");
        aiTypeStrArr.Add("--none--");

        alignStrArr.Empty();
    }
    else if(key == "team")
    {
        aiTypeStrArr.Add("end team");
        aiTypeStrArr.Add("endteam");
        aiTypeStrArr.Add("EndTeam");
        aiTypeStrArr.Add("--none--");
    }
    return true;
}

AutoInsert::AutoInsertType AutoInsert::GetAIT(int aiT)
{
    if (aiT == 0)
        return aitSeparate;
    else if (aiT == 1)
        return aitTogether;
    else if (aiT == 2)
        return aitTogetherCap;

    return aitNone;
}

int AutoInsert::GetAITInt(AutoInsert::AutoInsertType aiT)
{
    if (aiT == aitSeparate)
        return 0;
    else if (aiT == aitTogether)
        return 1;
    else if (aiT == aitTogetherCap)
        return 2;

    return 3;
}

wxString AutoInsert::FindKey(const wxString& statementName)
{
    std::map<wxString,wxString>::const_iterator it;
    wxString key;

    for (it = m_NameMap.begin(); it != m_NameMap.end(); ++it)
    {
        if (it->second == statementName)
        {
            key = it->first;
            break;
        }
    }
    return key;
}

void AutoInsert::MakeAutoInsert(cbEditor* ed)
{
    // Function should be called after 'Enter' only.
    // Is assumed that current line is empty (or only spaces).
    cbStyledTextCtrl* stc = ed->GetControl();
    if (!stc)
        return;

    FortranSourceForm fsForm;
    if (!g_FortranFileExt.IsFileFortran(ed->GetFilename(), fsForm))
        return;
    if (fsForm == fsfFixed)
        return;
    if(!IsAtLineEnd(stc))
        return;
    wxString lineStr;
    GetLine(stc, lineStr);
    if (lineStr.IsEmpty())
        return;

    wxString firstName;
    int firstNameIndent;
    int keyStartPos;
    int keyIndent;
    if (!GetIndentAndPos(stc, lineStr, firstName, firstNameIndent, keyStartPos, keyIndent)
        && !(lineStr.length() >= 4 && lineStr.Mid(lineStr.length()-4).Lower().IsSameAs("then")))
        return;

    wxString statementLineStr = lineStr.Mid(keyStartPos);
    wxString word = GetWord(statementLineStr,0);
    wxString wordLw = word.Lower();
    if (wordLw.IsSameAs("end"))
        return;

    std::map<wxString,wxString>::const_iterator it;
    wxString key;

    for (it = m_NameMap.begin(); it != m_NameMap.end(); ++it)
    {
        if (wordLw.IsSameAs(it->first))
        {
            key = it->first;
            break;
        }
    }

    wxString statementLineStrLw = statementLineStr.Lower();
    if (key.empty() ||
        key.IsSameAs("type")) // situation: "type(tname) function myfunc(..."
    {
        wxString funw = "function";
        int wstart = statementLineStrLw.Find(funw);
        if (wstart != wxNOT_FOUND)
        {
            wxString funword = GetWord(statementLineStrLw,wstart);
            if (funword.IsSameAs(funw))
            {
                key = funw;
                keyStartPos += wstart;
                word = GetWord(statementLineStr,wstart);
            }
        }
    }

    if ( key.IsSameAs("module") || // take care for "module subroutine Name()"
         (key.empty() &&
          (wordLw.IsSameAs("pure") || wordLw.IsSameAs("impure")
           || wordLw.IsSameAs("elemental")
           || wordLw.IsSameAs("recursive") || wordLw.IsSameAs("non_recursive"))) )
    {
        wxString reststr = statementLineStrLw.Mid(wordLw.length()+1).Trim(false);
        wxString secword = GetWord(reststr,0);
        if (secword.IsSameAs("procedure"))
            return;
        else if (secword.IsSameAs("subroutine") || secword.IsSameAs("function"))
        {
            key = secword;
            int wstart = statementLineStrLw.Find(secword);
            keyStartPos += wstart;
            word = GetWord(statementLineStr,wstart);
        }
    }

    if (key.empty() || key.IsSameAs("if"))
    {
        wxString thw = "then";
        if (statementLineStrLw.EndsWith(thw))
        {
            wxString thword = GetWord(statementLineStrLw,statementLineStrLw.length()-4);
            if (thword.IsSameAs(thw))
            {
                GetFortranLine(stc, lineStr);
                if (!GetIndentAndPos(stc, lineStr, firstName, firstNameIndent, keyStartPos, keyIndent))
                    return;
                word = GetWord(lineStr, keyStartPos);
                wordLw = word.Lower();
                if (wordLw.IsSameAs("if"))
                    key = "if";
            }
        }
        else
            key = wxEmptyString;
    }

    if (key.empty() && wordLw.IsSameAs("change"))
    {
        wxString secword = GetWord(statementLineStrLw,7);
        if (secword.IsSameAs("team"))
            key = "team";
    }

    if (key.IsEmpty())
        return;

    wxString lineRest = lineStr.Mid(keyStartPos+key.Len()).Trim(false);
    if (lineRest.empty() && !key.IsSameAs("interface") && !key.IsSameAs("block") && !key.IsSameAs("critical")
        && !key.IsSameAs("do") && !key.IsSameAs("program"))
        return; // unfinished statements or something else

    if (key.IsSameAs("team"))
    {
        lineRest = lineStr.Mid(keyStartPos+6).Trim(false).Mid(4).Trim(false);
    }

    if (key.IsSameAs("where") || key.IsSameAs("forall") || key.IsSameAs("team"))
    {
        if (!lineRest.StartsWith("("))
            return; // something is wrong with syntax
        else
        {
            int cl = FindEndBracket(lineRest,0);
            if (cl == wxNOT_FOUND)
                return; // we don't consider case when "where ( bla bla"
            else if (cl+1 < int(lineRest.length()))
                return; // there are some symbols after "where (bla bla)". It is not "where" construct
        }
    }
    else if (key.IsSameAs("type"))
    {
        if (lineRest.StartsWith("("))
            return; // here is declaration
        if (GetWord(lineRest,0).Lower().IsSameAs("is"))
            return; // "type is ..." statement
    }
    else if (key.empty() || m_AITMap[key] == aitNone)
        return;

    if (!DoEndStatementIsRequired(stc, key))
        return;

    wxString addStr;
    if (m_AITMap[key] == aitSeparate)
    {
        if (islower(word.GetChar(0)))
            addStr << "end " << key;
        else if (islower(word.GetChar(1)))
            addStr << "End " << key.Mid(0,1).Upper() << key.Mid(1);
        else
            addStr << "END " << key.Upper();
    }
    else if (m_AITMap[key] == aitTogether)
    {
        if (islower(word.GetChar(0)))
            addStr << "end" << key;
        else if (islower(word.GetChar(1)))
            addStr << "End" << key;
        else
            addStr << "END" << key.Upper();
    }
    else if (m_AITMap[key] == aitTogetherCap)
    {
        if (islower(word.GetChar(0)))
            addStr << "end" << key;
        else if (islower(word.GetChar(1)))
            addStr << "End" << key.Mid(0,1).Upper() << key.Mid(1);
        else
            addStr << "END" << key.Upper();
    }

    if (m_DoAddNameMap[key])
    {
        if (   key.IsSameAs("subroutine") || key.IsSameAs("function")
            || key.IsSameAs("program") || key.IsSameAs("module")
            || key.IsSameAs("submodule") || key.IsSameAs("interface"))
        {
            wxString name = GetWord(lineRest,0);
            if (name.length() > 0 && (isalnum(name.GetChar(0)) || (name.GetChar(0) == '_')))
                addStr << " " << name;
        }
        else if (key.IsSameAs("type"))
        {
            if (lineRest.StartsWith(","))
            {
                int idx = lineRest.Find("::");
                if (idx != wxNOT_FOUND)
                    lineRest = lineRest.Mid(idx).Trim(false);
                else
                    return; // something is wrong
            }
            wxString name = GetWord(lineRest,0);
            if (name.length() > 0 && (isalnum(name.GetChar(0)) || (name.GetChar(0) == '_')))
                addStr << " " << name;
        }
        else if (!firstName.empty() &&
            (  key.IsSameAs("do") || key.IsSameAs("if")
            || key.IsSameAs("associate") || key.IsSameAs("block")
            || key.IsSameAs("critical") || key.IsSameAs("select")
            || key.IsSameAs("forall") || key.IsSameAs("where")
            || key.IsSameAs("team") )
                 )
        {
            addStr << " " << firstName;
        }
    }

    int nspace = 0;
    if (!m_AlignTSMap[key] && !firstName.empty() &&
        (  key.IsSameAs("do") || key.IsSameAs("if")
        || key.IsSameAs("associate") || key.IsSameAs("block")
        || key.IsSameAs("critical") || key.IsSameAs("select")
        || key.IsSameAs("forall") || key.IsSameAs("where")
        || key.IsSameAs("team") )
        )
    {
        nspace = firstNameIndent;
    }
    else
        nspace = keyIndent;

    // Insert
    wxString spacStr;
    spacStr.Append(' ',nspace);
    addStr.Prepend("\n"+spacStr);

    stc->InsertText(stc->GetCurrentPos(),addStr);
}

wxString AutoInsert::GetWord(const wxString& line, size_t posStart)
{
    bool found = false;
    wxString wordBefore;
    size_t idx = 0;
    if (posStart > 0)
    {
        for (size_t i=posStart-1; true; i--)
        {
            if (!isalnum(line.GetChar(i)) && (line.GetChar(i) != '_'))
            {
                found = true;
                idx = i+1;
                break;
            }
            else if (i == 0)
                break;
        }
        if (found)
            wordBefore = line.Mid(idx,posStart-idx);
        else
            wordBefore = line.Mid(0,posStart);
    }

    found = false;
    for (size_t i=posStart; i<line.length(); i++)
    {
        if (!isalnum(line.GetChar(i)) && (line.GetChar(i) != '_'))
        {
            found = true;
            idx = i;
            break;
        }
    }

    if (found)
        return wordBefore+line.Mid(posStart,idx-posStart);
    return wordBefore+line.Mid(posStart);
}

int AutoInsert::FindEndBracket(const wxString str, size_t istart) const
{
    int level = 0;
    for (size_t i=istart; i<str.length(); i++)
    {
        if (str.GetChar(i) == '(')
            level+=1;
        else if (str.GetChar(i) == ')')
        {
            level-=1;
            if (level == 0)
                return int(i);
        }
    }
    return wxNOT_FOUND;
}

bool AutoInsert::DoEndStatementIsRequired(cbStyledTextCtrl* stc, const wxString& key)
{
    if (m_RegMap.count(key) < 1)
        return false;
    wxRegEx* reCur = m_RegMap[key];
    wxRegEx* reEndCur1 = m_RegMap["end" + key];
    wxRegEx* reEndCur2 = NULL;

    wxRegEx* reFinish1 = NULL;
    wxRegEx* reFinish2 = NULL;
    wxRegEx* reFinBack1 = NULL;
    wxRegEx* reFinBack2 = NULL;
    wxRegEx* reFinBack3 = NULL;
    bool isSubprog = false;
    bool noLevels = false;
    if (key.IsSameAs("if") || key.IsSameAs("do")
        || key.IsSameAs("associate") || key.IsSameAs("block") || key.IsSameAs("critical")
        || key.IsSameAs("select") || key.IsSameAs("where")
        || key.IsSameAs("forall") || key.IsSameAs("change"))
    {
        // limit search until the end of unit
        reFinish1 = m_RegMap["end"];
        reFinish2 = m_RegMap["endunit"];
        reFinBack1 = m_RegMap["function"];
        reFinBack2 = m_RegMap["subroutine"];
        reFinBack3 = m_RegMap["program"];
    }
    else if (key.IsSameAs("function") || key.IsSameAs("subroutine"))
    {
        reEndCur2 = m_RegMap["end"];
        reFinish1 = m_RegMap["endmodule"];
        reFinish2 = m_RegMap["endsubmodule"];
        isSubprog = true;
    }
    else if (key.IsSameAs("module") || key.IsSameAs("submodule") || key.IsSameAs("program"))
    {
        reEndCur2 = m_RegMap["end"];
        reFinish1 = m_RegMap["module"];
        reFinish2 = m_RegMap["submodule"];
        isSubprog = true;
    }
    else if (key.IsSameAs("type") || key.IsSameAs("enum"))
    {
        noLevels = true;
        reFinish1 = m_RegMap["end"];
        reFinish2 = m_RegMap["endunit"];
    }

    bool isIf = false;
    if (key.IsSameAs("if"))
        isIf = true;

    int line = stc->LineFromPosition(stc->GetCurrentPos()) + 1;
    int lcount = stc->GetLineCount();
    lcount = std::min(lcount,line+10000); // limit search for very long files
    wxString str;

    if (isSubprog)
    {
        while (line < lcount)
        {
            str = stc->GetLine(line);

            if (   (reFinish1 && reFinish1->Matches(str))
                || (reFinish2 && reFinish2->Matches(str)))
                break;
            else if (m_RegMap["function"]->Matches(str) || m_RegMap["subroutine"]->Matches(str)
                        || m_RegMap["module"]->Matches(str) || m_RegMap["submodule"]->Matches(str)
                        || m_RegMap["program"]->Matches(str) || m_RegMap["endinterface"]->Matches(str))
                break;
            else if (reEndCur1->Matches(str) || (reEndCur2 && reEndCur2->Matches(str)))
            {
                return false;
            }
            else
            {
                str = str.BeforeFirst('!').Trim();
                if (!str.IsEmpty())
                    return false;
            }
            line += 1;
        }
        return true;
    }
    else if (noLevels)
    {
        while (line < lcount)
        {
            str = stc->GetLine(line);

            if (   (reFinish1 && reFinish1->Matches(str))
                || (reFinish2 && reFinish2->Matches(str)))
                break;
            else if (m_RegMap["function"]->Matches(str) || m_RegMap["subroutine"]->Matches(str)
                        || m_RegMap["module"]->Matches(str) || m_RegMap["submodule"]->Matches(str)
                        || m_RegMap["program"]->Matches(str))
                break;
            else if (   m_RegMap["type"]->Matches(str) || m_RegMap["enum"]->Matches(str)
                     || m_RegMap["interface"]->Matches(str))
                break;
            else if (reEndCur1->Matches(str))
                return false;
            else if (   m_RegMap["if"]->Matches(str) || m_RegMap["do"]->Matches(str)
                     || m_RegMap["associate"]->Matches(str) || m_RegMap["block"]->Matches(str)
                     || m_RegMap["critical"]->Matches(str) || m_RegMap["select"]->Matches(str)
                     || m_RegMap["where"]->Matches(str) || m_RegMap["forall"]->Matches(str))
            {
                break;
            }
            line += 1;
        }
        return true;
    }
    else if (key.IsSameAs("interface"))
    {
        wxRegEx* reFin1 = m_RegMap["contains"];
        wxRegEx* reFin2 = m_RegMap["do"];
        wxRegEx* reFin3 = m_RegMap["if"];
        wxRegEx* reFin4 = m_RegMap["interface"];
        //wxRegEx* reFin5 = m_RegMap["module"];
        wxRegEx* reFin6 = m_RegMap["submodule"];
        wxRegEx* reFin7 = m_RegMap["program"];
        wxRegEx* reFin8 = m_RegMap["block"];
        wxRegEx* reFin9 = m_RegMap["critical"];
        wxRegEx* reFin10 = m_RegMap["associate"];
        wxRegEx* reFin11 = m_RegMap["block"];
        wxRegEx* reFin12 = m_RegMap["select"];
        wxRegEx* reFin13 = m_RegMap["where"];
        wxRegEx* reFin14 = m_RegMap["forall"];
        wxRegEx* reFin15 = m_RegMap["team"];

        while (line < lcount)
        {
            str = stc->GetLine(line);

            if (reFin1->Matches(str) || reFin2->Matches(str) || reFin3->Matches(str) || reFin4->Matches(str)
                || reFin6->Matches(str) || reFin7->Matches(str) || reFin8->Matches(str)
                || reFin9->Matches(str) || reFin10->Matches(str) || reFin11->Matches(str) || reFin12->Matches(str)
                || reFin13->Matches(str) || reFin14->Matches(str) || reFin15->Matches(str)
                )
                break;
            else if (reEndCur1->Matches(str))
                return false;

            line += 1;
        }
        return true;
    }

    //Determine level below
    int level = 0;
    while (line < lcount)
    {
        if (isIf)
        {
            GetLine(stc, str, line);
            if (str.Len() >= 4)
            {
                wxString eth = str.Mid(str.Len()-4,4).Lower();
                if (eth.IsSameAs("then"))
                    GetFortranLine(stc,str,line);
            }
        }
        else
            str = stc->GetLine(line);

        if (   (reFinish1 && reFinish1->Matches(str))
                 || (reFinish2 && reFinish2->Matches(str)))
            break;
        else if (reCur->Matches(str))
            level += 1;
        else if (reEndCur1->Matches(str) || (reEndCur2 && reEndCur2->Matches(str)))
        {
            level -= 1;
        }
        line += 1;
    }

    if (level == 0)
        return true;

    //Determine level above
    int level_down = level;
    level = 0;
    line = stc->LineFromPosition(stc->GetCurrentPos()) - 2;
    int lfin = std::max(0,line-1000); // limit search for very long subprograms
    while (line >= lfin)
    {
        if (isIf)
        {
            GetLine(stc, str, line);
            if (str.Len() >= 4)
            {
                wxString eth = str.Mid(str.Len()-4,4).Lower();
                if (eth.IsSameAs("then"))
                    GetFortranLine(stc,str,line);
            }
        }
        else
            str = stc->GetLine(line);

        if (   (reFinish1 && reFinish1->Matches(str))
                 || (reFinish2 && reFinish2->Matches(str)))
            break;
        else if (   (reFinBack1 && reFinBack1->Matches(str))
                 || (reFinBack2 && reFinBack2->Matches(str))
                 || (reFinBack3 && reFinBack3->Matches(str)) )
            break;
        else if (reCur->Matches(str))
            level += 1;
        else if (reEndCur1->Matches(str) || (reEndCur2 && reEndCur2->Matches(str)))
        {
            level -= 1;
        }
        line -= 1;
    }
    if ((level_down+level) != 0)
        return false;
    return true;
}

bool AutoInsert::GetIndentAndPos(cbStyledTextCtrl* stc, const wxString& lineStr, wxString& firstName, int& firstNameIndent, int& keyStartPos, int& keyIndent)
{
    bool inLabel = false;
    bool wasCh = false;
    bool inName = false;
    bool wasInName = false;
    bool haveNameEnd = false;
    int nsPos = -1;
    int nfPos = 0;
    int curIndent = 0;
    firstName = wxEmptyString;
    firstNameIndent = 0;
    keyStartPos = 0;
    keyIndent = 0;
    size_t lineStrLen = lineStr.length();

    for (size_t i=0; i<lineStrLen; i++)
    {
        wxChar ch = lineStr.GetChar(i);
        if (!wasCh)
        {
            if (ch == '\n')
                firstNameIndent = 0;
            else if (ch == '\t')
                firstNameIndent += stc->GetTabWidth();
            else if (isdigit(ch) || ch == ' ')
                firstNameIndent += 1;
        }

        if (ch == '\n')
            curIndent = 0;
        else
            curIndent += 1;

        if (!wasCh && isdigit(ch))
        {
            inLabel = true;
        }
        else if (inLabel)
        {
            if (ch == ' ' || ch == '\t')
                inLabel = false;
            else if (!isdigit(ch))
                return false; // something is wrong
        }
        else if ((inName || wasInName) && (ch == ':'))
        {
            if (i+1<lineStrLen && lineStr.GetChar(i+1) == ':')
                break;
            else
            {
                haveNameEnd = true;
                nfPos = i;
                break;
            }
        }
        else if (inName && !isalnum(ch) && (ch != '_'))
        {
            inName = false;
            if (isblank(ch))
                wasInName = true;
            else
                break;
        }
        else if (!inLabel && !inName && !wasInName && (isalpha(ch) || (ch == '_')))
        {
            wasCh = true;
            inName = true;
            nsPos = i;
        }
        else if (wasInName && !isblank(ch))
            break;
        else if (!isalnum(ch) && (ch != '_') && (ch != '&') && !isblank(ch))
            break;
    }

    wxString statementLineStr;
    if (haveNameEnd)
    {
        firstName = lineStr.Mid(nsPos,nfPos-nsPos).Trim();
        keyIndent = curIndent;
        for (size_t i=nfPos+1; i<lineStrLen; i++)
        {
            wxChar ch = lineStr.GetChar(i);
            if ( (ch == '\n') || ( (stc->GetEOLMode() == wxSCI_EOL_CR) && (ch == '\r') ) )
                keyIndent = 0;
            else if (ch == '\t')
                keyIndent += stc->GetTabWidth();
            else if (ch == ' ')
                keyIndent += 1;
            else
            {
                keyStartPos = i;
                break;
            }
        }
        if (keyStartPos == 0)
            return false;
    }
    else
    {
        if (nsPos == -1)
            return false;

        keyIndent = firstNameIndent;
        keyStartPos = nsPos;
    }
    return true;
}

bool AutoInsert::IsAtLineEnd(cbStyledTextCtrl* stc)
{
    int pos = stc->GetCurrentPos();
    int line = stc->LineFromPosition(pos);
    int posLE = stc->GetLineEndPosition(line);
    wxString str = stc->GetTextRange(pos, posLE).Trim();
    return str.IsEmpty();
}

void AutoInsert::GetLine(cbStyledTextCtrl* stc, wxString& lineStr, int line)
{
    if (line == -1)
    {
        int pos = stc->GetCurrentPos();
        line = stc->LineFromPosition(pos) - 1;
    }
    int posLS = stc->PositionFromLine(line);
    int posLE = stc->GetLineEndPosition(line);
    for (int i=posLS; i<posLE; i++)
    {
        int style = stc->GetStyleAt(i);
        if (style == wxSCI_F_COMMENT)
        {
            posLE = i; // here begins comment
            break;
        }
    }
    lineStr = stc->GetTextRange(posLS, posLE).Trim();
}

void AutoInsert::GetFortranLine(cbStyledTextCtrl* stc, wxString& lineStr, int line)
{
    if (line == -1)
    {
        int pos = stc->GetCurrentPos();
        line = stc->LineFromPosition(pos) - 1;
    }
    GetLine(stc, lineStr, line);

    for (int i=line-1; i>0; i--)
    {
        wxString lineStrBefore;
        GetLine(stc, lineStrBefore, i);
        if (lineStrBefore.EndsWith("&"))
        {
            lineStr.Prepend(lineStrBefore.Mid(0, lineStrBefore.length()-1).Append(" \n"));
        }
        else
            break;
    }
}

void AutoInsert::ReadAIOptions()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");
    std::map<wxString,wxString>::const_iterator it;

    for (it = m_NameMap.begin(); it != m_NameMap.end(); ++it)
    {
        wxString key = it->first;
        wxString strType = "/ainsert_type_" + key;
        wxString strAlign = "/ainsert_align_" + key;
        wxString strName = "/ainsert_name_" + key;

        int aiTInt = cfg->ReadInt(strType, 0);
        m_AITMap[key] = GetAIT(aiTInt);
        m_AlignTSMap[key] = cfg->ReadBool(strAlign, true);
        m_DoAddNameMap[key] = cfg->ReadBool(strName, false);
    }
}

void AutoInsert::WriteAIOptions()
{
    if (!m_RulesWereChanged)
        return;

    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");
    std::map<wxString,AutoInsertType>::const_iterator it;

    for (it = m_AITMap.begin(); it != m_AITMap.end(); ++it)
    {
        wxString key = it->first;
        wxString strType = "/ainsert_type_" + key;
        wxString strAlign = "/ainsert_align_" + key;
        wxString strName = "/ainsert_name_" + key;

        int aiTInt = GetAITInt(m_AITMap[key]);
        cfg->Write(strType, aiTInt);
        cfg->Write(strAlign, m_AlignTSMap[key]);
        cfg->Write(strName, m_DoAddNameMap[key]);
    }
}



