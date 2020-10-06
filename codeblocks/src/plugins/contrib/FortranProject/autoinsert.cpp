#include "autoinsert.h"

#ifndef CB_PRECOMP
    #include <configmanager.h>
#endif
#include <algorithm>

#include "fortranfileext.h"

extern FortranFileExt g_FortranFileExt;

AutoInsert::AutoInsert()
{
    // ctor
    m_NameMap[_T("if")] = _T("if (...) then");
    m_NameMap[_T("do")] = _T("do ...");
    m_NameMap[_T("subroutine")] = _T("subroutine");
    m_NameMap[_T("function")] = _T("function");
    m_NameMap[_T("interface")] = _T("interface");
    m_NameMap[_T("associate")] = _T("associate");
    m_NameMap[_T("block")] = _T("block");
    m_NameMap[_T("critical")] = _T("critical");
    m_NameMap[_T("module")] = _T("module");
    m_NameMap[_T("program")] = _T("program");
    m_NameMap[_T("select")] = _T("select ...");
    m_NameMap[_T("type")] = _T("type");
    m_NameMap[_T("where")] = _T("where (...)");
    m_NameMap[_T("enum")] = _T("enum");
    m_NameMap[_T("forall")] = _T("forall (...)");
    m_NameMap[_T("submodule")] = _T("submodule");
    m_NameMap[_T("team")] = _T("change team (...)");

    int opt = wxRE_ADVANCED | wxRE_ICASE | wxRE_NOSUB;
    m_RegMap[_T("if")] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*):(\\s*))?(if)(\\s*)(\\(.*\\))(\\s*)then\\y.*"), opt);
    m_RegMap[_T("endif")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(if)\\y"), opt);
    m_RegMap[_T("do")] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(do)(\\y)"), opt);
    m_RegMap[_T("enddo")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(do)\\y"), opt);
    m_RegMap[_T("subroutine")] = new wxRegEx(_T("(.*)subroutine(\\s+)([a-z0-9_]+)(\\s*)(\\(.*[\\)&]+)"), opt);
    m_RegMap[_T("endsubroutine")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(subroutine)\\y"), opt);
    m_RegMap[_T("function")] = new wxRegEx(_T("(.*)function(\\s+)([a-z0-9_]+)(\\s*)(\\(.*[\\)&]+)"), opt);
    m_RegMap[_T("endfunction")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(function)\\y"), opt);
    m_RegMap[_T("interface")] = new wxRegEx(_T("^[\\s\\t]*(abstract(\\s*))?interface(\\y)"), opt);
    m_RegMap[_T("endinterface")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(interface)\\y"), opt);
    m_RegMap[_T("associate")] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(associate)(\\s*)(\\(.*\\))"), opt);
    m_RegMap[_T("endassociate")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(associate)\\y"), opt);
    m_RegMap[_T("block")] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(block)\\y"), opt);
    m_RegMap[_T("endblock")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(block)\\y"), opt);
    m_RegMap[_T("critical")] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(critical)\\y"), opt);
    m_RegMap[_T("endcritical")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(critical)\\y"), opt);
    m_RegMap[_T("module")] = new wxRegEx(_T("^[\\s\\t]*(module)(\\s+)((?!procedure[\\s:]+)[a-z0-9_]+)\\y"), opt);
    m_RegMap[_T("endmodule")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(module)\\y"), opt);
    m_RegMap[_T("program")] = new wxRegEx(_T("^[\\s\\t]*program\\y"), opt);
    m_RegMap[_T("endprogram")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(program)\\y"), opt);
    m_RegMap[_T("select")] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(select)(\\s*)(case|type)(\\s*)(\\(.*\\))"), opt);
    m_RegMap[_T("endselect")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(select)\\y"), opt);
    m_RegMap[_T("type")] = new wxRegEx(_T("^([\\s\\t]*)(type)(\\s*)((\\s*,\\s*([a-z0-9_]+))*\\s*::)?(\\s*)([a-z0-9_]+)\\y"), opt);
    m_RegMap[_T("endtype")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(type)\\y"), opt);
    m_RegMap[_T("where")] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(where)(\\s*)(\\(.*\\))(\\s*)(!(.*))?$"), opt);
    m_RegMap[_T("endwhere")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(where)\\y"), opt);
    m_RegMap[_T("enum")] = new wxRegEx(_T("^[\\s\\t]*enum\\y"), opt);
    m_RegMap[_T("endenum")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(enum)\\y"), opt);
    m_RegMap[_T("forall")] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(forall)(\\s*)(\\(.*\\))(\\s*)(!(.*))?$"), opt);
    m_RegMap[_T("endforall")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(forall)\\y"), opt);
    m_RegMap[_T("submodule")] = new wxRegEx(_T("^[\\s\\t]*(submodule)(\\s*)(\\([a-z0-9_]+\\))(\\s*)([a-z0-9_]+)"), opt);
    m_RegMap[_T("endsubmodule")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(submodule)\\y"), opt);
    m_RegMap[_T("team")] = new wxRegEx(_T("^([\\s\\t]*)([0-9]*)([\\s\\t]*)(([a-z0-9_]+)(\\s*)(:)(\\s*))?(change)(\\s*)(team)(\\s*)(\\(.*\\))"), opt);
    m_RegMap[_T("endteam")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(team)\\y"), opt);

    m_RegMap[_T("end")] = new wxRegEx(_T("^[\\s\\t]*[0-9]*[\\s\\t]*\\y(end)\\y(([\\s\\t]*)!(.*))?([\\s\\t]*)$"), opt);
    m_RegMap[_T("endunit")] = new wxRegEx(_T("^[\\s\\t]*(end)(\\s*)(subroutine|function|program)\\y"), opt);
    m_RegMap[_T("contains")] = new wxRegEx(_T("^[\\s\\t]*(contains)([\\s\\t]*)$"), opt);

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
    if (!statm.IsEmpty() && m_NameMap.count(statm) > 0)
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

    alignStrArr.Add(_T("to statement"));
    alignStrArr.Add(_T("to name"));

    if (key == _T("if"))
    {
        aiTypeStrArr.Add(_T("end if"));
        aiTypeStrArr.Add(_T("endif"));
        aiTypeStrArr.Add(_T("EndIf"));
        aiTypeStrArr.Add(_T("--none--"));
    }
    else if(key == _T("do"))
    {
        aiTypeStrArr.Add(_T("end do"));
        aiTypeStrArr.Add(_T("enddo"));
        aiTypeStrArr.Add(_T("EndDo"));
        aiTypeStrArr.Add(_T("--none--"));
    }
    else if(key == _T("subroutine"))
    {
        aiTypeStrArr.Add(_T("end subroutine"));
        aiTypeStrArr.Add(_T("endsubroutine"));
        aiTypeStrArr.Add(_T("EndSubroutine"));
        aiTypeStrArr.Add(_T("--none--"));

        alignStrArr.Empty();
    }
    else if(key == _T("function"))
    {
        aiTypeStrArr.Add(_T("end function"));
        aiTypeStrArr.Add(_T("endfunction"));
        aiTypeStrArr.Add(_T("EndFunction"));
        aiTypeStrArr.Add(_T("--none--"));

        alignStrArr.Empty();
    }
    else if(key == _T("interface"))
    {
        aiTypeStrArr.Add(_T("end interface"));
        aiTypeStrArr.Add(_T("endinterface"));
        aiTypeStrArr.Add(_T("EndInterface"));
        aiTypeStrArr.Add(_T("--none--"));

        alignStrArr.Empty();
    }
    else if(key == _T("associate"))
    {
        aiTypeStrArr.Add(_T("end associate"));
        aiTypeStrArr.Add(_T("endassociate"));
        aiTypeStrArr.Add(_T("EndAssociate"));
        aiTypeStrArr.Add(_T("--none--"));
    }
    else if(key == _T("block"))
    {
        aiTypeStrArr.Add(_T("end block"));
        aiTypeStrArr.Add(_T("endblock"));
        aiTypeStrArr.Add(_T("EndBlock"));
        aiTypeStrArr.Add(_T("--none--"));
    }
    else if(key == _T("critical"))
    {
        aiTypeStrArr.Add(_T("end critical"));
        aiTypeStrArr.Add(_T("endcritical"));
        aiTypeStrArr.Add(_T("EndCritical"));
        aiTypeStrArr.Add(_T("--none--"));
    }
    else if(key == _T("module"))
    {
        aiTypeStrArr.Add(_T("end module"));
        aiTypeStrArr.Add(_T("endmodule"));
        aiTypeStrArr.Add(_T("EndModule"));
        aiTypeStrArr.Add(_T("--none--"));

        alignStrArr.Empty();
    }
    else if(key == _T("program"))
    {
        aiTypeStrArr.Add(_T("end program"));
        aiTypeStrArr.Add(_T("endprogram"));
        aiTypeStrArr.Add(_T("EndProgram"));
        aiTypeStrArr.Add(_T("--none--"));

        alignStrArr.Empty();
    }
    else if(key == _T("select"))
    {
        aiTypeStrArr.Add(_T("end select"));
        aiTypeStrArr.Add(_T("endselect"));
        aiTypeStrArr.Add(_T("EndSelect"));
        aiTypeStrArr.Add(_T("--none--"));
    }
    else if(key == _T("type"))
    {
        aiTypeStrArr.Add(_T("end type"));
        aiTypeStrArr.Add(_T("endtype"));
        aiTypeStrArr.Add(_T("EndType"));
        aiTypeStrArr.Add(_T("--none--"));

        alignStrArr.Empty();
    }
    else if(key == _T("where"))
    {
        aiTypeStrArr.Add(_T("end where"));
        aiTypeStrArr.Add(_T("endwhere"));
        aiTypeStrArr.Add(_T("EndWhere"));
        aiTypeStrArr.Add(_T("--none--"));
    }
    else if(key == _T("enum"))
    {
        aiTypeStrArr.Add(_T("end enum"));
        aiTypeStrArr.Add(_T("endenum"));
        aiTypeStrArr.Add(_T("EndEnum"));
        aiTypeStrArr.Add(_T("--none--"));

        alignStrArr.Empty();
        addNameEnabled = false;
    }
    else if(key == _T("forall"))
    {
        aiTypeStrArr.Add(_T("end forall"));
        aiTypeStrArr.Add(_T("endforall"));
        aiTypeStrArr.Add(_T("EndForall"));
        aiTypeStrArr.Add(_T("--none--"));
    }
    else if(key == _T("submodule"))
    {
        aiTypeStrArr.Add(_T("end submodule"));
        aiTypeStrArr.Add(_T("endsubmodule"));
        aiTypeStrArr.Add(_T("EndSubmodule"));
        aiTypeStrArr.Add(_T("--none--"));

        alignStrArr.Empty();
    }
    else if(key == _T("team"))
    {
        aiTypeStrArr.Add(_T("end team"));
        aiTypeStrArr.Add(_T("endteam"));
        aiTypeStrArr.Add(_T("EndTeam"));
        aiTypeStrArr.Add(_T("--none--"));
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
        && !(lineStr.Length() >= 4 && lineStr.Mid(lineStr.Length()-4).Lower().IsSameAs(_T("then"))))
        return;

    wxString statementLineStr = lineStr.Mid(keyStartPos);
    wxString word = GetWord(statementLineStr,0);
    wxString wordLw = word.Lower();
    if (wordLw.IsSameAs(_T("end")))
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
    if (key.IsEmpty() ||
        key.IsSameAs(_T("type"))) // situation: "type(tname) function myfunc(..."
    {
        wxString funw = _T("function");
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

    if ( key.IsSameAs(_T("module")) || // take care for "module subroutine Name()"
         (key.IsEmpty() &&
          (wordLw.IsSameAs(_T("pure")) || wordLw.IsSameAs(_T("impure"))
           || wordLw.IsSameAs(_T("elemental"))
           || wordLw.IsSameAs(_T("recursive")) || wordLw.IsSameAs(_T("non_recursive")))) )
    {
        wxString reststr = statementLineStrLw.Mid(wordLw.Length()+1).Trim(false);
        wxString secword = GetWord(reststr,0);
        if (secword.IsSameAs(_T("procedure")))
            return;
        else if (secword.IsSameAs(_T("subroutine")) || secword.IsSameAs(_T("function")))
        {
            key = secword;
            int wstart = statementLineStrLw.Find(secword);
            keyStartPos += wstart;
            word = GetWord(statementLineStr,wstart);
        }
    }

    if (key.IsEmpty() || key.IsSameAs(_T("if")))
    {
        wxString thw = _T("then");
        if (statementLineStrLw.EndsWith(thw))
        {
            wxString thword = GetWord(statementLineStrLw,statementLineStrLw.Length()-4);
            if (thword.IsSameAs(thw))
            {
                GetFortranLine(stc, lineStr);
                if (!GetIndentAndPos(stc, lineStr, firstName, firstNameIndent, keyStartPos, keyIndent))
                    return;
                word = GetWord(lineStr, keyStartPos);
                wordLw = word.Lower();
                if (wordLw.IsSameAs(_T("if")))
                    key = _T("if");
            }
        }
        else
            key = wxEmptyString;
    }

    if (key.IsEmpty() && wordLw.IsSameAs(_T("change")))
    {
        wxString secword = GetWord(statementLineStrLw,7);
        if (secword.IsSameAs(_T("team")))
            key = _T("team");
    }

    if (key.IsEmpty())
        return;

    wxString lineRest = lineStr.Mid(keyStartPos+key.Len()).Trim(false);
    if (lineRest.IsEmpty() && !key.IsSameAs(_T("interface")) && !key.IsSameAs(_T("block")) && !key.IsSameAs(_T("critical"))
        && !key.IsSameAs(_T("do")) && !key.IsSameAs(_T("program")))
        return; // unfinished statements or something else

    if (key.IsSameAs(_T("team")))
    {
        lineRest = lineStr.Mid(keyStartPos+6).Trim(false).Mid(4).Trim(false);
    }

    if (key.IsSameAs(_T("where")) || key.IsSameAs(_T("forall")) || key.IsSameAs(_T("team")))
    {
        if (!lineRest.StartsWith(_T("(")))
            return; // something is wrong with syntax
        else
        {
            int cl = FindEndBracket(lineRest,0);
            if (cl == wxNOT_FOUND)
                return; // we don't consider case when "where ( bla bla"
            else if (cl+1 < int(lineRest.Length()))
                return; // there are some symbols after "where (bla bla)". It is not "where" construct
        }
    }
    else if (key.IsSameAs(_T("type")))
    {
        if (lineRest.StartsWith(_T("(")))
            return; // here is declaration
        if (GetWord(lineRest,0).Lower().IsSameAs(_T("is")))
            return; // "type is ..." statement
    }
    else if (key.IsEmpty() || m_AITMap[key] == aitNone)
        return;

    if (!DoEndStatementIsRequired(stc, key))
        return;

    wxString addStr;
    if (m_AITMap[key] == aitSeparate)
    {
        if (islower(word.GetChar(0)))
            addStr << _T("end ") << key;
        else if (islower(word.GetChar(1)))
            addStr << _T("End ") << key.Mid(0,1).Upper() << key.Mid(1);
        else
            addStr << _T("END ") << key.Upper();
    }
    else if (m_AITMap[key] == aitTogether)
    {
        if (islower(word.GetChar(0)))
            addStr << _T("end") << key;
        else if (islower(word.GetChar(1)))
            addStr << _T("End") << key;
        else
            addStr << _T("END") << key.Upper();
    }
    else if (m_AITMap[key] == aitTogetherCap)
    {
        if (islower(word.GetChar(0)))
            addStr << _T("end") << key;
        else if (islower(word.GetChar(1)))
            addStr << _T("End") << key.Mid(0,1).Upper() << key.Mid(1);
        else
            addStr << _T("END") << key.Upper();
    }

    if (m_DoAddNameMap[key])
    {
        if (   key.IsSameAs(_T("subroutine")) || key.IsSameAs(_T("function"))
            || key.IsSameAs(_T("program")) || key.IsSameAs(_T("module"))
            || key.IsSameAs(_T("submodule")) || key.IsSameAs(_T("interface")))
        {
            wxString name = GetWord(lineRest,0);
            if (name.Length() > 0 && (isalnum(name.GetChar(0)) || (name.GetChar(0) == '_')))
                addStr << _T(" ") << name;
        }
        else if (key.IsSameAs(_T("type")))
        {
            if (lineRest.StartsWith(_T(",")))
            {
                int idx = lineRest.Find(_T("::"));
                if (idx != wxNOT_FOUND)
                    lineRest = lineRest.Mid(idx).Trim(false);
                else
                    return; // something is wrong
            }
            wxString name = GetWord(lineRest,0);
            if (name.Length() > 0 && (isalnum(name.GetChar(0)) || (name.GetChar(0) == '_')))
                addStr << _T(" ") << name;
        }
        else if (!firstName.IsEmpty() &&
            (  key.IsSameAs(_T("do")) || key.IsSameAs(_T("if"))
            || key.IsSameAs(_T("associate")) || key.IsSameAs(_T("block"))
            || key.IsSameAs(_T("critical")) || key.IsSameAs(_T("select"))
            || key.IsSameAs(_T("forall")) || key.IsSameAs(_T("where"))
            || key.IsSameAs(_T("team")) )
                 )
        {
            addStr << _T(" ") << firstName;
        }
    }

    int nspace = 0;
    if (!m_AlignTSMap[key] && !firstName.IsEmpty() &&
        (  key.IsSameAs(_T("do")) || key.IsSameAs(_T("if"))
        || key.IsSameAs(_T("associate")) || key.IsSameAs(_T("block"))
        || key.IsSameAs(_T("critical")) || key.IsSameAs(_T("select"))
        || key.IsSameAs(_T("forall")) || key.IsSameAs(_T("where"))
        || key.IsSameAs(_T("team")) )
        )
    {
        nspace = firstNameIndent;
    }
    else
        nspace = keyIndent;

    // Insert
    wxString spacStr;
    spacStr.Append(' ',nspace);
    addStr.Prepend(_T("\n")+spacStr);

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
    for (size_t i=posStart; i<line.Length(); i++)
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
    for (size_t i=istart; i<str.Length(); i++)
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
    wxRegEx* reEndCur1 = m_RegMap[_T("end") + key];
    wxRegEx* reEndCur2 = NULL;

    wxRegEx* reFinish1 = NULL;
    wxRegEx* reFinish2 = NULL;
    wxRegEx* reFinBack1 = NULL;
    wxRegEx* reFinBack2 = NULL;
    wxRegEx* reFinBack3 = NULL;
    bool isSubprog = false;
    bool noLevels = false;
    if (key.IsSameAs(_T("if")) || key.IsSameAs(_T("do"))
        || key.IsSameAs(_T("associate")) || key.IsSameAs(_T("block")) || key.IsSameAs(_T("critical"))
        || key.IsSameAs(_T("select")) || key.IsSameAs(_T("where"))
        || key.IsSameAs(_T("forall")) || key.IsSameAs(_T("change")))
    {
        // limit search until the end of unit
        reFinish1 = m_RegMap[_T("end")];
        reFinish2 = m_RegMap[_T("endunit")];
        reFinBack1 = m_RegMap[_T("function")];
        reFinBack2 = m_RegMap[_T("subroutine")];
        reFinBack3 = m_RegMap[_T("program")];
    }
    else if (key.IsSameAs(_T("function")) || key.IsSameAs(_T("subroutine")))
    {
        reEndCur2 = m_RegMap[_T("end")];
        reFinish1 = m_RegMap[_T("endmodule")];
        reFinish2 = m_RegMap[_T("endsubmodule")];
        isSubprog = true;
    }
    else if (key.IsSameAs(_T("module")) || key.IsSameAs(_T("submodule")) || key.IsSameAs(_T("program")))
    {
        reEndCur2 = m_RegMap[_T("end")];
        reFinish1 = m_RegMap[_T("module")];
        reFinish2 = m_RegMap[_T("submodule")];
        isSubprog = true;
    }
    else if (key.IsSameAs(_T("type")) || key.IsSameAs(_T("enum")))
    {
        noLevels = true;
        reFinish1 = m_RegMap[_T("end")];
        reFinish2 = m_RegMap[_T("endunit")];
    }

    bool isIf = false;
    if (key.IsSameAs(_T("if")))
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
            else if (m_RegMap[_T("function")]->Matches(str) || m_RegMap[_T("subroutine")]->Matches(str)
                        || m_RegMap[_T("module")]->Matches(str) || m_RegMap[_T("submodule")]->Matches(str)
                        || m_RegMap[_T("program")]->Matches(str) || m_RegMap[_T("endinterface")]->Matches(str))
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
            else if (m_RegMap[_T("function")]->Matches(str) || m_RegMap[_T("subroutine")]->Matches(str)
                        || m_RegMap[_T("module")]->Matches(str) || m_RegMap[_T("submodule")]->Matches(str)
                        || m_RegMap[_T("program")]->Matches(str))
                break;
            else if (   m_RegMap[_T("type")]->Matches(str) || m_RegMap[_T("enum")]->Matches(str)
                     || m_RegMap[_T("interface")]->Matches(str))
                break;
            else if (reEndCur1->Matches(str))
                return false;
            else if (   m_RegMap[_T("if")]->Matches(str) || m_RegMap[_T("do")]->Matches(str)
                     || m_RegMap[_T("associate")]->Matches(str) || m_RegMap[_T("block")]->Matches(str)
                     || m_RegMap[_T("critical")]->Matches(str) || m_RegMap[_T("select")]->Matches(str)
                     || m_RegMap[_T("where")]->Matches(str) || m_RegMap[_T("forall")]->Matches(str))
            {
                break;
            }
            line += 1;
        }
        return true;
    }
    else if (key.IsSameAs(_T("interface")))
    {
        wxRegEx* reFin1 = m_RegMap[_T("contains")];
        wxRegEx* reFin2 = m_RegMap[_T("do")];
        wxRegEx* reFin3 = m_RegMap[_T("if")];
        wxRegEx* reFin4 = m_RegMap[_T("interface")];
        //wxRegEx* reFin5 = m_RegMap[_T("module")];
        wxRegEx* reFin6 = m_RegMap[_T("submodule")];
        wxRegEx* reFin7 = m_RegMap[_T("program")];
        wxRegEx* reFin8 = m_RegMap[_T("block")];
        wxRegEx* reFin9 = m_RegMap[_T("critical")];
        wxRegEx* reFin10 = m_RegMap[_T("associate")];
        wxRegEx* reFin11 = m_RegMap[_T("block")];
        wxRegEx* reFin12 = m_RegMap[_T("select")];
        wxRegEx* reFin13 = m_RegMap[_T("where")];
        wxRegEx* reFin14 = m_RegMap[_T("forall")];
        wxRegEx* reFin15 = m_RegMap[_T("team")];

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
                if (eth.IsSameAs(_T("then")))
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
                if (eth.IsSameAs(_T("then")))
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
    size_t lineStrLen = lineStr.Length();

    for (size_t i=0; i<lineStrLen; i++)
    {
        wxChar ch = lineStr.GetChar(i);
        if (!wasCh)
        {
            if (ch == _T('\n'))
                firstNameIndent = 0;
            else if (ch == '\t')
                firstNameIndent += stc->GetTabWidth();
            else if (isdigit(ch) || ch == ' ')
                firstNameIndent += 1;
        }

        if (ch == _T('\n'))
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
            if ( (ch == _T('\n')) || ( (stc->GetEOLMode() == wxSCI_EOL_CR) && (ch == _T('\r')) ) )
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
        if (lineStrBefore.EndsWith(_T("&")))
        {
            lineStr.Prepend(lineStrBefore.Mid(0, lineStrBefore.Length()-1).Append(_T(" \n")));
        }
        else
            break;
    }
}

void AutoInsert::ReadAIOptions()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));
    std::map<wxString,wxString>::const_iterator it;

    for (it = m_NameMap.begin(); it != m_NameMap.end(); ++it)
    {
        wxString key = it->first;
        wxString strType = _T("/ainsert_type_") + key;
        wxString strAlign = _T("/ainsert_align_") + key;
        wxString strName = _T("/ainsert_name_") + key;

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

    ConfigManager* cfg = Manager::Get()->GetConfigManager(_T("fortran_project"));
    std::map<wxString,AutoInsertType>::const_iterator it;

    for (it = m_AITMap.begin(); it != m_AITMap.end(); ++it)
    {
        wxString key = it->first;
        wxString strType = _T("/ainsert_type_") + key;
        wxString strAlign = _T("/ainsert_align_") + key;
        wxString strName = _T("/ainsert_name_") + key;

        int aiTInt = GetAITInt(m_AITMap[key]);
        cfg->Write(strType, aiTInt);
        cfg->Write(strAlign, m_AlignTSMap[key]);
        cfg->Write(strName, m_DoAddNameMap[key]);
    }
}



