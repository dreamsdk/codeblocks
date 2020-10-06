/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */
#include "ccsmartfilter.h"

#ifndef CB_PRECOMP
    #include <wx/string.h>
#endif

#include "tokenf.h"

void CCSmartFilter::GetTokenKind(wxArrayString& words, int& kindFilter, bool& allowVariables, kindOfCCList& kindCC)
{
    kindCC = kccOther;
    allowVariables = false;
    wxString wordLw;
    wxString wordLastLw;
    int woCount = words.GetCount();
    if (woCount > 0)
    {
        wordLw = words.Item(0);
        wordLastLw = words.Item(woCount-1);
    }
    if (woCount > 1)
    {
        if ( (wordLw.IsSameAs('(') && words.Item(1).IsSameAs(_T("type"))) ||
             (wordLw.IsSameAs('(') && words.Item(1).IsSameAs(_T("extends"))) ||
             (wordLw.IsSameAs('(') && words.Item(1).IsSameAs(_T("class"))))
            wordLw.Prepend( words.Item(1) );
    }

    if (woCount >= 2 && wordLastLw.IsSameAs(_T("!")) &&
        words.Item(woCount-2).IsSameAs(_T("$")) )
    {
        if (woCount >= 3 &&
            (words.Item(woCount-3).IsSameAs(_T("omp")) || words.Item(woCount-3).IsSameAs(_T("acc"))))
        {
            kindFilter = tkOther;
            int idxa = words.Index(_T("("));
            if (idxa != wxNOT_FOUND)
            {
                int idxb = words.Index((_T(")")));
                if (idxb == wxNOT_FOUND || idxb > idxa)
                    allowVariables = true;
            }
            return;
        }
        else
        {
            words.RemoveAt(words.GetCount()-1);
            words.RemoveAt(words.GetCount()-1);
            woCount = words.GetCount();
            if (woCount > 0)
            {
                wordLastLw = words.Item(woCount-1);
            }
            else
            {
                wordLw = wxEmptyString;
                wordLastLw = wxEmptyString;
            }
        }
    }


    if (wordLw.IsEmpty())
    {
        kindFilter = tkOther;
        allowVariables = true;
    }
    else if (wordLw.IsSameAs(_T("call")))
    {
        kindFilter = tkSubroutine | tkInterface;
    }
    else if (woCount > 1 &&
             ( wordLastLw.IsSameAs(_T("generic")) || wordLastLw.IsSameAs(_T("procedure")) ||
              ( wordLastLw.IsSameAs(_T("module")) && words.Item(woCount-2).IsSameAs(_T("procedure")) ) ))
    {
        kindFilter = tkSubroutine | tkFunction | tkInterface;
        if (words.Index(_T(":")) == wxNOT_FOUND)
            kindFilter = kindFilter | tkOther;
    }
    else if (wordLw.IsSameAs(_T("use")) || wordLw.IsSameAs(_T("module")))
    {
        kindFilter = tkModule;
    }
    else if (wordLw.IsSameAs(_T("module")))
    {
        kindFilter = tkModule | tkSubmodule;
    }
    else if (woCount == 1 && wordLw.IsSameAs(_T("endmodule")))
    {
        kindFilter = tkModule;
    }
    else if (wordLastLw.IsSameAs(_T("submodule")))
    {
        kindFilter = tkModule | tkSubmodule;
    }
    else if ((woCount == 2 && wordLastLw.IsSameAs(_T("end")) && wordLw.IsSameAs(_T("submodule"))) ||
             (woCount == 1 && wordLw.IsSameAs(_T("endsubmodule"))))
    {
        kindFilter = tkSubmodule;
    }
    else if (woCount > 1 && wordLastLw.IsSameAs(_T("use")))
    {
        if (woCount > 2 && wordLw.IsSameAs(_T(":")) && words.Item(1).IsSameAs(_T(":")))
        {
            kindFilter = tkModule;
        }
        else
        {
            kindFilter = tkVariable | tkSubroutine | tkFunction | tkInterface | tkOther;
            allowVariables = true;
            kindCC = kccUseAssocTokens;
        }
    }
    else if (wordLastLw.IsSameAs(_T("private")) ||
             wordLastLw.IsSameAs(_T("public")) ||
             wordLastLw.IsSameAs(_T("protected")) )
    {
        kindFilter = tkVariable | tkSubroutine | tkFunction | tkInterface | tkType | tkOther;
        allowVariables = true;
        kindCC = kccAccessList;
    }
    else if (wordLw.IsSameAs('=') || wordLw.IsSameAs('+') || wordLw.IsSameAs('-') || wordLw.IsSameAs('*') ||
             wordLw.IsSameAs('/') || wordLw.IsSameAs('>') || wordLw.IsSameAs('<') || wordLw.IsSameAs('.'))
    {
        kindFilter = tkFunction | tkInterface;
        allowVariables = true;
    }
    else if (woCount == 3 && wordLw.IsSameAs('(') && words.Item(1).IsSameAs(_T("is"))
             && (words.Item(2).IsSameAs(_T("type")) || words.Item(2).IsSameAs(_T("class"))) )
    {
        kindFilter = tkType;
    }
    else if ((woCount == 2 && wordLastLw.IsSameAs(_T("allocate")) && wordLw.IsSameAs('('))
             || (woCount == 5 && wordLastLw.IsSameAs(_T("allocate")) && wordLw.IsSameAs(':')))
    {
        if (woCount == 2 && wordLw.IsSameAs('('))
        {
            kindFilter = tkType;
            allowVariables = true;
        }
        else // (woCount == 5 && wordLw.IsSameAs(':'))
        {
            kindFilter = tkVariable;
            allowVariables = true;
        }
    }
    else if (woCount >= 2 && words.Item(1).IsSameAs(_T("c_funloc")) && wordLw.IsSameAs('('))
    {
        kindFilter = tkSubroutine | tkFunction | tkInterface;
    }
    else if (woCount > 2 && words.Item(1).IsSameAs(_T("intent")) && wordLw.IsSameAs('('))
    {
        kindFilter = tkOther;
        allowVariables = false;
    }
    else if (wordLw.IsSameAs('(') || wordLw.IsSameAs(','))
    {
        kindFilter = tkFunction | tkInterface | tkOther;
        allowVariables = true;
    }
    else if ( wordLw.IsSameAs(_T("subroutine")) || wordLw.IsSameAs(_T("function")) || wordLw.IsSameAs(_T("interface"))
             || wordLw.IsSameAs(_T("procedure")) )
    {
        kindFilter = tkSubroutine | tkFunction | tkInterface;
    }
    else if (woCount == 1 && wordLw.IsSameAs(_T("endsubroutine")))
    {
        kindFilter = tkSubroutine | tkInterface;
    }
    else if (woCount == 1 && wordLw.IsSameAs(_T("endfunction")))
    {
        kindFilter = tkFunction | tkInterface;
    }
    else if (wordLw.IsSameAs(_T("type")) || wordLw.IsSameAs(_T("type(")) || wordLw.IsSameAs(_T("extends("))
             || wordLw.IsSameAs(_T("class(")))
    {
        kindFilter = tkType;
    }
    else if (wordLw.IsSameAs(';'))
    {
        kindFilter = tkOther;
        allowVariables = true;
    }
    else if (woCount >= 3 && wordLw.IsSameAs(':') && words.Item(1).IsSameAs(_T(":")) &&
              ((words.Item(woCount-1).IsSameAs(_T("type")) && words.Item(woCount-2).IsSameAs('('))  ||
               (words.Item(woCount-1).IsSameAs(_T("class")) && words.Item(woCount-2).IsSameAs('(')) ||
                words.Item(woCount-1).IsSameAs(_T("integer")) ||
                words.Item(woCount-1).IsSameAs(_T("real"))    ||
                words.Item(woCount-1).IsSameAs(_T("logical")) ||
                words.Item(woCount-1).IsSameAs(_T("complex")) ||
                words.Item(woCount-1).IsSameAs(_T("character")) ))
    {
        kindFilter = tkVariable;
        allowVariables = true;
    }
    else if (wordLw.IsSameAs(':'))
    {
        kindFilter = tkSubroutine | tkFunction | tkInterface;
        allowVariables = true;
    }
    else if (wordLw.IsSameAs(_T("do")))
    {
        kindFilter = tkOther;
        allowVariables = true;
    }
    else if (woCount > 1 && wordLw.IsSameAs(')') && (
                (wordLastLw.IsSameAs(_T("if")))
             || (wordLastLw.IsSameAs(_T("read")))
             || (wordLastLw.IsSameAs(_T("write"))) ))
    {
        kindFilter = tkOther | tkFunction | tkInterface;
        allowVariables = true;
    }
    else if (wordLw.IsSameAs('['))
    {
        kindFilter = tkVariable;
        allowVariables = true;
    }
    else
    {
        kindFilter = tkOther;
    }
}

bool CCSmartFilter::FitsToContext(const wxString& kw, const wxArrayString& firstWords)
{
    static std::set<wxString> onlyFirstSet;
    if (onlyFirstSet.size() == 0)
    {
        onlyFirstSet.insert(_T("contains"));
        onlyFirstSet.insert(_T("double"));
        onlyFirstSet.insert(_T("doubleprecision"));
        onlyFirstSet.insert(_T("else"));
        onlyFirstSet.insert(_T("elseif"));
        onlyFirstSet.insert(_T("elsewhere"));
        onlyFirstSet.insert(_T("enum"));
        onlyFirstSet.insert(_T("end"));
        onlyFirstSet.insert(_T("endassociate"));
        onlyFirstSet.insert(_T("endblock"));
        onlyFirstSet.insert(_T("endblockdata"));
        onlyFirstSet.insert(_T("endcritical"));
        onlyFirstSet.insert(_T("endenum"));
        onlyFirstSet.insert(_T("endfile"));
        onlyFirstSet.insert(_T("endforall"));
        onlyFirstSet.insert(_T("endfunction"));
        onlyFirstSet.insert(_T("endif"));
        onlyFirstSet.insert(_T("endinterface"));
        onlyFirstSet.insert(_T("endprocedure"));
        onlyFirstSet.insert(_T("enddo"));
        onlyFirstSet.insert(_T("endmodule"));
        onlyFirstSet.insert(_T("endprogram"));
        onlyFirstSet.insert(_T("endselect"));
        onlyFirstSet.insert(_T("endsubmodule"));
        onlyFirstSet.insert(_T("endsubroutine"));
        onlyFirstSet.insert(_T("endteam"));
        onlyFirstSet.insert(_T("endtype"));
        onlyFirstSet.insert(_T("endwhere"));
        onlyFirstSet.insert(_T("entry"));
        onlyFirstSet.insert(_T("error"));
        onlyFirstSet.insert(_T("equivalence"));
        onlyFirstSet.insert(_T("final"));
        onlyFirstSet.insert(_T("flush"));
        onlyFirstSet.insert(_T("forall"));
        onlyFirstSet.insert(_T("format"));
        onlyFirstSet.insert(_T("if"));
        onlyFirstSet.insert(_T("implicit"));
        onlyFirstSet.insert(_T("include"));
        onlyFirstSet.insert(_T("inquire"));
        onlyFirstSet.insert(_T("module"));
        onlyFirstSet.insert(_T("namelist"));
        onlyFirstSet.insert(_T("nullify"));
        onlyFirstSet.insert(_T("open"));
        onlyFirstSet.insert(_T("print"));
        onlyFirstSet.insert(_T("program"));
    }

    bool kwFits = true;
    wxString kwLw = kw.Lower();
    int lenFW = firstWords.size();
    wxArrayString fWL;
    for (int i=0; i<lenFW; ++i) {
        if (!firstWords[i].IsEmpty())
            fWL.Add(firstWords[i]);
    }
    lenFW = fWL.size();

    if (lenFW >= 1 && fWL[lenFW-1] == _T("end"))
    {
        if (kwLw == _T("associate") || kwLw == _T("block") || kwLw == _T("data") || kwLw == _T("critical") ||
            kwLw == _T("do") || kwLw == _T("enum") || kwLw == _T("forall") ||
            kwLw == _T("function") || kwLw == _T("if") || kwLw == _T("interface") || kwLw == _T("module") ||
            kwLw == _T("procedure") || kwLw == _T("program") || kwLw == _T("select") || kwLw == _T("submodule") ||
            kwLw == _T("subroutine") || kwLw == _T("team") || kwLw == _T("type") || kwLw == _T("where"))
        {
            kwFits = true;
        }
        else
            kwFits = false;
    }
    else if (lenFW > 0 && onlyFirstSet.count(kwLw) != 0)
    {
        kwFits = false;
    }
    else if (kwLw == _T("associate") || kwLw == _T("do") || kwLw == _T("change"))
    {
        if (lenFW == 0 || (lenFW > 0 && fWL[0] == _T(":")))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW == 1 && fWL[0] == _T("implicit"))
    {
        if (kwLw == _T("none") || kwLw == _T("real") || kwLw == _T("integer") ||
            kwLw == _T("logical") || kwLw == _T("character") ||kwLw == _T("type"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW >= 1 && fWL[0] == _T("do"))
    {
        if (kwLw == _T("concurrent") || kwLw == _T("while"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == _T("do"))
    {
        long label;
        if ((lenFW == 1 && fWL[0].ToLong(&label)) ||
            (lenFW == 2 && (fWL[0] == _T(":"))) ||
            lenFW == 0)
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == _T("concurrent"))
    {
        if (lenFW >= 1 && fWL[0] == _T("do"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == _T("if"))
    {
        long label;
        if ((lenFW == 1 && (fWL[0].ToLong(&label) || fWL[0] == _T("else"))) ||
            (lenFW == 2 && (fWL[0] == _T(":"))) ||
            lenFW == 0)
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == _T("allocate") || kwLw == _T("deallocate"))
    {
        if (lenFW == 0 || (lenFW > 0 && fWL[lenFW-1] == _T("if") && fWL[0] == _T(")")))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == _T("apostrophe"))
    {
        if (lenFW > 0 && fWL[lenFW-1] == _T("open"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW > 2 && fWL[0] == _T("(") && fWL[1] == _T("intent"))
    {
        if (kwLw == _T("in") || kwLw == _T("inout") || kwLw == _T("out"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW >= 2 && (fWL[lenFW-1] == _T("real") || fWL[lenFW-1] == _T("integer") || fWL[lenFW-1] == _T("logical") ||
                            fWL[lenFW-1] == _T("complex") || fWL[lenFW-1] == _T("character") ||
                            (fWL[lenFW-1] == _T("double") && fWL[lenFW-2] == _T("precision")) || fWL[lenFW-1] == _T("doubleprecision") ||
                            ((fWL[lenFW-1] == _T("type") || fWL[lenFW-1] == _T("class")) && fWL[lenFW-2] == _T("(")))  &&
             !CCSmartFilter::hasWord(_T("::"), fWL))
    {
        if (kwLw == _T("allocatable") || kwLw == _T("dimension") || kwLw == _T("pointer") || kwLw == _T("target") ||
            kwLw == _T("contiguous") || kwLw == _T("selected_char_kind") || kwLw == _T("selected_int_kind") ||
            kwLw == _T("selected_real_kind") || kwLw == _T("codimension") || kwLw == _T("size") || kwLw == _T("shape") ||
            kwLw == _T("intent") || kwLw == _T("optional") || kwLw == _T("save") || kwLw == _T("parameter") ||
            kwLw == _T("private") || kwLw == _T("public"))
        {
            kwFits = true;
        }
        else if (lenFW > 2 && fWL[0] == _T("(") && fWL[1] == _T("intent"))
        {
            if (kwLw == _T("in") || kwLw == _T("inout") || kwLw == _T("out"))
                kwFits = true;
            else
                kwFits = false;
        }
        else
            kwFits = false;
    }
    else if (kwLw == _T("allocatable") || kwLw == _T("dimension") || kwLw == _T("pointer") || kwLw == _T("target") ||
            kwLw == _T("contiguous") || kwLw == _T("codimension") ||
            kwLw == _T("intent") || kwLw == _T("contiguous") || kwLw == _T("optional"))
    {
        // the above keywords can be only as variable declaration attribute
        kwFits = false;
    }
    else if (kwLw == _T("stop"))
    {
        if (lenFW == 0 || (lenFW > 0 && (fWL[0] == _T(")") || fWL[0] == _T("error"))))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == _T("then"))
    {
        if (lenFW >= 3 && fWL[0] == _T(")"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == _T("bind"))
    {
        if (lenFW >= 2 && (fWL[0] == _T(",") || fWL[0] == _T(")")))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == _T("only"))
    {
        if (lenFW >= 2 && fWL[lenFW-1] == _T("use"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW >= 4 && (fWL[lenFW-1] == _T("open") || fWL[lenFW-1] == _T("read") || fWL[lenFW-1] == _T("write")) &&
             fWL[lenFW-2] == _T("(") && fWL[0] == _T("=") && fWL[1] == _T("delim"))
    {
        if (kwLw == _T("quote") || kwLw == _T("apostrophe") || kwLw == _T("none"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == _T("quote") || kwLw == _T("apostrophe"))
    {
        kwFits = false;
    }
    else if (lenFW >= 2 && fWL[lenFW-1] == _T("open") && fWL[lenFW-2] == _T("("))
    {
        if (kwLw == _T("unit") || kwLw == _T("access") || kwLw == _T("action") || kwLw == _T("asynchronous") ||
            kwLw == _T("blank") || kwLw == _T("decimal") || kwLw == _T("delim") || kwLw == _T("encoding") ||
            kwLw == _T("err") || kwLw == _T("file") || kwLw == _T("form") || kwLw == _T("iomsg") || kwLw == _T("iostat") ||
            kwLw == _T("newunit") || kwLw == _T("pad") || kwLw == _T("position") || kwLw == _T("recl") ||
            kwLw == _T("round") || kwLw == _T("sign") || kwLw == _T("status"))
        {
            kwFits = true;
        }
        else
            kwFits = false;
    }
    else if (lenFW >= 2 && fWL[lenFW-1] == _T("inquiry") && fWL[lenFW-2] == _T("("))
    {
        if (kwLw == _T("unit") || kwLw == _T("file") || kwLw == _T("access") || kwLw == _T("action") || kwLw == _T("asynchronous") ||
            kwLw == _T("blank") || kwLw == _T("decimal") || kwLw == _T("delim") || kwLw == _T("direct") || kwLw == _T("encoding") ||
            kwLw == _T("err") || kwLw == _T("exist") || kwLw == _T("form") || kwLw == _T("formated") || kwLw == _T("id") ||
            kwLw == _T("iomsg") || kwLw == _T("iostat") || kwLw == _T("name") || kwLw == _T("named") || kwLw == _T("nextrec") ||
            kwLw == _T("number") || kwLw == _T("opened") || kwLw == _T("pad") || kwLw == _T("pending") || kwLw == _T("pos") ||
            kwLw == _T("position") || kwLw == _T("read") || kwLw == _T("readwrite") || kwLw == _T("recl") ||
            kwLw == _T("round") || kwLw == _T("sequential") || kwLw == _T("sign") || kwLw == _T("size") || kwLw == _T("stream") ||
            kwLw == _T("unformated") || kwLw == _T("write"))
        {
            kwFits = true;
        }
        else
            kwFits = false;
    }
    else if (lenFW >= 2 && fWL[lenFW-1] == _T("close") && fWL[lenFW-2] == _T("("))
    {
        if (kwLw == _T("unit") || kwLw == _T("iomsg") || kwLw == _T("iostat") ||
            kwLw == _T("err") || kwLw == _T("status"))
        {
            kwFits = true;
        }
        else
            kwFits = false;
    }
    else if (lenFW >= 2 && (fWL[lenFW-1] == _T("read") || fWL[lenFW-1] == _T("write")) && fWL[lenFW-2] == _T("("))
    {
        if (kwLw == _T("unit") || kwLw == _T("fmt") || kwLw == _T("nml") || kwLw == _T("advance") || kwLw == _T("asynchronous") ||
            kwLw == _T("blank") || kwLw == _T("decimal") || kwLw == _T("delim") || kwLw == _T("end") || kwLw == _T("eor") ||
            kwLw == _T("err") || kwLw == _T("id") || kwLw == _T("iomsg") || kwLw == _T("iostat") || kwLw == _T("pad") ||
            kwLw == _T("pos") || kwLw == _T("rec") || kwLw == _T("round") || kwLw == _T("sign") || kwLw == _T("size"))
        {
            kwFits = true;
        }
        else
            kwFits = false;
    }
    else if (kwLw == _T("access") || kwLw == _T("action") || kwLw == _T("asynchronous") ||
            kwLw == _T("blank") || kwLw == _T("decimal") || kwLw == _T("delim") || kwLw == _T("encoding") ||
            kwLw == _T("err") || kwLw == _T("file") || kwLw == _T("form") || kwLw == _T("iomsg") || kwLw == _T("iostat") ||
            kwLw == _T("newunit") || kwLw == _T("pad") || kwLw == _T("position") || kwLw == _T("recl") ||
            kwLw == _T("round") || kwLw == _T("status ") || kwLw == _T("unit") || kwLw == _T("file") ||
            kwLw == _T("direct") || kwLw == _T("exist") || kwLw == _T("formated") || kwLw == _T("id") || kwLw == _T("name") ||
            kwLw == _T("named") || kwLw == _T("nextrec") ||
            kwLw == _T("number") || kwLw == _T("opened") || kwLw == _T("pending") || kwLw == _T("pos") || kwLw == _T("readwrite") ||
            kwLw == _T("sequential") || kwLw == _T("stream") || kwLw == _T("unformated"))
    {
        // the above keywords can be only in open, close, inquire, write and read statements.
        kwFits = false;
    }
    else if (kwLw == _T("sequence"))
    {
        if (lenFW == 0)
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW >= 1 && fWL[lenFW-1] == _T("go"))
    {
        if (kwLw == _T("to"))
            kwFits = true;
        else
            kwFits = false;
    }
    return kwFits;
}

bool CCSmartFilter::hasWord(const wxString& word, const wxArrayString& wordArr)
{
    wxString str;
    int wCount = wordArr.size();
    for (int i=0; i<wCount; ++i)
    {
        str.Append(wordArr[i]);
    }

    return (str.Find(word) != wxNOT_FOUND);
}
