/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */
#include "ccsmartfilter.h"

#include <sdk.h>
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
        if ( (wordLw.IsSameAs('(') && words.Item(1).IsSameAs("type")) ||
             (wordLw.IsSameAs('(') && words.Item(1).IsSameAs("extends")) ||
             (wordLw.IsSameAs('(') && words.Item(1).IsSameAs("class")))
            wordLw.Prepend( words.Item(1) );
    }

    if (woCount >= 2 && wordLastLw.IsSameAs("!") &&
        words.Item(woCount-2).IsSameAs("$") )
    {
        if (woCount >= 3 &&
            (words.Item(woCount-3).IsSameAs("omp") || words.Item(woCount-3).IsSameAs("acc")))
        {
            kindFilter = tkOther;
            int idxa = words.Index("(");
            if (idxa != wxNOT_FOUND)
            {
                int idxb = words.Index((")"));
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
    else if (wordLw.IsSameAs("call"))
    {
        kindFilter = tkSubroutine | tkInterface;
    }
    else if (woCount > 1 &&
             ( wordLastLw.IsSameAs("generic") || wordLastLw.IsSameAs("procedure") ||
              ( wordLastLw.IsSameAs("module") && words.Item(woCount-2).IsSameAs("procedure") ) ))
    {
        kindFilter = tkSubroutine | tkFunction | tkInterface;
        if (words.Index(":") == wxNOT_FOUND)
            kindFilter = kindFilter | tkOther;
    }
    else if (wordLw.IsSameAs("use") || wordLw.IsSameAs("module"))
    {
        kindFilter = tkModule;
    }
    else if (wordLw.IsSameAs("module"))
    {
        kindFilter = tkModule | tkSubmodule;
    }
    else if (woCount == 1 && wordLw.IsSameAs("endmodule"))
    {
        kindFilter = tkModule;
    }
    else if (wordLastLw.IsSameAs("submodule"))
    {
        kindFilter = tkModule | tkSubmodule;
    }
    else if ((woCount == 2 && wordLastLw.IsSameAs("end") && wordLw.IsSameAs("submodule")) ||
             (woCount == 1 && wordLw.IsSameAs("endsubmodule")))
    {
        kindFilter = tkSubmodule;
    }
    else if (woCount > 1 && wordLastLw.IsSameAs("use"))
    {
        if (woCount > 2 && wordLw.IsSameAs(":") && words.Item(1).IsSameAs(":"))
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
    else if (wordLastLw.IsSameAs("private") ||
             wordLastLw.IsSameAs("public") ||
             wordLastLw.IsSameAs("protected") )
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
    else if (woCount == 3 && wordLw.IsSameAs('(') && words.Item(1).IsSameAs("is")
             && (words.Item(2).IsSameAs("type") || words.Item(2).IsSameAs("class")) )
    {
        kindFilter = tkType;
    }
    else if ((woCount == 2 && wordLastLw.IsSameAs("allocate") && wordLw.IsSameAs('('))
             || (woCount == 5 && wordLastLw.IsSameAs("allocate") && wordLw.IsSameAs(':')))
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
    else if (woCount >= 2 && words.Item(1).IsSameAs("c_funloc") && wordLw.IsSameAs('('))
    {
        kindFilter = tkSubroutine | tkFunction | tkInterface;
    }
    else if (woCount > 2 && words.Item(1).IsSameAs("intent") && wordLw.IsSameAs('('))
    {
        kindFilter = tkOther;
        allowVariables = false;
    }
    else if (wordLw.IsSameAs('(') || wordLw.IsSameAs(','))
    {
        kindFilter = tkFunction | tkInterface | tkOther;
        allowVariables = true;
    }
    else if ( wordLw.IsSameAs("subroutine") || wordLw.IsSameAs("function") || wordLw.IsSameAs("interface")
             || wordLw.IsSameAs("procedure") )
    {
        kindFilter = tkSubroutine | tkFunction | tkInterface;
    }
    else if (woCount == 1 && wordLw.IsSameAs("endsubroutine"))
    {
        kindFilter = tkSubroutine | tkInterface;
    }
    else if (woCount == 1 && wordLw.IsSameAs("endfunction"))
    {
        kindFilter = tkFunction | tkInterface;
    }
    else if (wordLw.IsSameAs("type") || wordLw.IsSameAs("type(") || wordLw.IsSameAs("extends(")
             || wordLw.IsSameAs("class("))
    {
        kindFilter = tkType;
    }
    else if (wordLw.IsSameAs(';'))
    {
        kindFilter = tkOther;
        allowVariables = true;
    }
    else if (woCount >= 3 && wordLw.IsSameAs(':') && words.Item(1).IsSameAs(":") &&
              ((words.Item(woCount-1).IsSameAs("type") && words.Item(woCount-2).IsSameAs('('))  ||
               (words.Item(woCount-1).IsSameAs("class") && words.Item(woCount-2).IsSameAs('(')) ||
                words.Item(woCount-1).IsSameAs("integer") ||
                words.Item(woCount-1).IsSameAs("real")    ||
                words.Item(woCount-1).IsSameAs("logical") ||
                words.Item(woCount-1).IsSameAs("complex") ||
                words.Item(woCount-1).IsSameAs("character") ))
    {
        kindFilter = tkVariable;
        allowVariables = true;
    }
    else if (wordLw.IsSameAs(':'))
    {
        kindFilter = tkSubroutine | tkFunction | tkInterface;
        allowVariables = true;
    }
    else if (wordLw.IsSameAs("do"))
    {
        kindFilter = tkOther;
        allowVariables = true;
    }
    else if (woCount > 1 && wordLw.IsSameAs(')') && (
                (wordLastLw.IsSameAs("if"))
             || (wordLastLw.IsSameAs("read"))
             || (wordLastLw.IsSameAs("write")) ))
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
        onlyFirstSet.insert("contains");
        onlyFirstSet.insert("double");
        onlyFirstSet.insert("doubleprecision");
        onlyFirstSet.insert("else");
        onlyFirstSet.insert("elseif");
        onlyFirstSet.insert("elsewhere");
        onlyFirstSet.insert("enum");
        onlyFirstSet.insert("end");
        onlyFirstSet.insert("endassociate");
        onlyFirstSet.insert("endblock");
        onlyFirstSet.insert("endblockdata");
        onlyFirstSet.insert("endcritical");
        onlyFirstSet.insert("endenum");
        onlyFirstSet.insert("endfile");
        onlyFirstSet.insert("endforall");
        onlyFirstSet.insert("endfunction");
        onlyFirstSet.insert("endif");
        onlyFirstSet.insert("endinterface");
        onlyFirstSet.insert("endprocedure");
        onlyFirstSet.insert("enddo");
        onlyFirstSet.insert("endmodule");
        onlyFirstSet.insert("endprogram");
        onlyFirstSet.insert("endselect");
        onlyFirstSet.insert("endsubmodule");
        onlyFirstSet.insert("endsubroutine");
        onlyFirstSet.insert("endteam");
        onlyFirstSet.insert("endtype");
        onlyFirstSet.insert("endwhere");
        onlyFirstSet.insert("entry");
        onlyFirstSet.insert("error");
        onlyFirstSet.insert("equivalence");
        onlyFirstSet.insert("final");
        onlyFirstSet.insert("flush");
        onlyFirstSet.insert("forall");
        onlyFirstSet.insert("format");
        onlyFirstSet.insert("if");
        onlyFirstSet.insert("implicit");
        onlyFirstSet.insert("include");
        onlyFirstSet.insert("inquire");
        onlyFirstSet.insert("import");
        onlyFirstSet.insert("module");
        onlyFirstSet.insert("namelist");
        onlyFirstSet.insert("nullify");
        onlyFirstSet.insert("open");
        onlyFirstSet.insert("print");
        onlyFirstSet.insert("program");
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

    if (lenFW >= 1 && fWL[lenFW-1] == "end")
    {
        if (kwLw == "associate" || kwLw == "block" || kwLw == "data" || kwLw == "critical" ||
            kwLw == "do" || kwLw == "enum" || kwLw == "forall" ||
            kwLw == "function" || kwLw == "if" || kwLw == "interface" || kwLw == "module" ||
            kwLw == "procedure" || kwLw == "program" || kwLw == "select" || kwLw == "submodule" ||
            kwLw == "subroutine" || kwLw == "team" || kwLw == "type" || kwLw == "where")
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
    else if (kwLw == "associate" || kwLw == "do" || kwLw == "change")
    {
        if (lenFW == 0 || (lenFW > 0 && fWL[0] == ":"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW == 1 && fWL[0] == "implicit")
    {
        if (kwLw == "none" || kwLw == "real" || kwLw == "integer" ||
            kwLw == "logical" || kwLw == "character" ||kwLw == "type")
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW >= 1 && fWL[0] == "do")
    {
        if (kwLw == "concurrent" || kwLw == "while")
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == "do")
    {
        long label;
        if ((lenFW == 1 && fWL[0].ToLong(&label)) ||
            (lenFW == 2 && (fWL[0] == ":")) ||
            lenFW == 0)
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == "concurrent")
    {
        if (lenFW >= 1 && fWL[0] == "do")
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == "if")
    {
        long label;
        if ((lenFW == 1 && (fWL[0].ToLong(&label) || fWL[0] == "else")) ||
            (lenFW == 2 && (fWL[0] == ":")) ||
            lenFW == 0)
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == "allocate" || kwLw == "deallocate")
    {
        if (lenFW == 0 || (lenFW > 0 && fWL[lenFW-1] == "if" && fWL[0] == ")"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == "apostrophe")
    {
        if (lenFW > 0 && fWL[lenFW-1] == "open")
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW > 2 && fWL[0] == "(" && fWL[1] == "intent")
    {
        if (kwLw == "in" || kwLw == "inout" || kwLw == "out")
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW >= 2 && (fWL[lenFW-1] == "real" || fWL[lenFW-1] == "integer" || fWL[lenFW-1] == "logical" ||
                            fWL[lenFW-1] == "complex" || fWL[lenFW-1] == "character" ||
                            (fWL[lenFW-1] == "double" && fWL[lenFW-2] == "precision") || fWL[lenFW-1] == "doubleprecision" ||
                            ((fWL[lenFW-1] == "type" || fWL[lenFW-1] == "class") && fWL[lenFW-2] == "("))  &&
             !CCSmartFilter::hasWord("::", fWL))
    {
        if (kwLw == "allocatable" || kwLw == "dimension" || kwLw == "pointer" || kwLw == "target" ||
            kwLw == "contiguous" || kwLw == "selected_char_kind" || kwLw == "selected_int_kind" ||
            kwLw == "selected_real_kind" || kwLw == "codimension" || kwLw == "size" || kwLw == "shape" ||
            kwLw == "intent" || kwLw == "optional" || kwLw == "save" || kwLw == "parameter" ||
            kwLw == "private" || kwLw == "public" || kwLw == "asynchronous")
        {
            kwFits = true;
        }
        else if (lenFW > 2 && fWL[0] == "(" && fWL[1] == "intent")
        {
            if (kwLw == "in" || kwLw == "inout" || kwLw == "out")
                kwFits = true;
            else
                kwFits = false;
        }
        else
            kwFits = false;
    }
    else if (kwLw == "allocatable" || kwLw == "dimension" || kwLw == "pointer" || kwLw == "target" ||
            kwLw == "contiguous" || kwLw == "codimension" ||
            kwLw == "intent" || kwLw == "contiguous" || kwLw == "optional")
    {
        // the above keywords can be only as variable declaration attribute
        kwFits = false;
    }
    else if (kwLw == "stop")
    {
        if (lenFW == 0 || (lenFW > 0 && (fWL[0] == ")" || fWL[0] == "error")))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == "then")
    {
        if (lenFW >= 3 && fWL[0] == ")")
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == "bind")
    {
        if (lenFW >= 2 && (fWL[0] == "," || fWL[0] == ")"))
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == "only")
    {
        if (lenFW >= 2 && fWL[lenFW-1] == "use")
            kwFits = true;
        else if (lenFW == 1 && fWL[0] == "import")
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW >= 4 && (fWL[lenFW-1] == "open" || fWL[lenFW-1] == "read" || fWL[lenFW-1] == "write") &&
             fWL[lenFW-2] == "(" && fWL[0] == "=" && fWL[1] == "delim")
    {
        if (kwLw == "quote" || kwLw == "apostrophe" || kwLw == "none")
            kwFits = true;
        else
            kwFits = false;
    }
    else if (kwLw == "quote" || kwLw == "apostrophe")
    {
        kwFits = false;
    }
    else if (lenFW >= 2 && fWL[lenFW-1] == "open" && fWL[lenFW-2] == "(")
    {
        if (kwLw == "unit" || kwLw == "access" || kwLw == "action" || kwLw == "asynchronous" ||
            kwLw == "blank" || kwLw == "decimal" || kwLw == "delim" || kwLw == "encoding" ||
            kwLw == "err" || kwLw == "file" || kwLw == "form" || kwLw == "iomsg" || kwLw == "iostat" ||
            kwLw == "newunit" || kwLw == "pad" || kwLw == "position" || kwLw == "recl" ||
            kwLw == "round" || kwLw == "sign" || kwLw == "status")
        {
            kwFits = true;
        }
        else
            kwFits = false;
    }
    else if (lenFW >= 2 && fWL[lenFW-1] == "inquiry" && fWL[lenFW-2] == "(")
    {
        if (kwLw == "unit" || kwLw == "file" || kwLw == "access" || kwLw == "action" || kwLw == "asynchronous" ||
            kwLw == "blank" || kwLw == "decimal" || kwLw == "delim" || kwLw == "direct" || kwLw == "encoding" ||
            kwLw == "err" || kwLw == "exist" || kwLw == "form" || kwLw == "formated" || kwLw == "id" ||
            kwLw == "iomsg" || kwLw == "iostat" || kwLw == "name" || kwLw == "named" || kwLw == "nextrec" ||
            kwLw == "number" || kwLw == "opened" || kwLw == "pad" || kwLw == "pending" || kwLw == "pos" ||
            kwLw == "position" || kwLw == "read" || kwLw == "readwrite" || kwLw == "recl" ||
            kwLw == "round" || kwLw == "sequential" || kwLw == "sign" || kwLw == "size" || kwLw == "stream" ||
            kwLw == "unformated" || kwLw == "write")
        {
            kwFits = true;
        }
        else
            kwFits = false;
    }
    else if (lenFW >= 2 && fWL[lenFW-1] == "close" && fWL[lenFW-2] == "(")
    {
        if (kwLw == "unit" || kwLw == "iomsg" || kwLw == "iostat" ||
            kwLw == "err" || kwLw == "status")
        {
            kwFits = true;
        }
        else
            kwFits = false;
    }
    else if (lenFW >= 2 && (fWL[lenFW-1] == "read" || fWL[lenFW-1] == "write") && fWL[lenFW-2] == "(")
    {
        if (kwLw == "unit" || kwLw == "fmt" || kwLw == "nml" || kwLw == "advance" || kwLw == "asynchronous" ||
            kwLw == "blank" || kwLw == "decimal" || kwLw == "delim" || kwLw == "end" || kwLw == "eor" ||
            kwLw == "err" || kwLw == "id" || kwLw == "iomsg" || kwLw == "iostat" || kwLw == "pad" ||
            kwLw == "pos" || kwLw == "rec" || kwLw == "round" || kwLw == "sign" || kwLw == "size")
        {
            kwFits = true;
        }
        else
            kwFits = false;
    }
    else if (kwLw == "access" || kwLw == "action" || kwLw == "asynchronous" ||
            kwLw == "blank" || kwLw == "decimal" || kwLw == "delim" || kwLw == "encoding" ||
            kwLw == "err" || kwLw == "file" || kwLw == "iomsg" || kwLw == "iostat" ||
            kwLw == "newunit" || kwLw == "pad" || kwLw == "position" || kwLw == "recl" ||
            kwLw == "round" || kwLw == "status " || kwLw == "unit" || kwLw == "file" ||
            kwLw == "direct" || kwLw == "exist" || kwLw == "formated" || kwLw == "id" || kwLw == "name" ||
            kwLw == "named" || kwLw == "nextrec" ||
            kwLw == "number" || kwLw == "opened" || kwLw == "pending" || kwLw == "pos" || kwLw == "readwrite" ||
            kwLw == "sequential" || kwLw == "stream" || kwLw == "unformated")
    {
        // the above keywords can be only in open, close, inquire, write and read statements.
        kwFits = false;
    }
    else if (kwLw == "sequence")
    {
        if (lenFW == 0)
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW >= 1 && fWL[lenFW-1] == "go")
    {
        if (kwLw == "to")
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW == 1 && fWL[0] == "import")
    {
        if (kwLw == "none" || kwLw == "only" || kwLw == "all")
            kwFits = true;
        else
            kwFits = false;
    }
    else if (lenFW == 0 && (kwLw == "all" || kwLw == "in" || kwLw == "inout" || kwLw == "out" ||
                kwLw == "none" || kwLw == "nopass" || kwLw == "pass" || kwLw == "all" ||
                kwLw == "non_intrinsic" ||kwLw == "non_overridable" || kwLw == "non_recursive" ||
                kwLw == "images" || kwLw == "memory" || kwLw == "team") )
    {
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
