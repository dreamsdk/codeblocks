/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */
#include "ccsmartfilter.h"
#include "tokenf.h"
#include <wx/string.h>

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
        if ( (wordLw.IsSameAs('(') && words.Item(1).Lower().IsSameAs(_T("type"))) ||
             (wordLw.IsSameAs('(') && words.Item(1).Lower().IsSameAs(_T("extends"))) ||
             (wordLw.IsSameAs('(') && words.Item(1).Lower().IsSameAs(_T("class"))))
            wordLw.Prepend( words.Item(1).Lower() );
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
              ( wordLastLw.IsSameAs(_T("module")) && words.Item(woCount-2).Lower().IsSameAs(_T("procedure")) ) ))
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
    else if (woCount == 3 && wordLw.IsSameAs('(') &&  words.Item(1).Lower().IsSameAs(_T("is"))
             && (words.Item(2).Lower().IsSameAs(_T("type")) || words.Item(2).Lower().IsSameAs(_T("class"))) )
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

