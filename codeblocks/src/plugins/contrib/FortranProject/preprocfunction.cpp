
#include "preprocfunction.h"
#include "tokenizerf.h"

#include <wx/tokenzr.h>


PreProcFunction::PreProcFunction(const wxString& funName, const wxString& argStr, const wxString& funBodyIn,
                                 PreProcFunctionList *knownFunctions)
{
    m_FunName = funName;
    // in argStr we expecting "(a, b, c)"
    if ( !argStr.StartsWith("(") || !argStr.EndsWith(")") )
    {
        // something is wrong
        m_FunName = wxEmptyString;
        return;
    }
    std::set<wxString> funArgSet;
    wxStringTokenizer tokenizer(argStr.Mid(1,argStr.length()-2), ",", wxTOKEN_STRTOK);
    while (tokenizer.HasMoreTokens())
    {
        wxString par1 = tokenizer.GetNextToken().Trim(true).Trim(false);
        m_Args.push_back("&%arg_" + par1);
        funArgSet.insert(par1);
    }

    // join function from multiple lines into one line.
    wxString funBody;
    wxStringTokenizer funTokenizer(funBodyIn, "\n", wxTOKEN_STRTOK);
    while (funTokenizer.HasMoreTokens())
    {
        wxString line1 = funTokenizer.GetNextToken().Trim(true);
        if (line1.EndsWith("\\"))
            line1 = line1.Mid(0, line1.length()-1);
        funBody << line1;
    }

    if (funBody.IsEmpty())
        return;

    Tokenizerf funToks;
    funToks.InitFromBuffer(funBody + " ", fsfFree);
    while(true)
    {
        wxString tok = funToks.GetToken();
        if (tok.IsEmpty())
            break;
        wxString nexTok = funToks.PeekToken();
        if (tok == "##")
            tok = "&%oper_join";
        else if (funArgSet.count(tok) > 0)
            tok = "&%arg_" + tok;
        else if ( knownFunctions && knownFunctions->HasFunction(tok) && (nexTok.StartsWith("(") && nexTok.EndsWith(")")) )
        {
            // it is call to the defined function.
            tok = knownFunctions->GetFunction(tok)->Interpret(nexTok, knownFunctions, &funArgSet);
            funToks.GetToken(); // consume nexTok
        }
        m_Terms.push_back(tok);
    }
}

PreProcFunction::~PreProcFunction()
{
    //dtor
}

wxString PreProcFunction::Interpret(const wxString& argStr, PreProcFunctionList *knownFunctions, std::set<wxString>* makeArgSet)
{
    if ( !argStr.StartsWith("(") || !argStr.EndsWith(")") )
        return wxEmptyString;

    std::vector<wxString> varArr;
    wxStringTokenizer tokenizer(argStr.Mid(1,argStr.length()-2), ",", wxTOKEN_RET_EMPTY_ALL);
    while (tokenizer.HasMoreTokens())
    {
        wxString var1 = tokenizer.GetNextToken().Trim(true).Trim(false);
        varArr.push_back(var1);
    }

    if (varArr.size() != m_Args.size())
        return wxEmptyString; // number of dummy arguments not equal to the call arguments.

    wxString retStr;
    for (const auto& term: m_Terms)
    {
        if (term == "&%oper_join")
            ;
        else
            retStr << term;
    }

    for (size_t i=0; i<m_Args.size(); ++i)
    {
        wxString repStr;
        if (makeArgSet && makeArgSet->count(varArr[i]) > 0)
            repStr = "&%arg_" + varArr[i]; // mark this argument
        else
            repStr = varArr[i];
        retStr.Replace(m_Args[i], repStr);
    }
    return retStr;
}

PreProcFunctionList::PreProcFunctionList()
{
    // ctor
}

PreProcFunctionList::~PreProcFunctionList()
{
    //dtor
    for (auto fun: m_Functions)
    {
        delete(fun);
    }
}


bool PreProcFunctionList::HasFunction(const wxString& funName)
{
    for (const auto& name: m_FunctionNames)
    {
        if (name == funName)
            return true;
    }
    return false;
}

PreProcFunction* PreProcFunctionList::GetFunction(const wxString& funName)
{
    for (size_t i=0; i<m_FunctionNames.size(); ++i)
    {
        if (m_FunctionNames[i] == funName)
            return m_Functions[i];
    }
    return NULL;
}

void PreProcFunctionList::AddFunction(PreProcFunction* pFun)
{
    m_Functions.push_back(pFun);
    m_FunctionNames.push_back(pFun->m_FunName);
}


//bool PreProcFunction::GetNextTokenIdx(const wxString& line, size_t idxStartRead, size_t& idxStart, size_t& idxEnd)
//{
//    // returns true when token was found.
//    // idxStart is idx where token starts.
//    // idxEnd is idx after token ends.
//
//    std::map<wxString,wxString> m_KnownFunctions;
//
//    // Find where token starts
//    idxStart = 0;
//    idxEnd   = 0;
//    size_t lineLen = line.size();
//    if (lineLen == 0)
//        return false;
//
//    bool startFound = false;
//    for (size_t i=idxStartRead; i<lineLen; ++i)
//    {
//        wxChar char1 = line.GetChar(i);
//        if (char1 != ' ' && char1 != '\t' && char1 != '\n')
//        {
//            startFound = true;
//            idxStart = i;
//            break;
//        }
//    }
//
//    if (!startFound)
//        return false;
//
//    idxEnd = lineLen;
//    for (size_t i=idxStart; i<lineLen; ++i)
//    {
//        wxChar char1 = line.GetChar(i);
//        if (isalnum(char1) || char1 == '_' || char1 == '$' || char1 == '#')
//        {}
//        else
//        {
//            if(i == idxStart)
//                idxEnd = i + 1;
//            else
//                idxEnd = i;
//            break;
//        }
//    }
//    return true;
//}
