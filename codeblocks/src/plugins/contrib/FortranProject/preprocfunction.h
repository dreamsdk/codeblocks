
#ifndef PREPROCFUNCTION_H
#define PREPROCFUNCTION_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/string.h>
#endif
#include <vector>
#include <set>


class PreProcFunctionList;

class PreProcFunction
{
    public:
        PreProcFunction(const wxString& funName, const wxString& argStr, const wxString& funBodyIn,
                                 PreProcFunctionList *knownFunctions);
        ~PreProcFunction();
        wxString Interpret(const wxString& argStr, PreProcFunctionList *knownFunctions, std::set<wxString>* makeArgSet=NULL);

        wxString m_FunName;
        std::vector<wxString> m_Args;
        std::vector<wxString> m_Terms;
    private:
};

class PreProcFunctionList
{
    public:
        PreProcFunctionList();
        ~PreProcFunctionList();
        bool HasFunction(const wxString& funName);
        PreProcFunction* GetFunction(const wxString& funName);
        void AddFunction(PreProcFunction* pFun);
    private:
        std::vector<PreProcFunction*> m_Functions;
        std::vector<wxString> m_FunctionNames;
};

#endif // PREPROCFUNCTION_H

