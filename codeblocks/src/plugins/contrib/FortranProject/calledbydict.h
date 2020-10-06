#ifndef CALLEDBYDICT_H
#define CALLEDBYDICT_H

#include <map>
#include <set>
#include <list>

#include "tokenf.h"

class CalledByDict
{
    public:
        CalledByDict();
        virtual ~CalledByDict();

        void Build(TokensArrayF* allTokens);
        std::list<TokenF*>* GetCallingTokens(const wxString& name);

    protected:

    private:
        void FindChildrenNames(TokensArrayF* tokens, int tokenMask, std::set<wxString> &definedNames);
        void FillCalledByDict(TokensArrayF* tokens, std::set<wxString> &definedNames);

        std::map<wxString,std::list<TokenF*>*> m_NamesDict;
};

#endif // CALLEDBYDICT_H

