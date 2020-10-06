#ifndef SUBMODULETOKENF_H
#define SUBMODULETOKENF_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/arrstr.h>
#endif

#include "tokenf.h"

class SubmoduleTokenF : public TokenF
{
    public:
        SubmoduleTokenF() {};
        virtual ~SubmoduleTokenF() {};

        wxString m_AncestorModuleName;
        wxString m_ParentSubmoduleName;
};

#endif // SUBMODULETOKENF_H

