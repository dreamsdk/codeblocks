#ifndef SUBMODULETOKENF_H
#define SUBMODULETOKENF_H

#include "tokenf.h"
#include <wx/arrstr.h>


class SubmoduleTokenF : public TokenF
{
    public:
        SubmoduleTokenF() {};
        virtual ~SubmoduleTokenF() {};

        wxString m_AncestorModuleName;
        wxString m_ParentSubmoduleName;
};

#endif // SUBMODULETOKENF_H

