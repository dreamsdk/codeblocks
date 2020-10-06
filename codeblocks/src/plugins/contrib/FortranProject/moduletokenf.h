#ifndef MODULETOKENF_H
#define MODULETOKENF_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/arrstr.h>
#endif

#include "tokenf.h"

class ModuleTokenF : public TokenF
{
    public:
        ModuleTokenF();
        virtual ~ModuleTokenF();
        void SetDefaultPublic(bool defPublic);
        bool GetDefaultPublic() {return m_DefaultPublic;};
        void AddToPrivateList(wxString& privateTName);
        void AddToPublicList(wxString& publicTName);
        bool HasNameInPrivateList(wxString& str);
        bool HasNameInPublicList(wxString& str);

    private:
        bool m_DefaultPublic;
        wxSortedArrayString m_PrivateTList;
        wxSortedArrayString m_PublicTList;
};

#endif // MODULETOKENF_H
