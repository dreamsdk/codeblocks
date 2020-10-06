#ifndef FPIMAGELIST_H
#define FPIMAGELIST_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/window.h>
    #include <wx/imaglist.h>
#endif
#include <string>
#include <map>

#include "tokenf.h"

typedef std::map<std::string,int> StrIntMap;

class FPImageList
{
    public:
        FPImageList(int imSize);
        virtual ~FPImageList();
        wxImageList* GetImageList(){return m_pImlist;};
        int GetImageIdx(const std::string& name);
		int GetTokenKindImageIdx(TokenF* token);

    private:
        void CreateImageList(int imSize);

        wxImageList* m_pImlist;
        StrIntMap m_ImgNr;
};

#endif // FPIMAGELIST_H
