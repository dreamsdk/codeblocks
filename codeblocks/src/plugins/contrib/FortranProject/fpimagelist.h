#ifndef FPIMAGELIST_H
#define FPIMAGELIST_H

#include <wx/imaglist.h>
#include <string>
#include <map>
#include "tokenf.h"

typedef std::map<std::string,int> StrIntMap;

class FPImageList
{
    public:
        FPImageList();
        virtual ~FPImageList();
        wxImageList* GetImageList(){return m_pImlist;};
        int GetImageIdx(const std::string& name);
		int GetTokenKindImageIdx(TokenF* token);

    private:
        wxImageList* m_pImlist;
        StrIntMap m_ImgNr;
};

#endif // FPIMAGELIST_H
