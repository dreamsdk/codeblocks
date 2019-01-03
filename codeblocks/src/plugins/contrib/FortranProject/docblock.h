#ifndef DOCBLOCK_H
#define DOCBLOCK_H

#include "tokenf.h"
#include <map>
#include <wx/string.h>
#include <wx/html/htmlwin.h>


class DocBlock
{
    public:
        DocBlock();
        virtual ~DocBlock();
        wxString GetDescription();
        bool HasDescription();
        void AddDescription(const wxString &descr);
        wxString GetBrief();
        wxString GetValue(wxString& key);
        bool HasBrief();
        int GetParamCount();
        void AddBrief(const wxString &bline);
        void AddParam(const wxString &name, const wxString &descr);
        void Clear();

    protected:
    private:
        std::map<wxString,wxString> m_DocMap;
        wxString m_Description;
        wxString m_Brief;
};

class HtmlDoc
{
    public:
        static wxString GenerateHtmlDoc(TokenFlat* token, int token_idx, bool& hasDoc);
        static wxString OnDocumentationLink(wxHtmlLinkEvent &event, bool &dismissPopup, bool &isGoto, long int &tokenIdx);
        static wxString GetDocForTooltip(TokenFlat* token);
        static wxString GetDocShort(const wxString& tokDoc);
};

#endif // DOCBLOCK_H
