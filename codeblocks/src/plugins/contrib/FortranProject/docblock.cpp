
#include "docblock.h"

#ifndef CB_PRECOMP
    #include "cbcolourmanager.h"
#endif
#include <iostream>

DocBlock::DocBlock():
    m_Description(_T("**description**")),
    m_Brief(_T("**brief**"))
{
    //ctor
}

DocBlock::~DocBlock()
{
    //dtor
}

wxString DocBlock::GetDescription()
{
    if (m_DocMap.count(m_Description) == 1)
        return m_DocMap[m_Description];

    return wxEmptyString;
}

bool DocBlock::HasDescription()
{
    return (m_DocMap.count(m_Description) == 1);
}

void DocBlock::AddDescription(const wxString &descr)
{
    m_DocMap[m_Description] = descr;
}

wxString DocBlock::GetBrief()
{
    if (m_DocMap.count(m_Brief) == 1)
        return m_DocMap[m_Brief];

    return wxEmptyString;
}

bool DocBlock::HasBrief()
{
    return (m_DocMap.count(m_Brief) == 1);
}

int DocBlock::GetParamCount()
{
    int pc = 0;
    for (std::map<wxString,wxString>::iterator it = m_DocMap.begin(); it != m_DocMap.end(); ++it)
    {
        if (!it->first.IsSameAs(m_Description) && !it->first.IsSameAs(m_Brief))
            pc++;
    }
    return pc;
}

wxString DocBlock::GetValue(wxString& key)
{
    if (m_DocMap.count(key) == 1)
        return m_DocMap[key];

    return wxEmptyString;
}

void DocBlock::AddBrief(const wxString &bline)
{
    m_DocMap[_T("**brief**")] = bline;
}

void DocBlock::AddParam(const wxString &name, const wxString &descr)
{
    m_DocMap[name.Lower()] = descr;
}

void DocBlock::Clear()
{
    m_DocMap.clear();
}

//*****************************************
namespace HTMLTags
{
    static const wxString br = _T("<br>");
    static const wxString sep = _T(" ");
    static const wxString b1 = _T("<b>");
    static const wxString b0 = _T("</b>");

    static const wxString a1 = _T("<a>");
    static const wxString a0 = _T("</a>");

    static const wxString i1 = _T("<i>");
    static const wxString i0 = _T("</i>");

    static const wxString pre1 = _T("<pre>");
    static const wxString pre0 = _T("</pre>");

    static const wxString nbsp(_T("&nbsp;"));
    static const wxString tab = nbsp + nbsp + nbsp;

    static const wxString commandTag = _T("cmd=");
}

wxString HtmlDoc::GenerateHtmlDoc(TokenFlat* token, int token_idx, bool& hasDoc)
{
    //http://docs.wxwidgets.org/2.8/wx_wxhtml.html#htmltagssupported
    using namespace HTMLTags;

    ColourManager *colours = Manager::Get()->GetColourManager();

    wxString html = _T("<html><body bgcolor=\"");
    html += colours->GetColour(wxT("cc_docs_back")).GetAsString(wxC2S_HTML_SYNTAX) + _T("\" text=\"");
    html += colours->GetColour(wxT("cc_docs_fore")).GetAsString(wxC2S_HTML_SYNTAX) + _T("\" link=\"");
    html += colours->GetColour(wxT("cc_docs_link")).GetAsString(wxC2S_HTML_SYNTAX) + _T("\">");

    html += _T("<a name=\"top\"></a>");

    hasDoc = false;
    if (!token || token->m_DisplayName.IsEmpty())
        return wxEmptyString;

    // add parent:
    if (!token->m_ParentDisplayName.IsEmpty())
    {
        wxString parent;
        if (token->m_ParentTokenKind == tkModule)
            html += _T("module: ") + b1 + token->m_ParentDisplayName + b0 + br;
    }

    html += br;
    wxString moreInfo;

    //add scope and name:
    switch (token->m_TokenKind)
    {
    case tkFunction:
        html += token->m_PartFirst + _T(" function ") + b1 + token->m_DisplayName + b0;
        html += _T(" ") + token->m_Args.Trim(false);
        html += sep + token->m_PartLast;
        html += br;
        break;

    case tkSubroutine:
        html += _T("subroutine ") + b1 + token->m_DisplayName + b0;
        html += sep + token->m_Args;
        html += br;
        if (token->m_ParentTokenKind == tkFile)
            moreInfo = _T("global");
        else if (token->m_TokenAccess == taPrivate)
            moreInfo = _T("private");
        break;

    case tkVariable:
        html += token->m_TypeDefinition + _T(" :: ") + b1 + token->m_DisplayName + b0 + token->m_Args + br;
        moreInfo = token->GetTokenKindString();
        break;

    case tkInterface:
        html += token->m_TypeDefinition + nbsp + b1 + token->m_DisplayName + b0 + br;
        if (token->m_TypeDefinition.IsEmpty())
            moreInfo = _T("interface");
        else
            moreInfo = _T("generic interface");
        break;

    default:
        html += b1 + token->m_DisplayName + b0 + br;
        moreInfo = token->GetTokenKindString();
    }

    //add kind:
    if (!moreInfo.IsEmpty())
        html += i1 + _T("<font color=\"green\" size=3>") + _T("(") +
                moreInfo +_T(")") + _T("</font>") + i0 + br;

    if (!token->m_DocString.IsEmpty())
    {
        const wxString brsep = _T("@brief_end@");
        size_t brf = token->m_DocString.find(brsep);
        size_t bre_idx = 11;
        if (brf != wxString::npos)
            bre_idx += brf;
        else
            bre_idx = 0;

        if (bre_idx > 11)
        {
            html += br + i1 + b1 + _T("Brief:") + b0 + i0 + br;
            html += tab + token->m_DocString.substr(0,brf) + br;
            hasDoc = true;
        }

        if (bre_idx < token->m_DocString.size())
        {
            html += br + i1 + b1 + _T("Description:") + b0 + i0 + br;
            html += tab + token->m_DocString.substr(bre_idx) + br;
            hasDoc = true;
        }
    }

    //add go to declaration
    html += br + br + _T("<a href=\"") + commandTag + _T("goto") + wxString::Format(_T("%i"), token_idx)
               + _T("\">") +  _T("Open declaration") + _T("</a>") + br + br;

    // Append 'close' link:
    html += _T("<a href=\"") + commandTag + _T("close")
               + _T("\">") +  _T("close") + _T("</a>"),

    html += _T("</body></html>");

    return html;
}


wxString HtmlDoc::OnDocumentationLink(wxHtmlLinkEvent &event, bool &dismissPopup, bool &isGoto, long int &tokenIdx)
{
    using namespace HTMLTags;

    const wxString& href = event.GetLinkInfo().GetHref();
    wxString args;
    wxString tidx_str;

    dismissPopup = false;
    isGoto = false;

    if (!href.StartsWith(commandTag, &args))
        return wxEmptyString;

    if (args.StartsWith(_T("goto"), &tidx_str))
    {
        if(tidx_str.ToLong(&tokenIdx))
        {
            dismissPopup = true;
            isGoto = true;
        }
    }
    else if (args.StartsWith(_T("close")))
        dismissPopup = true;

    return wxEmptyString;
}

wxString HtmlDoc::GetDocForTooltip(TokenFlat* token)
{
    return HtmlDoc::GetDocShort(token->m_DocString);
}

wxString HtmlDoc::GetDocShort(const wxString& tokDoc)
{
    wxString doc;
    if (!tokDoc.IsEmpty())
    {
        const wxString brsep = _T("@brief_end@");
        size_t brf = tokDoc.find(brsep);
        size_t bre_idx = 11;
        if (brf != wxString::npos)
            bre_idx += brf;
        else
            bre_idx = 0;

        if (bre_idx > 11)
        {
            doc = tokDoc.substr(0,brf);
        }
        else if (bre_idx < tokDoc.size())
        {
            doc = tokDoc.substr(bre_idx);
            if (doc.size() > 120) // limit length of doc
                doc = doc.substr(0,120) + _T("...");
        }
    }
    return doc;
}


